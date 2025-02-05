(in-package #:org.shirakumo.machine-state)

(declaim (optimize (debug 3)))

(defmacro define-foreign-struct-accessors (&body types)
  `(progn
     ,@(loop
         for type in types
         for slots = (cffi:foreign-slot-names `(:struct ,type))
         append (loop
                  for slot in slots
                  collect `(defun ,(intern (format nil "~a-~a" type slot)) (,type)
                             (cffi:foreign-slot-value ,type '(:struct ,type) ',slot))))))

(define-foreign-struct-accessors
  kinfo-file kinfo-proc uvmexp timeval clockinfo stat statfs)

(defun sizeof (type)
  (cffi:foreign-type-size type))

(defun strerror ()
  (cffi:foreign-funcall "strerror" :int64 errno :string))

(cffi:defcfun "getpid" :int32)

(defmacro posix-call (function &rest args)
  `(let ((val (cffi:foreign-funcall ,function ,@args)))
     (if (< val 0)
         (fail (strerror))
         val)))

(cffi:define-foreign-library kvm (:openbsd "libkvm.so"))
(cffi:use-foreign-library kvm)

(defconstant +kvm-no-files+ (- (ash 1 31)))

(defun kvm-openfiles ()
  (cffi:with-foreign-object (errbuf :char +posix2-line-max+)
    (let ((ret (cffi:foreign-funcall
                "kvm_openfiles"
                :pointer (cffi:null-pointer)
                :pointer (cffi:null-pointer)
                :pointer (cffi:null-pointer)
                :int +kvm-no-files+
                :pointer errbuf
                :pointer #| kvm_t* |#)))
      (when (cffi:null-pointer-p ret)
        (fail (cffi:foreign-string-to-lisp errbuf :max-chars +posix2-line-max+)))
      ret)))

(defun kvm-close (kvm)
  (posix-call "kvm_close" :pointer kvm :int))

(defun kvm-fail (kvm &optional (function nil))
  (fail
   (cffi:foreign-funcall "kvm_geterr" :pointer kvm :string)
   function))

(defmacro with-kvm ((kvm) &body body)
  `(let ((,kvm (kvm-openfiles)))
     (unwind-protect
          (progn ,@body)
       (kvm-close ,kvm))))

;; TODO Maybe this is incorrect? only accounting for currently opened files?
(define-implementation process-io-bytes ()
  (cffi:with-foreign-objects ((count :int))
    (with-kvm (kvm)
      (let ((files (cffi:mem-aptr
                    (cffi:foreign-funcall
                     "kvm_getfiles"
                     :pointer kvm
                     :kern-file :kern-file-bypid
                     :int (getpid)
                     :size (sizeof '(:struct kinfo-file))
                     :pointer count
                     :pointer)
                    '(:struct kinfo-file))))

        (when (cffi:null-pointer-p files)
          (kvm-fail kvm "kvm_getfiles"))

        (loop
          for i below (cffi:mem-ref count :int)
          for file = (cffi:mem-aptr files '(:struct kinfo-file) i)
          summing (kinfo-file-read-bytes file) into read
          summing (kinfo-file-written-bytes file) into written
          finally (return (values (+ read written)
                                  read
                                  written)))))))

(defmacro with-current-proc ((proc) &body body)
  (let ((count (gensym)) (kvm (gensym)))
    `(cffi:with-foreign-object (,count :int)
       (with-kvm (,kvm)
         (let ((,proc (cffi:mem-aptr
                       (cffi:foreign-funcall
                        "kvm_getprocs"
                        :pointer ,kvm
                        :kern-proc :kern-proc-pid
                        :int (getpid)
                        :size (sizeof '(:struct kinfo-proc))
                        :pointer ,count
                        :pointer)
                       '(:struct kinfo-proc))))
           (when (cffi:null-pointer-p ,proc)
             (kvm-fail ,kvm "kvm_getprocs"))
           ,@body)))))

(define-implementation process-room ()
  (with-current-proc (proc)
    (let ((page-size (cffi:foreign-funcall "getpagesize" :int)))
      (* page-size (kinfo-proc-resident-set-size proc)))))

(define-implementation process-time ()
  (with-current-proc (proc)
    (+ (kinfo-proc-user-time-seconds proc)
       (/ (kinfo-proc-user-time-microseconds proc) 1000000.0d0))))

(defconstant +prio-process+ 0)
(define-implementation process-priority ()
  (with-current-proc (proc)
    (let ((value (- (kinfo-proc-nice proc) 20))) ;; Will return between 0 (-20) and 40 (20)
      (cond ((< value -8) :realtime)
            ((< value  0) :high)
            ((= value  0) :normal)
            ((< value +8) :low)
            (T :idle)))))

(define-implementation (setf process-priority) (priority)
  (let ((prio (ecase priority
                (:idle      19)
                (:low        5)
                (:normal     0)
                (:high      -5)
                (:realtime -20))))
    (posix-call "setpriority" :prio :prio-process :uint32 (getpid) :int prio :int))
  (process-priority))

(defun %sysctl (name namelen old oldlen new newlen)
  (posix-call "sysctl" (:pointer :int) name :uint namelen :pointer old (:pointer :size) oldlen :pointer new :size newlen :int))

(defun sysctl-len (%names name-count)
  (cffi:with-foreign-object (oldlen :size)
    (%sysctl %names name-count (cffi:null-pointer) oldlen (cffi:null-pointer) 0)
    (cffi:mem-ref oldlen :size)))

(defun sysctl (names out &optional out-size)
  (assert (>= (length names) 2) (names) "Need at least a name and a second level name to call sysctl")
  (let ((names (mapcar (lambda (name)
                         (if (keywordp name)
                             (cffi:foreign-enum-value :sysctl name)
                             name))
                       names))
        (name-count (length names)))
    (cffi:with-foreign-objects ((%names :int name-count))
      (loop
        for name in names and i from 0
        do (setf (cffi:mem-aref %names :int i) name))
      (let ((len (or out-size (sysctl-len %names name-count))))
        (cffi:with-foreign-objects ((oldlen :size))
          (setf (cffi:mem-ref oldlen :size) len)
          (%sysctl %names name-count out oldlen (cffi:null-pointer) 0)
          (cffi:mem-ref oldlen :size))))))

(define-implementation machine-room ()
  (cffi:with-foreign-object (uvm '(:struct uvmexp))
    (sysctl (list :ctl-vm :vm-uvmexp) uvm (sizeof '(:struct uvmexp)))
    (flet ((pages->bytes (n)
             (* n (uvmexp-pagesize uvm))))
      (let* ((total-pages (uvmexp-npages uvm))
             (free-pages (+ (uvmexp-free uvm) (uvmexp-inactive uvm)))
             (total-bytes (pages->bytes total-pages))
             (free-bytes (pages->bytes free-pages)))
        (values (- total-bytes free-bytes)
                total-bytes)))))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))
(defun get-unix-time ()
  (- (get-universal-time) +unix-epoch+))

(define-implementation machine-uptime ()
  (cffi:with-foreign-object (tv '(:struct timeval))
    (sysctl (list :ctl-kern :kern-boottime) tv (sizeof '(:struct timeval)))
    (- (get-unix-time) (timeval-sec tv))))

(define-implementation machine-cores ()
  (cffi:with-foreign-object (online-cores :int)
    (sysctl (list :ctl-hw :hw-ncpuonline) online-cores (sizeof :int))
    (cffi:mem-ref online-cores :int)))

;; Won't work for OpenBSD release <6.4 because the SPIN metric was introduced then
;; TODO: This is broken
(define-implementation machine-time (core)
  (cffi:with-foreign-objects ((cpustates :uint32 +cpustates+)
                              (clockinfo '(:struct clockinfo)))
    (sysctl (list :ctl-kern :kern-clockrate) clockinfo (sizeof '(:struct clockinfo)))
    (let ((scale (float (clockinfo-hz clockinfo) 0.0d0)))
      (flet ((conv (x) (/ x scale)))
        (sysctl (if (eq T core)
                    (list :ctl-kern :kern-cptime)
                    (list :ctl-kern :kern-cptime2 core))
                cpustates
                (* +cpustates+ (sizeof :uint32) 2))
        (destructuring-bind (user nice sys spin intr idle)
            (loop for i from 0 below +cpustates+ collect (cffi:mem-aref cpustates :uint32 i))
          (values (conv idle)
                  (conv (+ user nice sys spin intr idle))))))))

;;; Copied from linux.lisp

(defun pathname-force-file (path)
  (cond
    ((pathname-utils:root-p path) path)
    ((pathname-utils:file-p path) path)
    (T (let ((directories (pathname-directory path)))
         (make-pathname :defaults path
                        :directory (butlast directories)
                        :name (car (last directories)))))))

(defun find-mount-root (path)
  (labels ((dev-id (path)
             (cffi:with-foreign-objects ((stat '(:struct stat)))
               (posix-call "stat" :string (pathname-utils:native-namestring path) :pointer stat :int)
               (stat-dev stat)))
           (rec (path &optional (id (dev-id path)))
             (if (pathname-utils:root-p path)
                 path
                 (let* ((parent (pathname-utils:parent path))
                        (parent-id (dev-id parent)))
                   (if (= parent-id id)
                       (rec parent parent-id)
                       path)))))
    (pathname-force-file (rec (truename path)))))

;;;

(defun %getfsstat (buf &optional (count 0) (wait? t))
  (let* ((flags (if wait? :wait :nowait))
         (bufsize (* count (sizeof '(:struct statfs)))))
    (posix-call "getfsstat" :pointer (or buf (cffi:null-pointer)) :size bufsize :mnt flags :int)))

(defun mount-count ()
  (%getfsstat nil))

(defmacro do-filesystems ((fs) &body body)
  (let ((statfs (gensym)) (count (gensym)) (i (gensym)))
    `(let ((,count (mount-count)))
       (cffi:with-foreign-object (,statfs '(:struct statfs) ,count)
         (%getfsstat ,statfs ,count)
         (or (dotimes (,i ,count)
               (let ((,fs (cffi:mem-aptr ,statfs '(:struct statfs) ,i)))
                 ,@body))
             (fail "Filesystem not found"))))))

(define-implementation storage-device (path)
  (let ((mount-root (pathname-utils:native-namestring (pathname-force-file (find-mount-root path)))))
    (do-filesystems (fs)
      (let ((fs-mountpoint (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint)))
        (when (= 0 (cffi:foreign-funcall "strncmp" :pointer fs-mountpoint :string mount-root :size +mnamelen+ :int))
          (return (pathname-name
                   (cffi:foreign-string-to-lisp
                    (cffi:foreign-slot-pointer fs '(:struct statfs) 'device) :max-chars +mnamelen+))))))))

(define-implementation storage-device-path (device)
  (let ((device-path (pathname-utils:native-namestring (make-pathname :defaults #P"/dev/" :name device))))
    (do-filesystems (fs)
      (let ((fs-device (cffi:foreign-slot-pointer fs '(:struct statfs) 'device)))
        (when (= 0 (cffi:foreign-funcall "strncmp" :pointer fs-device :string device-path :size +mnamelen+ :int))
          (return (pathname-utils:parse-native-namestring
                   (cffi:foreign-string-to-lisp
                    (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint) :max-chars +mnamelen+))))))))

;; TODO This only accepts a disk label (partition), not an actual disk
(define-implementation storage-io-bytes (path)
  (setf path (pathname-utils:native-namestring
              (pathname-force-file
               (etypecase path
                 (pathname (find-mount-root path))
                 (string (storage-device-path path))))))

  (do-filesystems (fs)
    (let ((fs-mountpoint (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint)))
      (when (= 0 (cffi:foreign-funcall "strncmp" :pointer fs-mountpoint :string path :size +mnamelen+ :int))
        (let ((reads (+ (statfs-synchronous-reads fs)
                        (statfs-asynchronous-reads fs)))
              (writes (+ (statfs-synchronous-writes fs)
                         (statfs-asynchronous-writes fs))))
          (return-from storage-io-bytes
            (values (+ reads writes) reads writes)))))))

;; TODO This only accepts a disk label (partition), not an actual disk
(define-implementation storage-room (path)
  (when (stringp path)
    (setf path (storage-device-path path)))

  (do-filesystems (fs)
    (let ((fs-mountpoint (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint)))
      (when (= 0 (cffi:foreign-funcall "strncmp" :pointer fs-mountpoint :string (pathname-utils:native-namestring path) :size +mnamelen+ :int))
        (flet ((block->bytes (n)
                 (* n (statfs-block-size fs))))
          (return-from storage-room
            (values (block->bytes (statfs-available-blocks fs))
                    (block->bytes (statfs-blocks fs)))))))))
