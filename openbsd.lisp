(in-package #:org.shirakumo.machine-state)

;;;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L103
(defconstant +ctl-kern+ 1)
(defconstant +kern-osrelease+ 2)
(defconstant +kern-hostname+ 10)
(defconstant +kern-clockrate+ 12)
(defconstant +kern-boottime+ 21)
(defconstant +kern-cptime+ 40)
(defconstant +kern-cptime2+ 71)
(defconstant +kern-cpustats+ 85)

;;;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L919
(defconstant +ctl-hw+ 6)
(defconstant +hw-machine+ 1)
(defconstant +hw-model+ 2)
(defconstant +hw-diskstats+ 9)
(defconstant +hw-diskcount+ 10)
(defconstant +hw-sensors+ 11)
(defconstant +hw-vendor+ 14)
(defconstant +hw-product+ 15)
(defconstant +hw-physmem64+ 19)
(defconstant +hw-ncpuonline+ 25)

;; https://github.com/openbsd/src/blob/master/sys/uvm/uvmexp.h#L7
(defconstant +ctl-vm+ 2)
(defconstant +vm-uvmexp+ 4)

(defun sizeof (type)
  (cffi:foreign-type-size type))

;;;; errno is a thread local in openbsd, simple (defcvar errno) won't work
;;;; https://github.com/openbsd/src/blob/master/lib/libc/gen/errno.c#L57
;;;; https://github.com/openbsd/src/blob/master/include/errno.h#L54
(cffi:defcfun (errno-location "__errno") (:pointer :int))
(defun errno () (cffi:mem-ref (errno-location) :int))

(defun strerror (&optional (errno (errno)))
  (cffi:foreign-funcall "strerror" :int errno :string))

(defmacro posix-call (function &rest args)
  `(let ((val (cffi:foreign-funcall ,function ,@args)))
     (if (< val 0)
         (fail (strerror))
         val)))

(defun getpid ()
  (cffi:foreign-funcall "getpid" :long))

;;;; https://github.com/openbsd/src/tree/master/lib/libkvm
;;;; https://github.com/openbsd/src/blob/master/include/kvm.h
(cffi:define-foreign-library kvm (:openbsd "libkvm.so"))
(cffi:use-foreign-library kvm)

(defconstant +kvm-no-files+ (- (ash 1 31)))
(defconstant +posix2-line-max+ 2048)

;;;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L363
(defconstant +ki-maxcomlen+ 24) ;; ACtually _MAXCOMLEN

;;;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L370
(cffi:defcstruct (kinfo-proc :size 644 :conc-name kinfo-proc-)
  (user-id :uint32 :offset 128) ;; u_int32_t p_uid
  (group-id :uint32 :offset 136) ;; u_int32_t p_gid
  (nice :uint8 :offset 307) ;; u_int8_t p_nice
  (command-name (:array :char #.+ki-maxcomlen+) :offset 312) ;; char p_comm[KI_MAXCOMLEN]
  (resident-set-size :int32 :offset 384) ;; int32_t p_vm_rssize
  ;; (real-time-seconds :uint32 :offset 220) ;; u_int32_t p_rtime_sec
  ;; (real-time-microseconds :uint32 :offset 224) ;; u_int32_t p_rtime_usec
  (user-time-seconds :uint32 :offset 420) ;; u_int32_t p_utime_sec
  (user-time-microseconds :uint32 :offset 424)) ;; u_int32_t p_utime_sec

;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L752
(cffi:defcstruct (kinfo-file :size 624 :conc-name kinfo-file-)
  (read-bytes :uint64 :offset 96)
  (written-bytes :uint64 :offset 104))

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

(defun kvm-fail (kvm &optional (function nil))
  (fail (cffi:foreign-funcall "kvm_geterr" :pointer kvm :string) function))

(defmacro with-kvm ((kvm) &body body)
  `(let ((,kvm (kvm-openfiles)))
     (unwind-protect
          (progn ,@body)
       (posix-call "kvm_close" :pointer ,kvm :int))))

(defconstant +kern-file-bypid+ 2)

;;;; I could not find an API that show TOTAL process IO, there doesn't seem to be any tools that can do this either,
;;;; best I found is IO status of currently opened files
(define-implementation process-io-bytes ()
  (values 0 0 0)
  ;; (cffi:with-foreign-objects ((count :int))
  ;;   (with-kvm (kvm)
  ;;     (let ((files (cffi:mem-aptr
  ;;                   (cffi:foreign-funcall
  ;;                    "kvm_getfiles"
  ;;                    :pointer kvm
  ;;                    :int +kern-file-bypid+
  ;;                    :int (getpid)
  ;;                    :size (sizeof '(:struct kinfo-file))
  ;;                    :pointer count
  ;;                    :pointer)
  ;;                   '(:struct kinfo-file))))

  ;;       (when (cffi:null-pointer-p files)
  ;;         (kvm-fail kvm "kvm_getfiles"))

  ;;       (loop
  ;;         for i below (cffi:mem-ref count :int)
  ;;         for file = (cffi:mem-aptr files '(:struct kinfo-file) i)
  ;;         summing (kinfo-file-read-bytes file) into read
  ;;         summing (kinfo-file-written-bytes file) into written
  ;;         finally (return (values (+ read written)
  ;;                                 read
  ;;                                 written))))))
  )

(defconstant +kern-proc-pid+ 1)

(defmacro with-current-proc ((proc) &body body)
  (let ((count (gensym)) (kvm (gensym)))
    `(cffi:with-foreign-object (,count :int)
       (with-kvm (,kvm)
         (let ((,proc (cffi:mem-aptr
                       (cffi:foreign-funcall
                        "kvm_getprocs"
                        :pointer ,kvm
                        :int +kern-proc-pid+
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

;;;; User time only, will differ from output of PS or TOP
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
    (posix-call "setpriority" :int +prio-process+ :uint32 (getpid) :int prio :int))
  (process-priority)) ;; Get the actual priority

(defun %%sysctl (name namelen old oldlen new newlen)
  (cffi:foreign-funcall "sysctl" (:pointer :int) name :uint namelen :pointer old (:pointer :size) oldlen :pointer new :size newlen :int))

(defun %sysctl (names out &optional out-size)
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

      (cffi:with-foreign-objects ((oldlen :size))
        (setf (cffi:mem-ref oldlen :size) out-size)
        (%%sysctl %names name-count out oldlen (cffi:null-pointer) 0)))))

(defun sysctl-unchecked (names out &optional out-size)
  (%sysctl names out out-size))

(defun sysctl (names out &optional out-size)
  (let ((ret (%sysctl names out out-size)))
    (if (= -1 ret)
        (fail (strerror) "sysctl")
        ret)))

(defun sysctl-string (names size)
  (cffi:with-foreign-objects ((str :char size))
    (sysctl names str size)
    (cffi:foreign-string-to-lisp str :max-chars size)))

(cffi:defcstruct (uvmexp :size 344 :conc-name uvmexp-)
  (pagesize :int :offset 0)
  (npages :int :offset 12)
  (free :int :offset 16)
  (inactive :int :offset 24))

(define-implementation machine-room ()
  (cffi:with-foreign-object (uvm '(:struct uvmexp))
    (sysctl (list +ctl-vm+ +vm-uvmexp+) uvm (sizeof '(:struct uvmexp)))
    (flet ((pages->bytes (n)
             (* n (uvmexp-pagesize uvm))))
      (let* ((total-pages (uvmexp-npages uvm))
             (free-pages (+ (uvmexp-free uvm) (uvmexp-inactive uvm)))
             (total-bytes (pages->bytes total-pages))
             (free-bytes (pages->bytes free-pages)))
        (values (- total-bytes free-bytes)
                total-bytes)))))

(cffi:defcstruct (timeval :conc-name timeval-)
  (sec :uint64)
  (usec :uint64))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))
(defun get-unix-time ()
  (- (get-universal-time) +unix-epoch+))

(define-implementation machine-uptime ()
  (cffi:with-foreign-object (tv '(:struct timeval))
    (sysctl (list +ctl-kern+ +kern-boottime+) tv (sizeof '(:struct timeval)))
    (- (get-unix-time) (timeval-sec tv))))

(define-implementation machine-cores ()
  (cffi:with-foreign-object (online-cores :int)
    (sysctl (list +ctl-hw+ +hw-ncpuonline+) online-cores (sizeof :int))
    (cffi:mem-ref online-cores :int)))

(cffi:defcstruct (clockinfo :size 16 :conc-name clockinfo-)
  (hz :int :offset 0)) ;; int hz

(defconstant +cpustates+ 6)

(cffi:defcstruct (cpustats :size 56 :conc-name cpustats-)
  (times (:array :uint64 #.+cpustates+) :offset 0))

(defun core-time (core)
  (cffi:with-foreign-object (cpustats '(:struct cpustats))
    (sysctl (list +ctl-kern+ +kern-cpustats+ core) cpustats (sizeof '(:struct cpustats)))
    (cpustats-times cpustats)))

(defun cpu-time ()
  (cffi:with-foreign-object (cpustates :long +cpustates+)
    (sysctl (list +ctl-kern+ +kern-cptime+) cpustates (* +cpustates+ (sizeof :long)))
    (cffi:mem-ref cpustates `(:array :long ,+cpustates+))))

;;;; KERN_CPTIME2 returns wrong values for some reason, KERN_CPUSTATS works better
(define-implementation machine-time (core)
  (cffi:with-foreign-objects ((clockinfo '(:struct clockinfo)))
    (sysctl (list +ctl-kern+ +kern-clockrate+) clockinfo (sizeof '(:struct clockinfo)))
    (flet ((conv (x) (/ x (float (clockinfo-hz clockinfo) 0.0d0))))
      (let ((values (if (eq 't core)
                        (cpu-time)
                        (core-time core))))
        (destructuring-bind (user nice sys spin intr idle) (coerce values 'list)
          (values (conv idle)
                  (conv (+ user nice sys spin intr idle))))))))

;;; Copied from linux.lisp

(cffi:defcstruct (stat :size 108 :conc-name stat-)
  (dev     :uint64 :offset 0)
  (mode    :uint32 :offset 24))

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

(defconstant +mnt-wait+ 1)
(defconstant +mnt-nowait+ 2)
(defconstant +mfsnamelen+ 16)
(defconstant +mnamelen+ 90)

(cffi:defcstruct (statfs :size 564 :conc-name statfs-)
  (block-size :uint32 :offset 4)
  (blocks :uint64 :offset 12)
  (free-blocks :uint64 :offset 20) ;; All free blocks
  (available-blocks :int64 :offset 28) ;; Blocks available to non-superuser
  (synchronous-writes :uint64 :offset 60)
  (synchronous-reads :uint64 :offset 68)
  (asynchronous-writes :uint64 :offset 76)
  (asynchronous-reads :uint64 :offset 84)
  (max-filename-size :uint32 :offset 100)
  (filesystem-type (:array :char #.+mfsnamelen+) :offset 116)
  (mountpoint (:array :char #.+mnamelen+) :offset 132)
  (device (:array :char #.+mnamelen+) :offset 222))

(defun %getfsstat (buf &optional (count 0) (wait? t))
  (let* ((flags (if wait? +mnt-wait+ +mnt-nowait+))
         (bufsize (* count (sizeof '(:struct statfs)))))
    (posix-call "getfsstat" :pointer (or buf (cffi:null-pointer)) :size bufsize :int flags :int)))

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


;; No threads on x86 SBCL
#+(and openbsd sbcl x86)
(progn
  (define-implementation thread-time (thread) 0.0d0)
  (define-implementation thread-core-mask (thread) (1- (ash 1 (min 64 (machine-cores)))))
  (define-implementation thread-priority (thread) :normal))

#+(and openbsd sbcl (not x86))
(progn
  (define-implementation thread-time (thread) 0.0d0)
  (define-implementation thread-core-mask (thread) )
  (define-implementation thread-priority (thread) :normal))

;;;; https://github.com/openbsd/src/blob/master/include/ifaddrs.h#L31
(cffi:defcstruct (ifaddrs :conc-name ifaddrs-)
  (next (:pointer (:struct ifaddrs)))
  (name :string )
  (flags :uint)
  (address :pointer)
  (netmask :pointer)
  (broadcast :pointer)
  (p2p-destination :pointer)
  (data :pointer))

(cffi:defcstruct (sockaddr :conc-name sockaddr-)
  (length :uint8)
  (family :uint8)
  (data (:array :char 14)))

(cffi:defcstruct (sockaddr-dl :size 32 :conc-name sockaddr-dl-)
  (interface-name-length :unsigned-char :offset 5)
  (address-length :unsigned-char :offset 6)
  (data (:array :unsigned-char 24) :offset 8))

(defmacro do-ifaddrs ((ifaddr) &body body)
  (let ((ifap (gensym)))
    `(cffi:with-foreign-object (,ifap :pointer)
       (posix-call "getifaddrs" :pointer ,ifap :int)
       (let ((,ifap (cffi:mem-ref ,ifap :pointer)))
         (unwind-protect
              (do ((,ifaddr
                    (cffi:mem-ref ,ifap :pointer)
                    (ifaddrs-next ,ifaddr)))
                  ((cffi:null-pointer-p (ifaddrs-next ,ifaddr)) nil)
                ,@body)
           (cffi:foreign-funcall "freeifaddrs" :pointer ,ifap))))))

(define-implementation network-devices ()
  (let ((names nil))
    (do-ifaddrs (ifaddr)
      (pushnew (ifaddrs-name ifaddr) names :test #'string=))
    names))

(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 24)
(defconstant +af-link+ 18)

(defconstant +inet6-addrstrlen+ 46)
(defconstant +ni-numerichost+ 1)

(defconstant +eai-system+ -11)
(defun gai-strerror (ecode)
  (if (= +eai-system+ ecode)
      (strerror)
      (cffi:foreign-funcall "gai_strerror" :int ecode :string)))

(defun getnameinfo (sockaddr)
  (assert (member (sockaddr-family sockaddr) (list +af-inet+ +af-inet6+) :test #'=))
  (cffi:with-foreign-object (name :char +inet6-addrstrlen+)
    (let ((ret (cffi:foreign-funcall "getnameinfo"
                                     :pointer sockaddr
                                     :size (cffi:foreign-type-size '(:struct sockaddr))
                                     :pointer name
                                     :size +inet6-addrstrlen+
                                     :pointer (cffi:null-pointer)
                                     :size 0
                                     :int +ni-numerichost+
                                     :int)))
      (when (< ret 0)
        (fail (gai-strerror ret) "getnameinfo")))
    (cffi:foreign-string-to-lisp name :max-chars +inet6-addrstrlen+)))

(defun macaddr->string (macaddr &key (start 0) (end (+ start 6)))
  (format nil "~{~2,'0x~^:~}" (coerce (subseq macaddr start end) 'list)))

(define-implementation network-address (device)
  (let (ipv4 ipv6 mac)
    (do-ifaddrs (ifaddr)
      (when (string= device (ifaddrs-name ifaddr))
        (let* ((sockaddr (ifaddrs-address ifaddr))
               (address-family (sockaddr-family sockaddr)))
          (cond
            ((= +af-inet+ address-family) (unless ipv4 (setf ipv4 (getnameinfo sockaddr))))
            ((= +af-inet6+ address-family) (unless ipv6 (setf ipv6 (getnameinfo sockaddr))))
            ((= +af-link+ address-family)
             (unless mac
               (let* ((start (sockaddr-dl-interface-name-length sockaddr))
                      (end (+ start (sockaddr-dl-address-len sockaddr))))
                 (setf mac (macaddr->string (sockaddr-dl-data sockaddr) :start start :end end)))))))))
    (values ipv4 ipv6 mac)))

(define-implementation network-io-bytes (device) 0)

;;;; Reference:
;;;; https://github.com/openbsd/src/blob/master/sys/sys/sensors.h
;;;; https://github.com/openbsd/src/blob/master/sbin/sysctl/sysctl.c#L2554

(defconstant +sensor-type-volts-dc+ 2)
(defconstant +sensor-type-amphour+ 8)
(defconstant +sensor-type-integer+ 10)

(cffi:defcstruct (sensor :size 68 :conc-name sensor-)
  (value :int64 :offset 44))

(defconstant +sensor-name-size+ 16)
(cffi:defcstruct (sensordev :size 116 :conc-name sensordev-)
  (name (:array :char #.+sensor-name-size+) :offset 4)) ;;

(defconstant +enoent+ 2)
(defconstant +enxio+ 6)

(defun find-sensor-number (name &optional (dev 0))
  (cffi:with-foreign-object (sensordev '(:struct sensordev))
    (let ((ret (sysctl-unchecked (list +ctl-hw+ +hw-sensors+ dev) sensordev (sizeof '(:struct sensordev)))))
      (when (= -1 ret)
        (return-from find-sensor-number
          (if (= +enxio+ (errno))
              (find-sensor-number name (1+ dev))
              nil)))
      (let ((sensor-name (cffi:foreign-slot-pointer sensordev '(:struct sensordev) 'name)))
        (if (= 0 (cffi:foreign-funcall "strncmp" :pointer sensor-name :string name :size +sensor-name-size+ :int))
            dev
            (find-sensor-number name (1+ dev)))))))

(defun find-sensor-value (device sensor-type sensor-index)
  (cffi:with-foreign-object (sensor '(:struct sensor))
    (sysctl (list +ctl-hw+ +hw-sensors+ device sensor-type sensor-index)
            sensor
            (sizeof '(:struct sensor)))
    (sensor-value sensor)))

(define-implementation machine-battery ()
  (let ((battery-n (find-sensor-number "acpibat0")))
    (if battery-n
        (let ((last-full-capacity (find-sensor-value battery-n +sensor-type-amphour+ 0))
              (remaining-capacity (find-sensor-value battery-n +sensor-type-amphour+ 3))
              (state (find-sensor-value battery-n +sensor-type-integer+ 0)))
          (values remaining-capacity
                  last-full-capacity
                  (case state
                    (0 :full)
                    (1 :discharging)
                    (2 :charging))))
        (values 0 0 nil))))

(define-implementation machine-info ()
  (values
   (sysctl-string (list +ctl-hw+ +hw-vendor+) 128)
   (sysctl-string (list +ctl-hw+ +hw-product+) 128)
   :openbsd
   (sysctl-string (list +ctl-kern+ +kern-osrelease+) 16)))

(define-implementation machine-core-info ()
  (let ((processor (sysctl-string (list +ctl-hw+ +hw-model+) 128)))
    (values processor
            processor ;; There doesn't seem to be a separation between those
            nil
            (sysctl-string (list +ctl-hw+ +hw-machine+) 32))))

(defun getcwd ()
  (pathname-utils:parse-native-namestring
   (cffi:with-foreign-object (path :char 1024)
     (cffi:foreign-funcall "getcwd" :pointer path :size 1024)
     (cffi:foreign-string-to-lisp path :max-chars 1024))
   :as :directory))

(defun user-from-user-id (user-id)
  (cffi:foreign-funcall "user_from_uid" :uint32 user-id :int 1 :string))

(defun group-from-group-id (user-id)
  (cffi:foreign-funcall "group_from_gid" :uint32 user-id :int 1 :string))

(define-implementation process-info ()
  (with-current-proc (proc)
    (values
     ;; TODO: Try to get the full path? Maybe manually check the PATH variable?
     (let ((command (cffi:foreign-slot-pointer proc '(:struct kinfo-proc) 'command-name)))
       (pathname-utils:parse-native-namestring (cffi:foreign-string-to-lisp command)))
     (getcwd)
     (user-from-user-id (kinfo-proc-user-id proc))
     (group-from-group-id (kinfo-proc-group-id proc)))))

(define-implementation gpu-info ()
  (values nil "Unknown"))

(define-implementation network-info ()
  (sysctl-string (list +ctl-kern+ +kern-hostname+) 255))
