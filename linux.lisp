(in-package #:org.shirakumo.machine-state)

(defmacro with-proc ((file &rest fields) &body body)
  `(cffi:with-foreign-object (io :char 2048)
     (let ((file (cffi:foreign-funcall "fopen" :string ,file :string "rb" :pointer)))
       (when (cffi:null-pointer-p file)
         (fail (cffi:foreign-funcall "strerror" :int64 errno)))
       (cffi:foreign-funcall "fread" :pointer io :size 1 :size 2048 :pointer file :size)
       (cffi:foreign-funcall "fclose" :pointer file :void))
     (let ,(loop for (var field) in fields
                 collect `(,var (let* ((field ,field)
                                       (start (cffi:foreign-funcall "strstr" :pointer io :string field :pointer))
                                       (ptr (cffi:inc-pointer start (length field))))
                                  (cffi:foreign-funcall "atol" :pointer ptr :long))))
       ,@body)))

(defmacro do-proc (vars (file fgetsspec &optional return) &body body)
  `(cffi:with-foreign-objects ((io :char 2048)
                               ,@vars)
     (let ((file (cffi:foreign-funcall "fopen" :string ,file :string "rb" :pointer)))
       (when (cffi:null-pointer-p file)
         (fail (cffi:foreign-funcall "strerror" :int64 errno)))
       (unwind-protect 
            (loop while (/= 0 (cffi:foreign-funcall "fgets" :pointer io :size 2048 :pointer file :int))
                  do (when (= ,(length vars) (cffi:foreign-funcall "sscanf" :pointer io :string ,fgetsspec
                                                                   ,@(loop for (name) in vars collect :pointer collect name)
                                                                   :int))
                       (let ,(loop for (var type count) in vars
                                   when (null count)
                                   collect `(,var (cffi:mem-ref ,var ',type)))
                         ,@body))
                  finally (progn (return ,return)))
         (cffi:foreign-funcall "fclose" :pointer file :void)))))

(define-implementation process-io-bytes ()
  (with-proc ("/proc/self/io" (read "rchar: ") (write "wchar: "))
    (values (+ read write) read write)))

;;;; For whatever reason on Linux rusage is useless for this, so redefine it here.
(define-implementation process-room ()
  (with-proc ("/proc/self/smaps_rollup" (rss "Rss: "))
    (* 1024 rss)))

(define-implementation machine-time (core)
  (let ((scale (/ (float (posix-call "sysconf" :int 2 :long) 0d0))))
    (flet ((conv (x) (* x scale)))
      (etypecase core
        ((eql T)
         (do-proc ((user :int) (nice :int) (system :int) (idle :int) (iowait :int) (irq :int) (softirq :int))
             ("/proc/stat" "cpu %d %d %d %d %d %d %d")
           (return (values (conv idle) (conv (+ user nice system idle iowait irq softirq))))))
        (integer
         (do-proc ((c :int) (user :int) (nice :int) (system :int) (idle :int) (iowait :int) (irq :int) (softirq :int))
             ("/proc/stat" "cpu%d %d %d %d %d %d %d %d" (fail "No such core."))
           (when (= c core)
             (return (values (conv idle) (conv (+ user nice system idle iowait irq softirq)))))))))))

(cffi:defcstruct (stat :size 144 :conc-name stat-)
  (dev     :uint64 :offset 0)
  (mode    :uint32 :offset 24))

(defun readlink (path)
  (cffi:with-foreign-objects ((buf :char 512))
    (let ((size (cffi:foreign-funcall "readlink" :string (pathname-utils:native-namestring path) :pointer buf :int 512 :int)))
      (when (= size -1)
        (fail (cffi:foreign-funcall "strerror" :int64 errno)))
      (cffi:foreign-string-to-lisp buf :max-chars size))))

(defconstant +s-ifmt+ #o170000)
(defconstant +s-iflnk+ #o120000)
(defun s-istype (mode mask) (= (logand mode +s-ifmt+) mask))
(defun s-islnk (mode) (s-istype mode +s-iflnk+))

(defun symlinkp (path)
  (cffi:with-foreign-objects ((stat '(:struct stat)))
    (when (< (cffi:foreign-funcall "lstat" :string (pathname-utils:native-namestring path) :pointer stat :int) 0)
      (fail (cffi:foreign-funcall "strerror" :int64 errno)))
    (s-islnk (stat-mode stat))))

(defun pathname-force-file (path)
  (cond
    ((pathname-utils:root-p path) path)
    ((pathname-utils:file-p path) path)
    (t (let ((directories (pathname-directory path)))
         (make-pathname :defaults path
                        :directory (butlast directories)
                        :name (car (last directories)))))))

(defun find-mount-root (path)
  (labels ((dev-id (path)
             (cffi:with-foreign-objects ((stat '(:struct stat)))
               (when (< (cffi:foreign-funcall "stat" :string (pathname-utils:native-namestring path) :pointer stat :int) 0)
                 (fail (cffi:foreign-funcall "strerror" :int64 errno)))
               (stat-dev stat)))
           (rec (path &optional (id (dev-id path)))
             (if (pathname-utils:root-p path)
                 path
                 (let* ((parent (pathname-utils:parent path))
                        (parent-id (dev-id parent)))
                   (if (= parent-id id)
                       (rec parent parent-id)
                       path)))))
    (let* ((root (rec path))
           (root-file (pathname-force-file root)))
      (if (symlinkp root-file)
          (find-mount-root (readlink root-file))
          root))))

(define-implementation storage-device (path)
  (let* ((mount-root (pathname-utils:native-namestring (pathname-force-file (find-mount-root path)))))
    (do-proc ((mountpoint :char 512) (name :char 32))
        ("/proc/self/mountinfo" "%*d %*d %*d:%*d / %s %*[^-]- %*s %s"
                                (fail "Device not found in mountinfo table"))
      (when (= 0 (cffi:foreign-funcall "strncmp" :pointer mountpoint :string mount-root :size 32 :int))
        (return (pathname-name
                 (pathname-utils:parse-native-namestring
                  (cffi:foreign-string-to-lisp name :max-chars 32)
                  :as :file)))))))

(define-implementation storage-device-path (device)
  (do-proc ((mount :char 512) (name :char 32))
      ("/proc/self/mountinfo" "%*d %*d %*d:%*d / %s %*[^-]- %*s /dev/%s"
                              (fail "Device not found in mountinfo table"))
    (when (= 0 (cffi:foreign-funcall "strncmp" :pointer name :string device :size 32 :int))
      (return (pathname-utils:parse-native-namestring
               (cffi:foreign-string-to-lisp mount :max-chars 32)
               :as :directory)))))

(cffi:defcstruct (statvfs :size 112 :conc-name statvfs-)
  (bsize    :uint64 :offset  0)
  (frsize   :uint64 :offset  8)
  (blocks   :uint64 :offset 16)
  (bfree    :uint64 :offset 24)
  (bavail   :uint64 :offset 32)
  (files    :uint64 :offset 40)
  (ffree    :uint64 :offset 48)
  (favail   :uint64 :offset 56)
  (fsid     :uint64 :offset 64)
  (flag     :uint64 :offset 72)
  (namemax  :uint64 :offset 80))

(define-implementation storage-room (path)
  (when (stringp path)
    (setf path (storage-device-path path)))
  (cffi:with-foreign-objects ((statvfs '(:struct statvfs)))
    (posix-call "statvfs" :string (pathname-utils:native-namestring path) :pointer statvfs :int)
    (values (* (statvfs-bavail statvfs)
               (statvfs-bsize statvfs))
            (* (statvfs-blocks statvfs)
               (statvfs-bsize statvfs)))))

(define-implementation storage-io-bytes (device)
  (when (pathnamep device)
    (setf device (storage-device device)))
  (cffi:with-foreign-objects ((lasttop :char 32))
    (setf (cffi:mem-aref lasttop :char 0) 1)
    (etypecase device
      ((eql T)
       (let ((read 0) (write 0))
         (do-proc ((name :char 32)
              (reads :unsigned-long-long)
              (writes :unsigned-long-long))
             ("/proc/diskstats" "%*d %*d %31s %*u %*u %llu %*u %*u %*u %llu"
                                (values (* 512 (+ read write))
                                        (* 512 read)
                                        (* 512 write)))
           (when (/= 0 (cffi:foreign-funcall "strncmp" :pointer lasttop :pointer name :size (cffi:foreign-funcall "strlen" :pointer lasttop :size) :int))
             (cffi:foreign-funcall "strncpy" :pointer lasttop :pointer name :size 32)
             (incf read reads)
             (incf write writes)))))
      (string
       (do-proc ((name :char 32)
                 (reads :unsigned-long-long)
                 (writes :unsigned-long-long))
           ("/proc/diskstats" "%*d %*d %31s %*u %*u %llu %*u %*u %*u %llu" (fail "No such device."))
         (when (= 0 (cffi:foreign-funcall "strncmp" :pointer name :string device :size 32 :int))
           (return (values (* 512 (+ reads writes))
                           (* 512 reads)
                           (* 512 writes)))))))))

(define-implementation network-devices ()
  (let ((list ()))
    (do-proc ((name :char 32))
        ("/proc/net/dev" "%31s" (cddr (nreverse list)))
      (push (cffi:foreign-string-to-lisp name :count (1- (cffi:foreign-funcall "strlen" :pointer name :size))) list))))

(define-implementation network-io-bytes (device)
  (etypecase device
    ((eql T)
     (let ((read 0) (write 0))
       (do-proc ((name :char 32)
                 (reads :unsigned-long-long)
                 (writes :unsigned-long-long))
           ("/proc/net/dev" "%31s %llu %*u %*u %*u %*u %*u %*u %*u %llu %*u"
                            (values (+ read write) read write))
         (unless (= 0 (cffi:foreign-funcall "strncmp" :pointer name :string "lo:" :size 32 :int))
           (incf read reads)
           (incf write writes)))))
    (string
     (do-proc ((name :char 32)
               (reads :unsigned-long-long)
               (writes :unsigned-long-long))
         ("/proc/net/dev" "%31s %llu %*u %*u %*u %*u %*u %*u %*u %llu %*u"
                          (fail "No such device."))
       (when (= 0 (cffi:foreign-funcall "strncmp" :pointer name :string device :size (1- (length device)) :int))
         (return (values (+ reads writes)
                         reads
                         writes)))))))
