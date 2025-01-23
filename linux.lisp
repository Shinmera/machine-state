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

(defmacro do-proc (file fgetsspec vars finally &body body)
  `(cffi:with-foreign-objects ((io :char 2048)
                               ,@vars)
     (let ((file (cffi:foreign-funcall "fopen" :string ,file :string "rb" :pointer)))
       (when (cffi:null-pointer-p file)
         (fail (cffi:foreign-funcall "strerror" :int64 errno)))
       (prog1 (loop while (/= 0 (cffi:foreign-funcall "fgets" :pointer io :size 2048 :pointer file :int))
                    do (when (= ,(length vars) (cffi:foreign-funcall "sscanf" :pointer io :string ,fgetsspec
                                                                     ,@(loop for (name) in vars collect :pointer collect name)
                                                                     :int))
                         ,@body)
                    finally (progn ,finally))
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
         (with-proc ("/proc/stat" (user "cpu  ") (nice " ") (system " ") (idle " "))
           (values (conv idle) (conv (+ user nice system idle)))))
        (integer
         (with-proc ("/proc/stat" (user (format NIL "cpu~d " core)) (nice " ") (system " ") (idle " "))
           (values (conv idle) (conv (+ user nice system idle)))))))))

(cffi:defcstruct (stat :size 144 :conc-name stat-)
  (dev     :uint64 :offset  0))

(define-implementation storage-device (path)
  (cffi:with-foreign-objects ((stat '(:struct stat)))
    (when (< (cffi:foreign-funcall "stat" :string (pathname-utils:native-namestring path) :pointer stat :int) 0)
      (fail (cffi:foreign-funcall "strerror" :int64 errno)))
    (let ((dev (stat-dev stat)))
      (do-proc "/proc/self/mountinfo" "%*d %*d %d:%d / %*s %*s %*s - %*s /dev/%s"
          ((l :int) (r :int) (name :char 32))
          (fail "Device not found in mountinfo table")
        (when (and (= (cffi:mem-ref l :int) (ldb (byte 32 8) dev))
                   (= (cffi:mem-ref r :int) (1- (ldb (byte 8 0) dev))))
          (return (cffi:foreign-string-to-lisp name :max-chars 32)))))))

(define-implementation storage-io-bytes (device)
  (when (pathnamep device)
    (setf device (storage-device device)))
  (cffi:with-foreign-objects ((lasttop :char 32))
    (setf (cffi:mem-aref lasttop :char 0) 1)
    (let ((count 0))
      (etypecase device
        ((eql T)
         (do-proc "/proc/diskstats" "%*d %*d %31s %*u %*u %llu %*u %*u %*u %llu"
             ((name :char 32)
              (reads :unsigned-long-long)
              (writes :unsigned-long-long))
           (when (/= 0 (cffi:foreign-funcall "strncmp" :pointer lasttop :pointer name :size (cffi:foreign-funcall "strlen" :pointer lasttop :size) :int))
             (cffi:foreign-funcall "strncpy" :pointer lasttop :pointer name :size 32)
             (incf count (+ (cffi:mem-ref reads :unsigned-long-long)
                            (cffi:mem-ref writes :unsigned-long-long))))))
        (string
         (do-proc "/proc/diskstats" "%*d %*d %31s %*u %*u %llu %*u %*u %*u %llu"
             ((name :char 32)
              (reads :unsigned-long-long)
              (writes :unsigned-long-long))
           (when (= 0 (cffi:foreign-funcall "strncmp" :pointer name :string device :size 32 :int))
             (incf count (+ (cffi:mem-ref reads :unsigned-long-long)
                            (cffi:mem-ref writes :unsigned-long-long)))
             (return)))))
      ;; Sector size is 512 bytes.
      (* 512 count))))

(define-implementation network-io-bytes (device)
  (etypecase device
    ((eql T)
     (let ((count 0))
       (do-proc "/proc/net/dev" "%31s %llu %*u %*u %*u %*u %*u %*u %*u %llu %*u"
           ((name :char 32)
            (reads :unsigned-long-long)
            (writes :unsigned-long-long))
           (return count)
         (unless (= 0 (cffi:foreign-funcall "strncmp" :pointer name :string "lo:" :size 32 :int))
           (incf count (+ (cffi:mem-ref reads :unsigned-long-long)
                          (cffi:mem-ref writes :unsigned-long-long)))))))
    (string
     (do-proc "/proc/net/dev" "%31s %llu %*u %*u %*u %*u %*u %*u %*u %llu %*u"
         ((name :char 32)
          (reads :unsigned-long-long)
          (writes :unsigned-long-long))
         (fail "No such device.")
       (when (= 0 (cffi:foreign-funcall "strncmp" :pointer name :string device :size (1- (length device)) :int))
         (return (+ (cffi:mem-ref reads :unsigned-long-long)
                    (cffi:mem-ref writes :unsigned-long-long))))))))
