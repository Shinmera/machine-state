(in-package #:org.shirakumo.machine-state)

(defmacro with-proc ((file &rest fields) &body body)
  `(cffi:with-foreign-object (io :char 2048)
     (let ((file (cffi:foreign-funcall "fopen" :string ,file :string "rb" :pointer)))
       (when (cffi:null-pointer-p file)
         (fail (cffi:foreign-funcall "strerror" :int64 errno)))
       (cffi:foreign-funcall "fread" :pointer io :size 1 :size 2048 :pointer file :size)
       (cffi:foreign-funcall "fclose" :pointer file :void))
     (let ,(loop for (var field) in fields
                 collect `(,var (let* ((start (cffi:foreign-funcall "strstr" :pointer io :string ,field :pointer))
                                       (ptr (cffi:inc-pointer start ,(length field))))
                                  (cffi:foreign-funcall "atol" :pointer ptr :long))))
       ,@body)))

(define-implementation process-io-bytes ()
  (with-proc ("/proc/self/io" (read "rchar: ") (write "wchar: "))
    (+ read write)))

;;;; For whatever reason on Linux rusage is useless for this, so redefine it here.
(define-implementation process-room ()
  (with-proc ("/proc/self/smaps_rollup" (rss "Rss: "))
    (* 1024 rss)))
