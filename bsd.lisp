(in-package #:org.shirakumo.machine-state)

#-openbsd
(cffi:defcvar (errno "errno") :int64)

(defun errno ()
  #-openbsd errno
  ;; errno is a thread local in openbsd, simple (defcvar errno) won't work
  ;; https://github.com/openbsd/src/blob/master/lib/libc/gen/errno.c#L57
  ;; https://github.com/openbsd/src/blob/master/include/errno.h#L54
  #+openbsd (cffi:mem-ref (cffi:foreign-funcall "__errno" (:pointer :int))))

(defun strerror (&optional (errno (errno)))
  (cffi:foreign-funcall "strerror" :int errno :string))

(defmacro posix-call (function &rest args)
  (let ((%val (gensym)))
    `(let ((,%val (cffi:foreign-funcall ,function ,@args)))
       (if (< ,%val 0)
           (fail (strerror) ,function)
           ,%val))))

(defgeneric sysctl (mib &optional out out-size)
  (:documentation
   "Call sysctl with MIB as the list of names, store the result in OUT, which must be of at least OUT-SIZE.
If OUT is NIL, call sysctl with MIB and return the number of bytes that would be written into OUT."))

#+freebsd
(defmethod sysctl ((mib string) &optional out out-size)
  (cffi:with-foreign-object (oldlen :size)
    (when out
      (setf (cffi:mem-ref oldlen :size) out-size))
    (posix-call "sysctlbyname"
                :string mib
                :pointer (or out (cffi:null-pointer))
                (:pointer :size) oldlen
                :pointer (cffi:null-pointer)
                :size 0
                :int)
    (or out (cffi:mem-ref oldlen :int))))

(defmethod sysctl ((mib list) &optional out out-size)
  (let ((mibn (length mib)))
    (assert (>= mibn 2) (mib) "Need at least a name and a second level name to call sysctl")

    (cffi:with-foreign-objects ((%mib :int mibn)
                                (oldlen :size))
      (loop
        for name in mib and i from 0
        do (setf (cffi:mem-aref %mib :int i) name))

      (when out
        (setf (cffi:mem-ref oldlen :size) out-size))

      (posix-call "sysctl"
                  (:pointer :int) %mib
                  :uint mibn
                  :pointer (or out (cffi:null-pointer))
                  (:pointer :size) oldlen
                  :pointer (cffi:null-pointer)
                  :size 0
                  :int)
      (or out (cffi:mem-ref oldlen :int)))))

(defun sysctl-unchecked (mib out &optional out-size)
  "Like SYSCTL but don't handle the ERRNO, useful for when ERRNO has special meanings."
  (sysctl mib out out-size))

(defmacro with-sysctl ((mib out type &optional (count 1)) &body body)
  "Utility for SYSCTL, MIB is evaluated into a list."
  (let ((%mib (gensym)) (%count (gensym)))
    `(let ((,%mib ',mib) (,%count ,count))
       (cffi:with-foreign-object (,out ,type ,%count)
         (sysctl ,%mib ,out (* ,%count (cffi:foreign-type-size ,type)))
         ,@body))))

(defmacro with-sysctls ((&rest sysctls) &body body)
  "Like with sysctl, but allows for multiple at once."
  (if sysctls
      `(with-sysctl (,@(car sysctls))
         (with-sysctls (,@(cdr sysctls)) ,@body))
      `(progn ,@body)))

(defmacro sysctl-string (names size)
  "Like SYSCTL but the return value is a string of SIZE characters."
  (let ((%str (gensym)))
    `(with-sysctl (,names ,%str :char ,size)
       (cffi:foreign-string-to-lisp ,%str :max-chars ,size))))

(cffi:defcstruct (timeval :conc-name timeval-)
  (sec :uint64)
  (usec :uint64))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))
(defun get-unix-time () (- (get-universal-time) +unix-epoch+))

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/sys/time.h#L480
;;;; https://github.com/openbsd/src/blob/master/sys/sys/time.h#L157
(cffi:defcstruct (clockinfo :size #+openbsd 16
                                  #+freebsd 20 ;; FreeBSD has a reserved field
                            :conc-name clockinfo-)
  (hz :int))
