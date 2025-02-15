(in-package #:org.shirakumo.machine-state)

#-openbsd
(cffi:defcvar (errno "errno") :int64)

(defun errno ()
  #-openbsd errno
  ;; errno is a thread local in openbsd, simple (defcvar errno) won't work
  ;; https://github.com/openbsd/src/blob/master/lib/libc/gen/errno.c#L57
  ;; https://github.com/openbsd/src/blob/master/include/errno.h#L54
  #+openbsd (cffi:mem-ref (cffi:foreign-funcall "__errno" (:pointer :int)) :int))

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

    (cffi:with-foreign-objects ((%mib :int mibn) (oldlen :size))
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

(defun count-fields (str separator)
  (let ((i 1))
    (loop
      for ch across str
      if (char= ch separator)
        do (incf i))
    i))

(defun sysctl-name-to-mib (name &optional (mibn (count-fields name #\.)))
  (cffi:with-foreign-objects ((mibp :int mibn)
                              (sizep :size))
    (setf (cffi:mem-ref sizep :size) mibn)
    (cffi:foreign-funcall "sysctlnametomib"
                          :string name
                          (:pointer :int) mibp
                          (:pointer :size) sizep)
    (loop for i below mibn
          collect (cffi:mem-aref mibp :int i))))

(defun sysctl-resolve-mib (mib-or-name)
  (etypecase mib-or-name
    #+freebsd
    (string mib-or-name)
    (list (mapcan (lambda (x)
                    (etypecase x
                      #+freebsd
                      (string (sysctl-name-to-mib x))
                      (number (list x))))
                  mib-or-name))))

(defmacro with-sysctl ((mib out type &optional (count 1)) &body body)
  "Utility for SYSCTL, MIB is evaluated into a list."
  (flet ((ensure-list (x) (if (listp x) x (list x))))
    (let ((%mib (gensym)) (%count (gensym)))
      `(let* ((,%mib (sysctl-resolve-mib (list ,@(ensure-list mib)))) (,%count ,count))
         (cffi:with-foreign-object (,out ,type ,%count)
           (sysctl ,%mib ,out (* ,%count (cffi:foreign-type-size ,type)))
           ,@body)))))

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
  (usec #+32-bit :uint32 #+64-bit :uint64))

(defun timeval->seconds (tv)
  (+ (print (timeval-sec tv))
     (/ (print (timeval-usec tv)) 1000000.0d0)))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))
(defun get-unix-time () (- (get-universal-time) +unix-epoch+))

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/sys/time.h#L480
;;;; https://github.com/openbsd/src/blob/master/sys/sys/time.h#L157
(cffi:defcstruct (clockinfo :size #+openbsd 16
                                  #+freebsd 20 ;; FreeBSD has a reserved field
                            :conc-name clockinfo-)
  (hz :int))

(defun getpid () (cffi:foreign-funcall "getpid" :long)) ;; pid_t
(defun page-size () (cffi:foreign-funcall "getpagesize" :int))

(defconstant +maxcomlen+
  #+openbsd 24 ;; Actually _MAXCOMLEN, https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L363
  #+freebsd 19) ;; https://github.com/freebsd/freebsd-src/blob/main/sys/sys/param.h#L125

(defun process-nice->priority (value)
  (cond ((< value -8) :realtime)
        ((< value  0) :high)
        ((= value  0) :normal)
        ((< value +8) :low)
        (T :idle)))

(defun priority->process-nice (priority)
  (ecase priority
    (:idle      19)
    (:low        5)
    (:normal     0)
    (:high      -5)
    (:realtime -20)))

(define-implementation (setf process-priority) (priority)
  (let ((prio (priority->process-nice priority)))
    (posix-call "setpriority" :int 0 :uint32 (getpid) :int prio :int))
  (process-priority)) ;; Get the actual priority

(defun split-path (path &optional (delimiter #\:))
  (let (paths start)
    (do ((i 0 (1+ i)))
        ((= i (length path)) (nreverse paths))
      (when (char= (schar path i) delimiter)
        (push (subseq path (or start 0) i) paths)
        (setf start (1+ i))))))

(defun resolve-executable (command)
  (let ((path (cffi:foreign-funcall "getenv" :string "PATH" :string)))
    (when path
      (dolist (dir (split-path path #\:))
        (let ((exec-path (make-pathname
                          :defaults (pathname-utils:parse-native-namestring dir :as :directory)
                          :name command)))
          (when (probe-file exec-path)
            (return-from resolve-executable exec-path)))))))

(defun uid->user (uid) (cffi:foreign-funcall "user_from_uid" :uint32 uid :int 1 :string))
(defun gid->group (gid) (cffi:foreign-funcall "group_from_gid" :uint32 gid :int 1 :string))
