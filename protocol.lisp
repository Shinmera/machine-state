(in-package #:org.shirakumo.machine-state)

(define-condition query-failed (error)
  ((function :initarg :function :initform NIL)
   (message :initarg :message :initform NIL))
  (:report (lambda (c s) (format s "The machine state query~@[ for ~a~] failed~@[:~%~%  ~a~]"
                                 (slot-value c 'function) (slot-value c 'message)))))

(defun fail (&optional message function)
  (error 'query-failed :function function :message message))

(defmacro define-protocol-fun (name args vals &body default)
  `(progn
     (declaim (ftype (function ,(mapcar #'second args) (values ,@vals &optional)) ,name))
     (declaim (inline ,name))
     (setf (fdefinition ',name)
           (lambda ,(mapcar #'first args)
             ,@default))))

(defmacro define-implementation (fun args &body body)
  `(defun ,fun ,args
     (flet ((fail (&optional message (function ',fun))
              (error 'query-failed :function function :message message)))
       (declare (ignorable #'fail))
       ,@body)))

(define-protocol-fun process-io-bytes () ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
  (values 0 0 0))

(define-protocol-fun process-room () ((unsigned-byte 64))
  0)

(define-protocol-fun process-time () (double-float)
  0d0)

(define-protocol-fun machine-room () ((unsigned-byte 64) (unsigned-byte 64))
  (values 0 0))

(define-protocol-fun machine-cores () ((unsigned-byte 16))
  1)

(define-protocol-fun machine-uptime () ((unsigned-byte 64))
  0)

(define-protocol-fun machine-time ((core T)) (double-float double-float)
  (values 0d0 0d0))

(define-protocol-fun thread-time ((thread T)) (double-float)
  0d0)

(define-protocol-fun thread-core-mask ((thread T)) ((unsigned-byte 64))
  (1- (ash 1 (machine-cores))))

(define-protocol-fun (setf thread-core-mask) ((mask (unsigned-byte 64)) (thread T)) ((unsigned-byte 64))
  (thread-core-mask thread))

(define-protocol-fun process-priority () ((member :idle :low :normal :high :realtime))
  :normal)

(define-protocol-fun thread-priority ((thread T)) ((member :idle :low :normal :high :realtime))
  :normal)

(define-protocol-fun (setf process-priority) ((priority (member :idle :low :normal :high :realtime))) ((member :idle :low :normal :high :realtime))
  :normal)

(define-protocol-fun (setf thread-priority) ((priority (member :idle :low :normal :high :realtime)) (thread T)) ((member :idle :low :normal :high :realtime))
  :normal)

(define-protocol-fun gc-room () ((unsigned-byte 64) (unsigned-byte 64))
  #+sbcl
  (values (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage))
          (sb-ext:dynamic-space-size))
  #+ccl (let ((free (ccl::%freebytes))
              (used (ccl::%usedbytes)))
          (values free
                  (+ free used)))
  #+ecl
  (values #+boehm-gc (si:gc-stats T)
          #-boehm-gc 0
          (ext:get-limit 'ext:heap-size))
  #+clasp
  (values (- (sys:dynamic-space-size) (sys:dynamic-usage))
          (sys:dynamic-space-size))
  #+abcl
  (let* ((runtime (java:jstatic "getRuntime"
                                (java:jclass "java.lang.Runtime")))
         ;; TODO: maxMemory? What does this method mean?
         (total-memory (java:jcall "totalMemory" runtime))
         (free-memory (java:jcall "freeMemory" runtime)))
    (values free-memory total-memory))
  #+clisp
  (multiple-value-bind (used room)
      (sys::%room)
    (values used (+ used room)))
  #-(or ccl sbcl ecl clasp abcl clisp)
  (values 0 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-symbol* (name package)
    (find-symbol (string name) (string package))))

(define-protocol-fun gc-time () (double-float)
  #+sbcl
  (/ (float #.(or (find-symbol* "*GC-REAL-TIME*" "SB-EXT")
                  (find-symbol* "*GC-RUN-TIME*" "SB-EXT")
                  0d0)
            0d0)
     INTERNAL-TIME-UNITS-PER-SECOND)
  #+ccl
  (/ (float (ccl:gctime) 0d0)
     INTERNAL-TIME-UNITS-PER-SECOND)
  #+(and ecl (not boehm-gc))
  (/ (float (si::gc-time) 0d0)
     INTERNAL-TIME-UNITS-PER-SECOND)
  #+clasp
  (/ (float (sys:gc-real-time) 0d0)
     INTERNAL-TIME-UNITS-PER-SECOND)
  #-(or ccl sbcl (and ecl (not boehm-gc)) clasp)
  0d0)

(define-protocol-fun gpu-room () ((unsigned-byte 64) (unsigned-byte 64))
  (values 0 0))

(define-protocol-fun gpu-time () (double-float)
  0d0)

(define-protocol-fun stack-room () ((unsigned-byte 64) (unsigned-byte 64))
  #+ccl
  (multiple-value-bind (stack stack-used)
      (ccl::%stack-space)
    (values (- stack stack-used) stack))
  #+ecl
  (values 0 (ext:get-limit 'ext:lisp-stack))
  #+sbcl
  (let* ((stack-total (- sb-vm::*control-stack-end* sb-vm::*control-stack-start*))
         (spaces (ignore-errors
                  (symbol-value
                   (or (find-symbol* :+all-spaces+ :sb-vm)
                       (find-symbol* :+stack-spaces+ :sb-vm))))))
    (values
     ;; FIXME: This is implemented the way it is because sometimes
     ;; either of +all-spaces+ or +stack-spaces+ is undefined due to
     ;; SBCL-internal magic -- aartaka
     (- stack-total
        (if spaces
            (funcall (third (find :control-stack spaces :key #'first)))
            stack-total))
     stack-total))
  #-(or ccl ecl sbcl)
  (values 0 0))

(define-protocol-fun static-room () ((unsigned-byte 64))
  #+ccl
  (multiple-value-bind (heap-used static-used staticlib-used frozen-space-size)
      (ccl::%usedbytes)
    (declare (ignorable heap-used))
    (+ static-used staticlib-used frozen-space-size))
  #+clisp
  (nth-value 2 (sys::%room))
  #+sbcl
  (let ((spaces (ignore-errors (symbol-value (find-symbol* :+all-spaces+ :sb-vm)))))
    (if spaces
        (funcall (third (find :static spaces :key #'first)))
        0))
  #+(or ccl clisp sbcl)
  0)

(define-protocol-fun storage-device ((path (or string pathname))) (string)
  (declare (ignore path))
  (fail "Not implemented."))

(define-protocol-fun storage-device-path ((device string)) (pathname)
  (declare (ignore device))
  (fail "Not implemented."))

(define-protocol-fun storage-room ((path (or string pathname))) ((unsigned-byte 64) (unsigned-byte 64))
  (declare (ignore path))
  (values 0 0))

(define-protocol-fun storage-io-bytes ((path T)) ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
  (declare (ignore path))
  (values 0 0 0))

(define-protocol-fun network-devices () (list)
  ())

(define-protocol-fun network-io-bytes ((device T)) ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
  (declare (ignore device))
  (values 0 0 0))

(defun os-type ()
  (or #+(or win32 windows) :WINDOWS
      #+(and linux (not android)) :LINUX
      #+(and (or darwin macos) (not ios) (not mach)) :DARWIN
      #+android :ANDROID
      #+ios :IOS
      #+(or netbsd net-bsd) :NETBSD
      #+(or freebsd free-bsd) :FREEBSD
      #+(or openbsd open-bsd) :OPENBSD
      #+beos :BEOS
      #+solaris :SOLARIS
      #+(or react reactos) :REACT
      #+(or plan9 p9) :PLAN9
      #+mezzano :MEZZANO
      #+nx :NX))

(define-protocol-fun machine-info () (string string symbol string)
  (values "Unknown" "Unknown" (os-type) "Unknown"))

(define-protocol-fun machine-battery () (double-float double-float symbol)
  (values 0 0 NIL))

(defun arch-type ()
  (or #+(and x86 (not (or x86-64 amd64))) :X86
      #+(or x86-64 amd64) :AMD64
      #+(and (or arm armv7 armv6 armv5) (not arm64)) :ARM
      #+arm64 :ARM64
      #+(and riscv (not riscv64)) :RISCV
      #+(or riscv64 rv64) :RISCV64
      #+(or ppc power powerpc) :PPC
      #+sparc :SPARC))

(define-protocol-fun machine-core-info () (string string symbol string)
  (values "Unknown" "Unknown" (arch-type) "Unknown"))

(define-protocol-fun process-info () (pathname pathname string string)
  (values *default-pathname-defaults* *default-pathname-defaults* "Unknown" "Unknown"))

(define-protocol-fun gpu-info () (symbol string string)
  (values NIL "Unknown" "Unknown"))

(define-protocol-fun network-info () ((or string null))
  (values NIL))

(define-protocol-fun network-address ((device string)) ((or string null) (or string null) (or string null))
  (declare (ignore device))
  (values NIL NIL NIL))
