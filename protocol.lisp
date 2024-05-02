(in-package #:org.shirakumo.machine-state)

(define-condition query-failed (error)
  ((function :initarg :function :initform NIL)
   (message :initarg :message :initform NIL))
  (:report (lambda (c s) (format s "The machine state query~@[ for ~a~] failed~@[:~%~%  ~a~]"
                                 (slot-value c 'function) (slot-value c 'message)))))

(defmacro define-protocol-fun (name args vals &body default)
  `(progn
     (declaim (ftype (function ,(mapcar #'second args) (values ,@vals &optional)) ,name))
     (declaim (inline ,name))
     (setf (fdefinition ',name)
           (lambda ,(mapcar #'first args)
             ,@default))))

(defmacro define-implementation (fun args &body body)
  `(defun ,fun ,args
     (flet ((fail (&optional message)
              (error 'query-failed :function ',fun :message message)))
       (declare (ignorable #'fail))
       ,@body)))

(define-protocol-fun process-io-bytes () ((unsigned-byte 64))
  0)

(define-protocol-fun process-room () ((unsigned-byte 64))
  0)

(define-protocol-fun process-time () (double-float)
  0d0)

(define-protocol-fun machine-room () ((unsigned-byte 64) (unsigned-byte 64))
  (values 0 0))

(define-protocol-fun machine-cores () ((unsigned-byte 16))
  1)

(define-protocol-fun thread-time ((thread T)) (double-float)
  0d0)

(define-protocol-fun thread-core-mask ((thread T)) ((unsigned-byte 64))
  (1- (ash 1 (machine-cores))))

(define-protocol-fun (setf thread-core-mask) ((mask (unsigned-byte 64)) (thread T)) ((unsigned-byte 64))
  (thread-core-mask thread))

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

(define-protocol-fun gc-time () (double-float)
  #+sbcl
  (/ (float #.(or (find-symbol "*GC-REAL-TIME*" "SB-EXT")
                  (find-symbol "*GC-RUN-TIME*" "SB-EXT")
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
    (values stack-used stack))
  #+ecl
  (values 0 (ext:get-limit 'ext:lisp-stack))
  #+sbcl
  (values
   ;; FIXME: This is implemented the way it is because sometimes
   ;; either of +all-spaces+ or +stack-spaces+ is undefined due to
   ;; SBCL-internal magic -- aartaka
   (funcall (third (find :control-stack
                         (ignore-errors
                          (symbol-value
                           (or (uiop:find-symbol* :+all-spaces+ :sb-vm nil)
                               (uiop:find-symbol* :+stack-spaces+ :sb-vm nil))))
                         :key #'first)))
   (- sb-vm::*control-stack-end* sb-vm::*control-stack-start*))
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
  (funcall (third (find :static
                        (ignore-errors
                         (symbol-value
                          (uiop:find-symbol* :+all-spaces+ :sb-vm nil)))
                        :key #'first)))
  #+(or ccl clisp sbcl)
  0)
