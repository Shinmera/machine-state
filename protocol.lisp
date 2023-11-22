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
          (values used
                  (+ free used)))
  #+ecl
  (values (si:gc-stats T) (ext:get-limit 'ext:heap-size))
  #+clasp
  (values (- (sys:dynamic-space-size) (sys:dynamic-usage))
          (sys:dynamic-space-size))
  #-(or ccl sbcl ecl clasp)
  (values 0 0))

(define-protocol-fun gc-time () (double-float)
  #+sbcl
  (/ (float sb-ext:*gc-real-time* 0d0)
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
