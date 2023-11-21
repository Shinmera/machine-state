(in-package #:org.shirakumo.machine-state)

(defmacro define-protocol-fun (name args vals &body default)
  `(progn
     (declaim (ftype (function ,(mapcar #'second args) (values ,@vals &optional)) ,name))
     (declaim (inline ,name))
     (setf (fdefinition ',name)
           (lambda ,(mapcar #'first args)
             ,@default))))

(defmacro define-implementation (fun args &body body)
  `(defun ,fun ,args ,@body))

(define-protocol-fun process-io-bytes () ((unsigned-byte 64))
  0)

(define-protocol-fun process-room () (double-float)
  (values 0 0))

(define-protocol-fun process-time () (double-float)
  0d0)

(define-protocol-fun machine-room () ((unsigned-byte 64) (unsigned-byte 64))
  (values 0 0))

(define-protocol-fun thread-time ((thread T)) (double-float)
  0d0)

(define-protocol-fun gc-room () ((unsigned-byte 64) (unsigned-byte 64))
  #+sbcl
  (values (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage))
          (sb-ext:dynamic-space-size))
  #-sbcl (values 0 0))

(define-protocol-fun gc-time () (double-float)
  #+sbcl
  (/ (float sb-ext:*gc-real-time* 0d0)
     INTERNAL-TIME-UNITS-PER-SECOND)
  #-sbcl 0d0)

(define-protocol-fun gpu-room () ((unsigned-byte 64) (unsigned-byte 64))
  (values 0 0))

(define-protocol-fun gpu-time () (double-float)
  0d0)
