(in-package :machine-state)

(define-implementation machine-cores ()
  (mezzano.supervisor:logical-core-count))

(define-implementation machine-room ()
  (multiple-value-bind (free total)
      (mezzano.supervisor:physical-memory-statistics)
    (values (* total 4096) (* free 4096))))

;;this is technically incorrect, should I do what mezzano's ROOM does?
;;ROOM is very slow...
(define-implementation gc-room () (machine-room))
(define-implementation process-room () (machine-room))

(define-implementation gc-time ()
  mezzano.internals::*gc-time*)

(define-implementation thread-time (thread)
  (let ((the-thread (if (eql thread t) (bt:current-thread) thread)))
    (float
     (/ (mezzano.supervisor:thread-run-time the-thread) (* 1000 1000))
     0d0)))
