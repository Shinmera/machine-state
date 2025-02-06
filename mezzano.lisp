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

(define-implementation machine-info ()
  (values "Unknown"
          "Unknown"
          :mezzano
          (lisp-implementation-version)))

(define-implementation machine-core-info ()
  (multiple-value-bind (cpuid-max vendor-1 vendor-3 vendor-2) (mezzano.internals::cpuid 0)
    (let (model version)
      (when (>= cpuid-max 1)
        (multiple-value-bind (a b c d) (mezzano.internals::cpuid 1)
          (let ((model (ldb (byte 4 4) a))
                (family-id (ldb (byte 4 8) a))
                (extended-model-id (ldb (byte 4 16) a))
                (extended-family-id (ldb (byte 8 20) a)))
            (setf model (format NIL "~X ~X"
                                (if (or (= family-id #x6) (= family-id #xF))
                                    (+ (ash extended-model-id 4) model)
                                    model)
                                (if (= family-id #xF)
                                    (+ family-id extended-family-id)
                                    family-id))))))
      (values (mezzano.internals::decode-cpuid-vendor vendor-1 vendor-2 vendor-3)
              (or model "Unknown")
              (arch-type)
              (or version "Unknown")))))

(define-implementation machine-uptime ()
  (truncate (get-internal-run-time) INTERNAL-TIME-UNITS-PER-SECOND))

;; (define-implementation storage-device (path)
;;   (fail))

;; (define-implementation storage-device-path (device)
;;   (fail))

;; (define-implementation storage-room (device)
;;   (fail))

(define-implementation network-devices ()
  (mapcar #'princ-to-string (mezzano.sync:watchable-set-items mezzano.driver.network-card::*nics*)))

(defun network-card (device)
  (or (find device (mezzano.sync:watchable-set-items mezzano.driver.network-card::*nics*)
            :key #'princ-to-string
            :test #'equal)
      (error "No such device.")))

(define-implementation network-io-bytes (device)
  (etypecase device
    ((eql T)
     (let ((read 0) (write 0))
       (dolist (card (mezzano.sync:watchable-set-items mezzano.driver.network-card::*nics*))
         (multiple-value-bind (rx-bytes rx-packets rx-errors tx-bytes tx-packets tx-errors collisions)
             (mezzano.driver.network-card:statistics card)
           (declare (ignore rx-packets rx-errors tx-packets tx-errors collisions))
           (incf read rx-bytes)
           (incf write tx-bytes)))
       (values (+ read write) read write)))
    (string
     (multiple-value-bind (rx-bytes rx-packets rx-errors tx-bytes tx-packets tx-errors collisions)
         (mezzano.driver.network-card:statistics (network-card device))
       (declare (ignore rx-packets rx-errors tx-packets tx-errors collisions))
       (values (+ rx-bytes tx-bytes) rx-bytes tx-bytes)))))

(define-implementation network-info ()
  (values (machine-instance)))

(define-implementation network-address (device)
  (let ((card (network-card device)))
    (values (format NIL "~/mezzano.network.ethernet:format-mac-address/"
                    (mezzano.driver.network-card:mac-address card))
            (mezzano.network.ip:ipv4-interface-address card nil)
            NIL)))
