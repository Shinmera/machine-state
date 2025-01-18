(defpackage #:org.shirakumo.machine-state.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:machine-state #:org.shirakumo.machine-state))
  (:export))

(in-package #:org.shirakumo.machine-state.test)

(define-test machine-state
  (is >= 0 (machine-state:process-io-bytes))
  (is >= 0 (machine-state:process-room))
  (is >= 0 (machine-state:process-time))
  (is-values (machine-state:machine-room) (>= 0) (>= 0))
  (is >= 1 (machine-state:machine-cores))
  (is >= 0 (machine-state:machine-uptime))
  (is >= 0 (machine-state:thread-time T))
  (is >= 1 (machine-state:thread-core-mask T))
  (is member '(:IDLE :LOW :NORMAL :HIGH :REALTIME) (machine-state:process-priority))
  (is member '(:IDLE :LOW :NORMAL :HIGH :REALTIME) (machine-state:thread-priority T))
  (is-values (machine-state:gc-room) (>= 0) (>= 0))
  (is >= 0 (machine-state:gc-time))
  (is-values (machine-state:stack-room) (>= 0) (>= 0))
  (is >= 0 (machine-state:static-room))
  (is-values (machine-state:storage-room *default-pathname-defaults*) (>= 0) (>= 0)))
