(defpackage #:org.shirakumo.machine-state
  (:use #:cl)
  (:export
   #:query-failed
   #:process-io-bytes
   #:process-room
   #:process-time
   #:machine-room
   #:machine-cores
   #:thread-time
   #:thread-core-mask
   #:process-priority
   #:thread-priority
   #:gc-room
   #:gc-time
   #:gpu-room
   #:gpu-time
   #:static-room
   #:stack-room))
