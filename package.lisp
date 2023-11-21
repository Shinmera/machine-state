(defpackage #:org.shirakumo.machine-state
  (:use #:cl)
  (:export
   #:process-io-bytes
   #:process-room
   #:process-time
   #:machine-room
   #:thread-time
   #:gc-room
   #:gc-time
   #:gpu-room
   #:gpu-time))
