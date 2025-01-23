(defpackage #:org.shirakumo.machine-state
  (:use #:cl)
  #+windows
  (:local-nicknames (#:com #:org.shirakumo.com-on))
  (:export
   #:query-failed
   #:process-io-bytes
   #:process-room
   #:process-time
   #:machine-room
   #:machine-cores
   #:machine-uptime
   #:machine-time
   #:thread-time
   #:thread-core-mask
   #:process-priority
   #:thread-priority
   #:gc-room
   #:gc-time
   #:gpu-room
   #:gpu-time
   #:static-room
   #:stack-room
   #:storage-device
   #:storage-device-path
   #:storage-room
   #:storage-io-bytes
   #:network-io-bytes))
