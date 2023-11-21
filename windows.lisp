(in-package #:org.shirakumo.machine-state)

#++
(define-implementation thread-time (thread)
  (cffi:with-foreign-objects ((creation-time :uint64)
                              (exit-time :uint64)
                              (kernel-time :uint64)
                              (user-time :uint64))
    (cffi:foreign-funcall "GetThreadTimes"
                          :pointer NIL
                          :pointer creation-time
                          :pointer exit-time
                          :pointer kernel-time
                          :pointer user-time
                          :bool)
    (* (float (+ (cffi:mem-ref kernel-time :uint64)
                 (cffi:mem-ref user-time :uint64))
              0d0)
       10e-9)))

(cffi:defcstruct (io-counters :conc-name io-counters-)
  (reads :ullong)
  (writes :ullong)
  (others :ullong)
  (read-bytes :ullong)
  (write-bytes :ullong)
  (other-bytes :ullong))

(define-implementation process-io-bytes ()
  (cffi:with-foreign-object (io-counters '(:struct io-counters))
    (cffi:foreign-funcall "GetProcessIoCounters"
                          :pointer (cffi:foreign-funcall "GetCurrentProcess" :pointer)
                          :pointer io-counters
                          :bool)
    (+ (io-counters-read-bytes io-counters)
       (io-counters-write-bytes io-counters)
       (io-counters-other-bytes io-counters))))

(define-implementation process-time ()
  (cffi:with-foreign-objects ((creation-time :uint64)
                              (exit-time :uint64)
                              (kernel-time :uint64)
                              (user-time :uint64))
    (cffi:foreign-funcall "GetProcessTimes"
                          :pointer (cffi:foreign-funcall "GetCurrentProcess" :pointer)
                          :pointer creation-time
                          :pointer exit-time
                          :pointer kernel-time
                          :pointer user-time
                          :bool)
    (* (float (+ (cffi:mem-ref kernel-time :uint64)
                 (cffi:mem-ref user-time :uint64))
              0d0)
       10e-9)))

(define-implementation machine-io-bytes ()
  0)

(define-implementation machine-time ()
  0d0)

(define-implementation machine-room ()
  (values 0 0))
