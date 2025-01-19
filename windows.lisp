(in-package #:org.shirakumo.machine-state)

(cffi:define-foreign-library psapi
  (:windows "Psapi.dll"))

(cffi:define-foreign-library ntdll
  (:windows "Ntdll.dll"))

(cffi:use-foreign-library psapi)
(cffi:use-foreign-library ntdll)

(defmacro windows-call (function &rest args)
  `(unless (cffi:foreign-funcall ,function ,@args)
     (fail (org.shirakumo.com-on:error-message))))

(defmacro nt-call (function &rest args)
  `(let ((value (cffi:foreign-funcall ,function ,@args)))
     (unless (= 0 value)
       (fail (org.shirakumo.com-on:error-message
              (cffi:foreign-funcall "LsaNtStatusToWinError" :uint32 value :ulong))))))

(defun process ()
  (cffi:foreign-funcall "GetCurrentProcess" :pointer))

(cffi:defcstruct (io-counters :conc-name io-counters-)
  (reads :ullong)
  (writes :ullong)
  (others :ullong)
  (read-bytes :ullong)
  (write-bytes :ullong)
  (other-bytes :ullong))

(define-implementation process-io-bytes ()
  (cffi:with-foreign-object (io-counters '(:struct io-counters))
    (windows-call "GetProcessIoCounters"
                  :pointer (process)
                  :pointer io-counters
                  :bool)
    (+ (io-counters-read-bytes io-counters)
       (io-counters-write-bytes io-counters)
       (io-counters-other-bytes io-counters))))

(cffi:defcstruct (memory-counters :conc-name memory-counters-)
  (cb :uint32)
  (page-fault-count :uint32)
  (peak-working-set-size :size)
  (working-set-size :size)
  (quota-peak-paged-pool-usage :size)
  (quota-paged-pool-usage :size)
  (quota-peak-non-paged-pool-usage :size)
  (quota-non-paged-pool-usage :size)
  (pagefile-usage :size)
  (peak-page-file-usage :size))

(define-implementation process-room ()
  (cffi:with-foreign-objects ((memory-counters '(:struct memory-counters)))
    (windows-call "GetProcessMemoryInfo"
                  :pointer (process)
                  :pointer memory-counters
                  :bool)
    (memory-counters-working-set-size memory-counters)))

(define-implementation process-time ()
  (cffi:with-foreign-objects ((creation-time :uint64)
                              (exit-time :uint64)
                              (kernel-time :uint64)
                              (user-time :uint64))
    (windows-call "GetProcessTimes"
                  :pointer (process)
                  :pointer creation-time
                  :pointer exit-time
                  :pointer kernel-time
                  :pointer user-time
                  :bool)
    (convert-file-time (cffi:mem-ref user-time :uint64))))

(cffi:defcstruct (memory-status :conc-name memory-status-)
  (length :uint32)
  (memory-load :uint32)
  (total-physical :uint64)
  (available-physical :uint64)
  (total-page-file :uint64)
  (available-page-file :uint64)
  (total-virtual :uint64)
  (available-virtual :uint64)
  (available-extended-virtual :uint64))

(define-implementation machine-room ()
  (cffi:with-foreign-objects ((memory-status '(:struct memory-status)))
    (let ((available (memory-status-available-physical memory-status))
          (total (memory-status-total-physical memory-status)))
      (values (- total available)
              total))))

(cffi:defcstruct (system-info :conc-name system-info-)
  (oem-id :uint32)
  (page-size :uint32)
  (minimum-application-address :pointer)
  (maximum-application-address :pointer)
  (active-processor-mask :uint64)
  (number-of-processors :uint32)
  (processor-type :uint32)
  (allocation-granularity :uint32)
  (processor-level :uint16)
  (processor-revision :uint16))

(define-implementation machine-cores ()
  (cffi:with-foreign-objects ((system-info '(:struct system-info)))
    (cffi:foreign-funcall "GetSystemInfo"
                          :pointer system-info
                          :void)
    (system-info-number-of-processors system-info)))

(define-implementation machine-uptime ()
  (cffi:with-foreign-objects ((time :long-long)
                              (freq :long-long))
    (windows-call "QueryUnbiasedInterruptTime" :pointer time :bool)
    (values (round (cffi:mem-ref time :long-long) 10000000))))

(declaim (inline convert-file-time))
(defun convert-file-time (time)
  (* 10d-9 (float time 0d0)))

(define-implementation machine-time (core)
  (etypecase core
    ((eql T)
     (cffi:with-foreign-objects ((idle-time :uint64)
                                 (kernel-time :uint64)
                                 (user-time :uint64))
       (windows-call "GetSystemTimes"
                     :pointer idle-time
                     :pointer kernel-time
                     :pointer user-time
                     :bool)
       (values (convert-file-time (cffi:mem-ref idle-time :uint64))
               (convert-file-time (+ (cffi:mem-ref kernel-time :uint64)
                                     (cffi:mem-ref idle-time :uint64)
                                     (cffi:mem-ref user-time :uint64))))))
    (integer
     ;; TODO: this
     )))

(defmacro with-thread-handle ((handle thread &optional (default 0)) &body body)
  `(if (or (eql ,thread T)
           (eql ,thread (bt:current-thread)))
       (let ((,handle (cffi:foreign-funcall "GetCurrentThread" :pointer)))
         ,@body)
       ,default))

(define-implementation thread-time (thread)
  (with-thread-handle (handle thread)
    (cffi:with-foreign-objects ((creation-time :uint64)
                                (exit-time :uint64)
                                (kernel-time :uint64)
                                (user-time :uint64))
      (windows-call "GetThreadTimes"
                    :pointer handle
                    :pointer creation-time
                    :pointer exit-time
                    :pointer kernel-time
                    :pointer user-time
                    :bool)
      (convert-file-time (cffi:mem-ref user-time :uint64)))))

(cffi:defcstruct (thread-info :conc-name thread-info-)
  (exit-status :uint32)
  (base-address :pointer)
  (process :pointer)
  (thread :pointer)
  (affinity-mask :uint64)
  (priority :long)
  (base-priority :long))

(define-implementation thread-core-mask (thread)
  (with-thread-handle (handle thread (1- (ash 1 (machine-cores))))
    (cffi:with-foreign-objects ((info '(:struct thread-info)))
      (cffi:foreign-funcall "NtQueryInformationThread"
                            :pointer handle
                            :int #x04
                            :pointer info
                            :ulong (cffi:foreign-type-size '(:struct thread-info))
                            :uint32)
      (thread-info-affinity-mask info))))

(define-implementation (setf thread-core-mask) (mask thread)
  (with-thread-handle (handle thread (1- (ash 1 (machine-cores))))
    (if (= 0 (cffi:foreign-funcall "SetThreadAffinityMask"
                                   :pointer handle
                                   :uint64 mask
                                   :uint64))
        (fail (org.shirakumo.com-on:error-message))
        mask)))

(define-implementation process-priority ()
  (let ((value (cffi:foreign-funcall "GetPriorityClass" :pointer (process))))
    (case (cffi:foreign-funcall "GetPriorityClass" :pointer (process))
      (#x00000000 (fail (org.shirakumo.com-on:error-message)))
      (#x00000040 :idle)
      (#x00004000 :low)
      (#x00000020 :normal)
      (#x00000080 :high)
      (#x00000100 :realtime)
      (T :normal))))

(define-implementation (setf process-priority) (priority)
  (windows-call "SetPriorityClass"
                :pointer (process)
                :uint16 (ecase priority
                          (:idle     #x00000040)
                          (:low      #x00004000)
                          (:normal   #x00000020)
                          (:high     #x00000080)
                          (:realtime #x00000100))
                :bool)
  priority)

(define-implementation thread-priority (thread)
  (with-thread-handle (handle thread :normal)
    (let ((value (cffi:foreign-funcall "GetThreadPriority" :pointer handle :uint)))
      (when (= value 2147483647)
        (fail (org.shirakumo.com-on:error-message)))
      (cond ((< value -8) :idle)
            ((< value  0) :low)
            ((= value  0) :normal)
            ((< value +8) :high)
            (T            :realtime)))))

(define-implementation (setf thread-priority) (thread priority)
  (with-thread-handle (handle thread :normal)
    (windows-call "SetThreadPriority"
                  :pointer handle
                  :int (ecase priority
                         (:idle    -15)
                         (:low      -1)
                         (:normal    0)
                         (:high      2)
                         (:realtime 15))
                  :bool)
    priority))

(define-implementation storage-room (path)
  (cffi:with-foreign-objects ((available-to-caller :int64)
                              (total :int64)
                              (available :int64))
    (org.shirakumo.com-on:with-wstring (str (pathname-utils:native-namestring
                                             (pathname-utils:to-directory path)))
      (windows-call "GetDiskFreeSpaceExW"
                    :pointer str
                    :pointer available-to-caller
                    :pointer total
                    :pointer available
                    :bool))
    (values (cffi:mem-ref available :int64)
            (cffi:mem-ref total :int64))))
