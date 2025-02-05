(in-package #:org.shirakumo.machine-state)

(cffi:define-foreign-library psapi
  (:windows "Psapi.dll"))

(cffi:define-foreign-library ntdll
  (:windows "Ntdll.dll"))

(cffi:define-foreign-library pdh
  (:windows "Pdh.dll"))

(cffi:define-foreign-library iphlpapi
  (:windows "Iphlpapi.dll"))

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

(defmacro pdh-call (&rest args)
  `(let ((ret (cffi:foreign-funcall ,@args :size)))
     (unless (= 0 ret)
       (let ((msg (com:error-message ret 'pdh)))
         (fail (if (string/= "" msg) 
                   msg
                   (format NIL "Performance counter call failed with ~d" ret))
               'machine-time)))))

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
    (values (+ (io-counters-read-bytes io-counters)
               (io-counters-write-bytes io-counters)
               (io-counters-other-bytes io-counters))
            (io-counters-read-bytes io-counters)
            (io-counters-write-bytes io-counters))))

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
     (unless (cffi:foreign-library-loaded-p 'pdh)
       (cffi:load-foreign-library 'pdh))
     (cffi:with-foreign-objects ((handle :pointer)
                                 (counters :pointer 3)
                                 (type :uint32)
                                 (data :double))
       (pdh-call "PdhOpenQueryW"
                 :pointer (cffi:null-pointer)
                 :pointer (cffi:null-pointer)
                 :pointer handle)
       (let ((handle (cffi:mem-ref handle :pointer)))
         (unwind-protect
              (progn
                (flet ((add-counter (i name)
                         (pdh-call "PdhAddCounterW"
                                   :pointer handle
                                   com:wstring name
                                   :pointer (cffi:null-pointer)
                                   :pointer (cffi:mem-aptr counters :pointer i))))
                  (add-counter 0 (format NIL "\\Processor(~d)\\% Processor Time" core))
                  (add-counter 1 (format NIL "\\Processor(~d)\\% Idle Time" core))
                  (add-counter 2 (format NIL "\\Processor(~d)\\% Privileged Time" core)))
                (pdh-call "PdhCollectQueryData" :pointer handle)
                (flet ((get-counter (i)
                         (pdh-call "PdhGetFormattedCounterValue"
                                   :pointer (cffi:mem-aptr counters :pointer i)
                                   :uint32 #x00000200 #|PDH_FMT_DOUBLE|#
                                   :pointer type
                                   :pointer data)
                         (cffi:mem-ref data :double)))
                  (let ((proc (get-counter 0))
                        (idle (get-counter 1))
                        (priv (get-counter 2)))
                    (values idle (+ proc idle priv)))))
           (cffi:foreign-funcall "PdhCloseQuery" :pointer handle)))))))

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

(define-implementation storage-device (path)
  (etypecase path
    (pathname (or (pathname-device path)
                  (pathname-device *default-pathname-defaults*)
                  "C"))
    (string (or (pathname-device (pathname-utils:parse-native-namestring path))
                (pathname-device *default-pathname-defaults*)
                "C"))))

(define-implementation storage-device-path (device)
  (make-pathname :device device :directory '(:absolute)))

(define-implementation storage-room (path)
  (cffi:with-foreign-objects ((available-to-caller :int64)
                              (total :int64)
                              (available :int64))
    (windows-call "GetDiskFreeSpaceExW"
                  com:wstring (etypecase path
                                (string (format NIL "~a:/" path))
                                (pathname
                                 (pathname-utils:native-namestring (pathname-utils:to-directory path))))
                  :pointer available-to-caller
                  :pointer total
                  :pointer available
                  :bool)
    (values (cffi:mem-ref available :int64)
            (cffi:mem-ref total :int64))))

(cffi:defcstruct (disk-performance :conc-name disk-performance-)
  (bytes-read :int64)
  (bytes-written :int64)
  (read-time :int64)
  (write-time :int64)
  (idle-time :int64)
  (read-count :uint32)
  (write-count :uint32)
  (queue-depth :uint32)
  (split-count :uint32)
  (query-time :int64)
  (storage-device-number :uint32)
  (storage-manager-name :uint16 :count 8))

(define-implementation storage-io-bytes (device)
  (when (pathnamep device)
    (setf device (storage-device device)))
  (let ((handle (cffi:foreign-funcall "CreateFileA"
                                      :string (format NIL "\\\\.\\~a:" device)
                                      :uint32 0
                                      :uint32 3 #| FILE_SHARE_READ | FILE_SHARE_WRITE |#
                                      :pointer (cffi:null-pointer)
                                      :uint32 3 #| OPEN_EXISTING |#
                                      :uint32 #x02000000 #| FILE_FLAG_BACKUP_SEMANTICS |#
                                      :pointer (cffi:null-pointer)
                                      :pointer)))
    (when (= (cffi:pointer-address handle) #+64-bit (1- (ash 1 64)) #-64-bit (1- (ash 1 32)))
      (fail (org.shirakumo.com-on:error-message)))
    (unwind-protect
         (cffi:with-foreign-objects ((perf '(:struct disk-performance)))
           (windows-call "DeviceIoControl"
                         :pointer handle
                         :uint32 458784 #| IOCTL_DISK_PERFORMANCE |#
                         :pointer (cffi:null-pointer)
                         :uint32 0
                         :pointer perf
                         :uint32 (cffi:foreign-type-size '(:struct disk-performance))
                         :pointer (cffi:null-pointer)
                         :pointer (cffi:null-pointer)
                         :bool)
           (values (+ (disk-performance-bytes-read perf)
                      (disk-performance-bytes-written perf))
                   (disk-performance-bytes-read perf)
                   (disk-performance-bytes-written perf)))
      (cffi:foreign-funcall "CloseHandle" :pointer handle))))

(cffi:defcstruct (ifrow :size 1352 :conc-name ifrow-)
  (alias       :uint16 :count 257 :offset 28)
  (in-octets   :uint64 :offset 1208)
  (out-octets  :uint64 :offset 1280))

(cffi:defcstruct (iftable :conc-name iftable-)
  (entries :ulong)
  (table (:struct ifrow) :count 128))

(define-implementation network-devices ()
  (unless (cffi:foreign-library-loaded-p 'iphlpapi)
    (cffi:load-foreign-library 'iphlpapi))
  (cffi:with-foreign-objects ((table :pointer))
    (let ((ret (cffi:foreign-funcall "GetIfTable2" :pointer table :size)))
      (unless (= 0 ret)
        (let ((msg (com:error-message ret 'iphlpapi)))
          (fail (if (string/= "" msg) msg
                    (format NIL "GetIfTable2 call failed with ~d" ret))))))
    (let ((table (cffi:mem-ref table :pointer)))
      (unwind-protect
           (let ((list ()))
             (dotimes (i (iftable-entries table) (nreverse list))
               (let* ((row (cffi:mem-aptr (cffi:foreign-slot-pointer table '(:struct iftable) 'table)
                                          '(:struct ifrow) i))
                      (name (com:wstring->string
                             (cffi:foreign-slot-pointer row '(:struct ifrow) 'alias)
                             256)))
                 (push name list))))
        (cffi:foreign-funcall "FreeMibTable" :pointer table)))))

(define-implementation network-io-bytes (device)
  (unless (cffi:foreign-library-loaded-p 'iphlpapi)
    (cffi:load-foreign-library 'iphlpapi))
  (cffi:with-foreign-objects ((table :pointer))
    (let ((ret (cffi:foreign-funcall "GetIfTable2" :pointer table :size)))
      (unless (= 0 ret)
        (let ((msg (com:error-message ret 'iphlpapi)))
          (fail (if (string/= "" msg) msg
                    (format NIL "GetIfTable2 call failed with ~d" ret))))))
    (let ((table (cffi:mem-ref table :pointer)))
      (unwind-protect
           (etypecase device
             ((eql T)
              (let ((read 0) (write 0))
                (declare (type (unsigned-byte 64) read write))
                (dotimes (i (iftable-entries table) (values (+ read write) read write))
                  (let ((row (cffi:mem-aptr (cffi:foreign-slot-pointer table '(:struct iftable) 'table)
                                            '(:struct ifrow) i)))
                    (incf read (ifrow-in-octets row))
                    (incf write (ifrow-out-octets row))))))
             (string
              (dotimes (i (iftable-entries table) (fail "No such device found."))
                (let* ((row (cffi:mem-aptr (cffi:foreign-slot-pointer table '(:struct iftable) 'table)
                                           '(:struct ifrow) i))
                       (name (com:wstring->string
                              (cffi:foreign-slot-pointer row '(:struct ifrow) 'alias)
                              256)))
                  (when (string= name device)
                    (return (values (+ (ifrow-in-octets row)
                                       (ifrow-out-octets row))
                                    (ifrow-in-octets row)
                                    (ifrow-out-octets row))))))))
        (cffi:foreign-funcall "FreeMibTable" :pointer table)))))

(define-implementation network-info ()
  (cffi:with-foreign-object (hostname :char 512)
    (posix-call "gethostname" :pointer hostname :size 512 :int)
    (cffi:foreign-string-to-lisp hostname :max-chars 512)))
