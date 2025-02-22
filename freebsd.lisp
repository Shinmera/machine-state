(in-package #:org.shirakumo.machine-state)

(define-implementation machine-room ()
  (let ((page-size (sysctl-ref "hw.pagesize" :uint32))
        (physmem (sysctl-ref "hw.physmem" :uint64))
        (free (sysctl-ref "vm.stats.vm.v_free_count" :uint32)))
    (values (- physmem (* free page-size)) physmem)))

(define-implementation machine-uptime ()
  (with-sysctl ("kern.boottime" tv '(:struct timeval))
    (- (get-unix-time) (timeval-sec tv))))

(define-implementation machine-cores ()
  (sysctl-ref "hw.ncpu" :int))

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/sys/resource.h#L172
(defconstant +cpustates+ 5)

(defun cpu-time ()
  (sysctl-ref "kern.cp_time" `(:array :uint64 ,+cpustates+)))

(defun core-time (core)
  (let ((size (* (machine-cores) +cpustates+)))
    (with-sysctl ("kern.cp_times" cpustates :uint64 size)
      (cffi:mem-aref cpustates `(:array :uint64 ,+cpustates+) core))))

(define-implementation machine-time (core)
  (with-sysctl ("kern.clockrate" clockinfo '(:struct clockinfo))
    (flet ((conv (x) (/ x (float (clockinfo-hz clockinfo) 0.0d0))))
      (let ((values (cond
                      ((eq 't core) (cpu-time))
                      ((>= core (machine-cores)) (fail "No such core."))
                      (t (core-time core)))))
        (destructuring-bind (user nice sys intr idle) (coerce values 'list)
          (values (conv idle)
                  (conv (+ user nice sys intr idle))))))))

(define-implementation machine-info ()
  (values "Unknown" "Unknown" :freebsd (sysctl-string "kern.osrelease" 32)))

(define-implementation machine-core-info ()
  (let ((processor (sysctl-string "hw.model" 128)))
    (values processor
            processor ;; There doesn't seem to be a separation between those
            (arch-type)
            (sysctl-string "hw.machine" 32))))

(cffi:defcstruct (rusage :size 144 :conc-name rusage-)
  (user-time (:struct timeval))
  (system-time (:struct timeval)))

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/sys/user.h#L118
(cffi:defcstruct (kinfo-proc :size 1088 :conc-name kinfo-proc-)
  (user-id :uint32 :offset 168) ;; ki_uid
  (group-id :uint32 :offset 180) ;; ki_rgid
  (resident-set-size :int32 :offset 264) ;; ki_rssize
  (nice :int8 :offset 389) ;; ki_nice
  (command-name (:array :char #.(1+ +maxcomlen+)) :offset 447) ;; ki_comm
  ;; (runtime :uint64 :offset 328) ;; ki_runtime (microseconds)
  (thread-id :int32 :offset 600)  ;; ki_tid
  (rusage (:struct rusage) :offset 608)) ;; ki_rusage

(defmacro with-current-process ((proc) &body body)
  `(with-sysctl (("kern.proc.pid" (getpid)) ,proc '(:struct kinfo-proc))
     ,@body))

(define-implementation process-room ()
  (with-current-process (proc)
    (* (page-size) (kinfo-proc-resident-set-size proc))))

(define-implementation process-priority ()
  (with-current-process (proc)
    (process-nice->priority (kinfo-proc-nice proc))))

(define-implementation process-time ()
  (with-current-process (proc)
    (let* ((rusage (cffi:foreign-slot-pointer proc '(:struct kinfo-proc) 'rusage))
           (tv (cffi:foreign-slot-pointer rusage '(:struct rusage) 'user-time)))
      (timeval->seconds tv))))

(define-implementation process-info ()
  (with-current-process (proc)
    (values (let ((command (cffi:foreign-string-to-lisp
                            (cffi:foreign-slot-pointer proc '(:struct kinfo-proc) 'command-name))))
              (or (resolve-executable command) command))
            (cffi:with-foreign-object (cwd :char 1024)
              (cffi:foreign-funcall "getcwd" (:pointer :char) cwd :size 1024)
              (pathname-utils:parse-native-namestring (cffi:foreign-string-to-lisp cwd :max-chars 1024) :as :directory))
            (uid->user (kinfo-proc-user-id proc))
            (gid->group (kinfo-proc-group-id proc)))))

(cffi:defcstruct (stat :size 224 #| 64-bit |# :conc-name stat-)
  (dev     :int :offset 0) ;; st_dev
  (mode    :int :offset 24)) ;; st_mode

(defconstant +mnamelen+ 1024)

#+64-bit
(cffi:defcstruct (statfs :size 2344 :conc-name statfs-)
  (block-size :uint32 :offset 16)
  (blocks :uint64 :offset 32)
  (available-blocks :int64 :offset 48) ;; Blocks available to non-superuser
  (synchronous-writes :uint64 :offset 72)
  (synchronous-reads :uint64 :offset 80)
  (asynchronous-writes :uint64 :offset 88)
  (asynchronous-reads :uint64 :offset 96)
  (device (:array :char #.+mnamelen+) :offset 296)
  (mountpoint (:array :char #.+mnamelen+) :offset 1320))

(define-implementation storage-device (path)
  (let ((mount-root (pathname-utils:native-namestring (pathname-force-file (find-mount-root path)))))
    (do-filesystems (fs)
      (let ((fs-mountpoint (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint)))
        (when (strncmp-lisp fs-mountpoint mount-root :max-chars +mnamelen+)
          (return (cffi:foreign-string-to-lisp
                   (cffi:foreign-slot-pointer fs '(:struct statfs) 'device) :max-chars +mnamelen+)))))))

(define-implementation storage-device-path (device)
  (do-filesystems (fs)
    (let ((fs-device (cffi:foreign-slot-pointer fs '(:struct statfs) 'device)))
      (when (strncmp-lisp fs-device device :max-chars +mnamelen+)
        (return (pathname-utils:parse-native-namestring
                 (cffi:foreign-string-to-lisp
                  (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint) :max-chars +mnamelen+)))))))

;;;; TODO: does NOT work with ZFS
(define-implementation storage-io-bytes (path)
  (setf path (pathname-utils:native-namestring
              (pathname-force-file
               (etypecase path
                 (pathname (find-mount-root path))
                 (string (storage-device-path path))))))

  (do-filesystems (fs)
    (let ((fs-mountpoint (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint)))
      (when (strncmp-lisp fs-mountpoint path :max-chars +mnamelen+)
        (let ((reads (+ (statfs-synchronous-reads fs)
                        (statfs-asynchronous-reads fs)))
              (writes (+ (statfs-synchronous-writes fs)
                         (statfs-asynchronous-writes fs))))
          (return-from storage-io-bytes
            (values (+ reads writes) reads writes)))))))

(define-implementation storage-room (path)
  (when (stringp path)
    (setf path (storage-device-path path)))

  (let ((mount-root (pathname-utils:native-namestring (pathname-force-file (find-mount-root path)))))
    (do-filesystems (fs)
      (let ((fs-mountpoint (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint)))
        (when (strncmp-lisp fs-mountpoint mount-root :max-chars +mnamelen+)
          (flet ((block->bytes (n)
                   (* n (statfs-block-size fs))))
            (return-from storage-room
              (values (block->bytes (statfs-available-blocks fs))
                      (block->bytes (statfs-blocks fs))))))))))

(define-implementation network-info ()
  (sysctl-string "kern.hostname" 255))

(cffi:defcstruct (if-data :size 152 ;; #+openbsd 136
                          :conc-name if-data-)
  (ibytes :uint64 :offset 64)
  (obytes :uint64 :offset 72))

(define-implementation network-io-bytes (device)
  (let ((read 0) (written 0))
    (do-ifaddrs (ifaddr)
      (when (string= device (ifaddrs-name ifaddr))
        (let ((data (ifaddrs-data ifaddr)))
          (incf read (if-data-ibytes data))
          (incf written (if-data-obytes data)))))
    (values (+ read written) read written)))

;;;; Reference:
;;;; https://github.com/freebsd/freebsd-src/blob/main/usr.sbin/acpi/acpiconf/acpiconf.c
;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/dev/acpica/acpiio.h

(cffi:defcstruct (acpi-battinfo :size 16))
(cffi:defcstruct (acpi-bif :size 164))

;; https://github.com/freebsd/freebsd-src/blob/main/usr.sbin/acpi/acpiconf/acpiconf.c#L82
(defconstant +unknown-cap+ #xffffffff)
(defconstant +unknown-voltage+ #xffffffff)

(cffi:defcstruct (acpi-bst :size 16 :conc-name acpi-bst-)
  (state :uint32) ;; state
  (rate :uint32) ;; rate
  (capacity :uint32) ;; cap
  (voltage :uint32)) ;; volts

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/dev/acpica/acpiio.h#L76
(cffi:defcstruct (acpi-bix :size 256 :conc-name acpi-bix-)
  (units :uint32 :offset 0) ;; units
  (design-capacity :uint32 :offset 4) ;; dcap
  (last-full-capacity :uint32 :offset 8) ;; lfcap
  (design-volts :uint32 :offset 16)) ;; dvol

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/dev/acpica/acpiio.h#L175
(cffi:defcunion acpi-battery-ioctl-arg
  (unit :int)
  (battinfo (:struct acpi-battinfo))
  (bix (:struct acpi-bix))
  (bif (:struct acpi-bif))
  (bst (:struct acpi-bst)))

(defconstant +acpi-bix-units-mw+ 0)
(defconstant +acpi-bix-units-ma+ 1)
(defun acpi-bix-last-full-capacity-mAh (bix)
  (flet ((mWh->mAh (mWh mV)
           (/ mWh (* 1000 mV))))
    (let ((last (acpi-bix-last-full-capacity bix))
          (unit (acpi-bix-units bix)))
      (cond
        ((= +unknown-cap+ last) 0.0d0)
        ((= +acpi-bix-units-mw+ unit)
         (let ((mV (acpi-bix-design-volts bix)))
           (if (= +unknown-voltage+ mV)
               0.0d0
               (mWh->mAh last mV))))
        ((= +acpi-bix-units-ma+ unit) last)
        (t 0.0d0)))))

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/dev/acpica/acpiio.h#L188
(defconstant +acpiio-bat-get-bix+ 3238019600)
(defconstant +acpiio-bat-get-bst+ 3238019601)

(defun battery-last-full-capacity (acpifd battio battn)
  (setf (cffi:foreign-slot-value battio '(:union acpi-battery-ioctl-arg) 'unit) battn)
  (posix-call "ioctl" :int acpifd :unsigned-long +acpiio-bat-get-bix+ :pointer battio :int)
  (let ((bix (cffi:foreign-slot-pointer battio '(:union acpi-battery-ioctl-arg) 'bix)))
    (acpi-bix-last-full-capacity-mAh bix)))

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/dev/acpica/acpiio.h#L157
(defconstant +acpi-batt-stat-discharg+ 1)
(defconstant +acpi-batt-stat-charging+ 2)
(defconstant +acpi-batt-stat-critical+ 4)
(defconstant +acpi-batt-stat-invalid+ (logior +acpi-batt-stat-discharg+ +acpi-batt-stat-charging+))
(defconstant +acpi-batt-stat-bst-mask+ (logior +acpi-batt-stat-invalid+ +acpi-batt-stat-critical+))
(defconstant +acpi-batt-stat-not-present+ +acpi-batt-stat-bst-mask+)

(defun battery-state (acpifd battio battn)
  (setf (cffi:foreign-slot-value battio '(:union acpi-battery-ioctl-arg) 'unit) battn)
  (posix-call "ioctl" :int acpifd :unsigned-long +acpiio-bat-get-bst+ :pointer battio :int)
  (let* ((bst (cffi:foreign-slot-pointer battio '(:union acpi-battery-ioctl-arg) 'bix))
         (remaining-capacity (acpi-bst-capacity bst))
         (state (acpi-bst-state bst)))
    (values (if (or (= remaining-capacity +unknown-cap+)
                    (= remaining-capacity -1))
                0.0d0
                remaining-capacity)
            (if (= +acpi-batt-stat-not-present+ state)
                nil
                (ecase (logand state +acpi-batt-stat-bst-mask+)
                  (0 :charging) ;; 0 is reported as "high" on acpiconf
                  (#.+acpi-batt-stat-charging+ :charging)
                  (#.+acpi-batt-stat-discharg+ :discharging)
                  (t nil))))))

(define-implementation machine-battery ()
  (with-fd (acpifd #P"/dev/acpi" :direction :input)
    (cffi:with-foreign-object (battio '(:union acpi-battery-ioctl-arg))
      (let ((last-full-capacity (battery-last-full-capacity acpifd battio 0)))
        (multiple-value-bind (remaining-capacity state) (battery-state acpifd battio 0)
          (values (float remaining-capacity 0.0d0)
                  (float last-full-capacity 0.0d0)
                  ;; There doesn't seem to be a "full" state, full capacity still shows as charging
                  ;; so detect it manually
                  (if (= remaining-capacity last-full-capacity)
                      :full
                      state)))))))
