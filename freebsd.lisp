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

;;;; TODO: storage-io-bytes
;;;; TODO: thread-io-bytes
;;;; TODO: machine-battery
