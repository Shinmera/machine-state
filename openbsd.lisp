(in-package #:org.shirakumo.machine-state)

;;;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L103
(defconstant +ctl-kern+ 1)
(defconstant +kern-proc-pid+ 1)
(defconstant +kern-osrelease+ 2)
(defconstant +kern-hostname+ 10)
(defconstant +kern-clockrate+ 12)
(defconstant +kern-boottime+ 21)
(defconstant +kern-cptime+ 40)
(defconstant +kern-proc+ 66)
(defconstant +kern-cptime2+ 71)
(defconstant +kern-proc-cwd+ 78)
(defconstant +kern-cpustats+ 85)
(defconstant +kern-proc-show-threads+ #x40000000)

;;;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L919
(defconstant +ctl-hw+ 6)
(defconstant +hw-machine+ 1)
(defconstant +hw-model+ 2)
(defconstant +hw-diskstats+ 9)
(defconstant +hw-diskcount+ 10)
(defconstant +hw-sensors+ 11)
(defconstant +hw-vendor+ 14)
(defconstant +hw-product+ 15)
(defconstant +hw-physmem64+ 19)
(defconstant +hw-ncpuonline+ 25)

;;;; https://github.com/openbsd/src/blob/master/sys/uvm/uvmexp.h#L7
(defconstant +ctl-vm+ 2)
(defconstant +vm-uvmexp+ 4)

;;;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L370
(cffi:defcstruct (kinfo-proc :size 644 :conc-name kinfo-proc-)
  (user-id :uint32 :offset 128) ;; p_uid
  (group-id :uint32 :offset 136) ;; p_gid
  (nice :uint8 :offset 307) ;; p_nice
  (command-name (:array :char #.+maxcomlen+) :offset 312) ;; p_comm
  (resident-set-size :int32 :offset 384) ;; p_vm_rssize
  (user-time-seconds :uint32 :offset 420) ;; p_uutime_sec
  (user-time-microseconds :uint32 :offset 424) ;; p_uutime_usec
  (thread-id :int32 :offset 608) ;; p_tid
  (thread-name (:array :char #.+maxcomlen+) :offset 624)) ;; p_name

;;;; https://github.com/openbsd/src/blob/master/sys/sys/sysctl.h#L752
(cffi:defcstruct (kinfo-file :size 624 :conc-name kinfo-file-)
  (read-bytes :uint64 :offset 96) ;; f_rbytes
  (written-bytes :uint64 :offset 104)) ;; f_wbytes

(defmacro with-current-process ((proc) &body body)
  (let ((size (cffi:foreign-type-size '(:struct kinfo-proc))))
    `(with-sysctl ((+ctl-kern+ +kern-proc+ +kern-proc-pid+ (getpid) ,size 1) ,proc '(:struct kinfo-proc))
       ,@body)))

(define-implementation process-room ()
  (with-current-process (proc)
    (* (page-size) (kinfo-proc-resident-set-size proc))))

;;;; These functions are shared between the processes and threads impls
;;;; User time only, will differ from output of PS or TOP
(defun %process-time (kinfo-proc)
  (+ (kinfo-proc-user-time-seconds kinfo-proc)
     (/ (kinfo-proc-user-time-microseconds kinfo-proc) 1000000.0d0)))

(define-implementation process-time ()
  (with-current-process (proc)
    (%process-time proc)))

(define-implementation process-priority ()
  (with-current-process (proc)
    (let ((value (- (kinfo-proc-nice proc) 20)))  ;; Will return between 0 (-20) and 40 (20)
      (process-nice->priority value))))

#+thread-support
(progn
  (defmacro with-threads ((thread &optional pid) &body body)
    (let ((mib (gensym)) (%pid (gensym)) (i (gensym)) (nproc (gensym)) (procs (gensym)) (kinfo-proc-size (gensym)))
      ;; Call sysctl once to find how many bytes will be returned
      `(let* ((,%pid (or ,pid (getpid)))
              (,kinfo-proc-size (cffi:foreign-type-size '(:struct kinfo-proc)))
              (,mib (list +ctl-kern+ +kern-proc+ (logior +kern-proc-pid+ +kern-proc-show-threads+) ,%pid ,kinfo-proc-size 0))
              (,nproc (ceiling (/ (sysctl ,mib nil) ,kinfo-proc-size))))
         (rplaca (last ,mib) ,nproc)
         (cffi:with-foreign-object (,procs '(:struct kinfo-proc) ,nproc)
           (sysctl ,mib ,procs (* (cffi:foreign-type-size '(:struct kinfo-proc)) ,nproc))
           (dotimes (,i ,nproc)
             (let ((,thread (cffi:mem-aptr ,procs '(:struct kinfo-proc) ,i)))
               (when (> (kinfo-proc-thread-id ,thread) 1)
                 ,@body)))))))

  (defmacro with-current-thread ((thread) &body body)
    (let ((tid (gensym)))
      `(let ((,tid (cffi:foreign-funcall "getthrid" :long)))
         (with-threads (,thread)
           (when (= ,tid (kinfo-proc-thread-id ,thread))
             (return ,@body))))))

  (defmacro with-current-thread-handle ((handle thread &optional (default 0)) &body body)
    `(if (or (eql ,thread T)
             (eql ,thread (bt:current-thread)))
         (with-current-thread (,handle)
           ,@body)
         ,default))

  (define-implementation thread-time (thread)
    (with-current-thread-handle (handle thread 0.0d0)
      (%process-time handle))))

;;;; process-io-bytes, thread-priority and thread-core-mask are unsupported
;;;; Reference: https://github.com/openbsd/src/blob/master/include/unistd.h#L100

(defun split-path (path &optional (delimiter #\:))
  (let (paths start)
    (do ((i 0 (1+ i)))
        ((= i (length path)) (nreverse paths))
      (when (char= (schar path i) delimiter)
        (push (subseq path (or start 0) i) paths)
        (setf start (1+ i))))))

(define-implementation process-info ()
  (with-current-process (proc)
    (values (let ((command (cffi:foreign-string-to-lisp
                            (cffi:foreign-slot-pointer proc '(:struct kinfo-proc) 'command-name))))
              (or (resolve-executable command) command))
            (pathname-utils:parse-native-namestring (sysctl-string (+ctl-kern+ +kern-proc-cwd+ (getpid)) 1024) :as :directory)
            (uid->user (kinfo-proc-user-id proc))
            (gid->group (kinfo-proc-group-id proc)))))

(cffi:defcstruct (uvmexp :size 344 :conc-name uvmexp-)
  (pagesize :int :offset 0)
  (npages :int :offset 12)
  (free :int :offset 16)
  (inactive :int :offset 24))

(define-implementation machine-room ()
  (with-sysctl ((+ctl-vm+ +vm-uvmexp+) uvm '(:struct uvmexp))
    (flet ((pages->bytes (n)
             (* n (uvmexp-pagesize uvm))))
      (let* ((total-pages (uvmexp-npages uvm))
             (free-pages (+ (uvmexp-free uvm) (uvmexp-inactive uvm)))
             (total-bytes (pages->bytes total-pages))
             (free-bytes (pages->bytes free-pages)))
        (values (- total-bytes free-bytes)
                total-bytes)))))

(define-implementation machine-uptime ()
  (with-sysctl ((+ctl-kern+ +kern-boottime+) tv '(:struct timeval))
    (- (get-unix-time) (timeval-sec tv))))

(define-implementation machine-cores ()
  (with-sysctl ((+ctl-hw+ +hw-ncpuonline+) cores :int)
    (cffi:mem-ref cores :int)))

(defconstant +cpustates+ 6)

(cffi:defcstruct (cpustats :size 56 :conc-name cpustats-)
  (times (:array :uint64 #.+cpustates+))) ;; cs_time

(defun core-time (core)
  (with-sysctl ((+ctl-kern+ +kern-cpustats+ core) cpustats '(:struct cpustats))
    (cpustats-times cpustats)))

(defun cpu-time ()
  (with-sysctl ((+ctl-kern+ +kern-cptime+) cpustates :long +cpustates+)
    (cffi:mem-ref cpustates `(:array :long ,+cpustates+))))

;;;; KERN_CPTIME2 returns wrong values for some reason, KERN_CPUSTATS works better
(define-implementation machine-time (core)
  (with-sysctl ((+ctl-kern+ +kern-clockrate+) clockinfo '(:struct clockinfo))
    (flet ((conv (x) (/ x (float (clockinfo-hz clockinfo) 0.0d0))))
      (let ((values (cond
                      ((eq 't core) (cpu-time))
                      ((>= core (machine-cores)) (fail "No such core."))
                      (t (core-time core)))))
        (destructuring-bind (user nice sys spin intr idle) (coerce values 'list)
          (values (conv idle)
                  (conv (+ user nice sys spin intr idle))))))))

;;;; Reference:
;;;; https://github.com/openbsd/src/blob/master/sys/sys/sensors.h
;;;; https://github.com/openbsd/src/blob/master/sbin/sysctl/sysctl.c#L2554

(defconstant +sensor-type-volts-dc+ 2)
(defconstant +sensor-type-amphour+ 8)
(defconstant +sensor-type-integer+ 10)

(cffi:defcstruct (sensor :size 68 :conc-name sensor-)
  (value :int64 :offset 44))

(defconstant +sensor-name-size+ 16)
(cffi:defcstruct (sensordev :size 116 :conc-name sensordev-)
  (name (:array :char #.+sensor-name-size+) :offset 4)) ;; xname

(defconstant +enoent+ 2)
(defconstant +enxio+ 6)

(defun find-sensor-number (name &optional (dev 0))
  (cffi:with-foreign-object (sensordev '(:struct sensordev))
    (let ((ret (sysctl-unchecked (list +ctl-hw+ +hw-sensors+ dev) sensordev (cffi:foreign-type-size '(:struct sensordev)))))
      (when (= -1 ret)
        (return-from find-sensor-number
          (if (= +enxio+ (errno))
              (find-sensor-number name (1+ dev))
              nil)))
      (let ((sensor-name (cffi:foreign-slot-pointer sensordev '(:struct sensordev) 'name)))
        (if (strncmp-lisp sensor-name name :max-chars +sensor-name-size+)
            dev
            (find-sensor-number name (1+ dev)))))))

(defun find-sensor-value (device sensor-type sensor-index)
  (with-sysctl ((+ctl-hw+ +hw-sensors+ device sensor-type sensor-index) sensor '(:struct sensor))
    (sensor-value sensor)))

(define-implementation machine-battery ()
  (let ((battery-n (find-sensor-number "acpibat0")))
    (if battery-n
        (let ((last-full-capacity (find-sensor-value battery-n +sensor-type-amphour+ 0))
              (remaining-capacity (find-sensor-value battery-n +sensor-type-amphour+ 3))
              (state (find-sensor-value battery-n +sensor-type-integer+ 0)))
          (values (float remaining-capacity 0.0d0)
                  (float last-full-capacity 0.0d0)
                  (case state
                    (0 :full)
                    (1 :discharging)
                    (2 :charging))))
        (values 0.0d0 0.0d0 nil))))

(define-implementation machine-info ()
  (values
   (sysctl-string (+ctl-hw+ +hw-vendor+) 128)
   (sysctl-string (+ctl-hw+ +hw-product+) 128)
   :openbsd
   (sysctl-string (+ctl-kern+ +kern-osrelease+) 16)))

(define-implementation machine-core-info ()
  (let ((processor (sysctl-string (+ctl-hw+ +hw-model+) 128)))
    (values processor
            processor ;; There doesn't seem to be a separation between those
            (arch-type)
            (sysctl-string (+ctl-hw+ +hw-machine+) 32))))

(cffi:defcstruct (stat :size #+32-bit 108
                             #+64-bit 128
                       :conc-name stat-)
  (mode    :int :offset 0) ;; st_mode
  (dev     :int :offset 4)) ;; st_dev

(defconstant +mfsnamelen+ 16)
(defconstant +mnamelen+ 90)

#+32-bit
(cffi:defcstruct (statfs :size 564 :conc-name statfs-)
  (block-size :uint32 :offset 4)
  (blocks :uint64 :offset 12)
  (available-blocks :int64 :offset 28) ;; Blocks available to non-superuser
  (synchronous-writes :uint64 :offset 60)
  (synchronous-reads :uint64 :offset 68)
  (asynchronous-writes :uint64 :offset 76)
  (asynchronous-reads :uint64 :offset 84)
  (mountpoint (:array :char #.+mnamelen+) :offset 132)
  (device (:array :char #.+mnamelen+) :offset 222))

#+64-bit
(cffi:defcstruct (statfs :size 568 :conc-name statfs-)
  (block-size :uint32 :offset 4)
  (blocks :uint64 :offset 16)
  (available-blocks :int64 :offset 32) ;; Blocks available to non-superuser
  (synchronous-writes :uint64 :offset 64)
  (synchronous-reads :uint64 :offset 72)
  (asynchronous-writes :uint64 :offset 80)
  (asynchronous-reads :uint64 :offset 88)
  (mountpoint (:array :char #.+mnamelen+) :offset 136)
  (device (:array :char #.+mnamelen+) :offset 226))

(define-implementation storage-device (path)
  (let ((mount-root (pathname-utils:native-namestring (pathname-force-file (find-mount-root path)))))
    (do-filesystems (fs)
      (let ((fs-mountpoint (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint)))
        (when (strncmp-lisp fs-mountpoint mount-root :max-chars +mnamelen+)
          (return (pathname-name
                   (cffi:foreign-string-to-lisp
                    (cffi:foreign-slot-pointer fs '(:struct statfs) 'device) :max-chars +mnamelen+))))))))

(define-implementation storage-device-path (device)
  (let ((device-path (pathname-utils:native-namestring (make-pathname :defaults #P"/dev/" :name device))))
    (do-filesystems (fs)
      (let ((fs-device (cffi:foreign-slot-pointer fs '(:struct statfs) 'device)))
        (when (strncmp-lisp fs-device device-path :max-chars +mnamelen+)
          (return (pathname-utils:parse-native-namestring
                   (cffi:foreign-string-to-lisp
                    (cffi:foreign-slot-pointer fs '(:struct statfs) 'mountpoint) :max-chars +mnamelen+))))))))

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
  (sysctl-string (+ctl-kern+ +kern-hostname+) 255))

;;;; network-io-bytes is unsupported
