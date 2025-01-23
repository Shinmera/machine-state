(in-package #:org.shirakumo.machine-state)

(defmacro sysctl (prop type &body body)
  `(cffi:with-foreign-objects ((ret ',type)
                               (size :size))
     (setf (cffi:mem-ref size :size) (cffi:foreign-type-size ',type))
     (let ((status (cffi:foreign-funcall "sysctlbyname" :string ,prop :pointer ret :pointer size :pointer (cffi:null-pointer) :size 0 :int)))
       (cond ((/= 0 status)
              (fail (cffi:foreign-funcall "strerror" :int64 status :string)))
             (T ,@body)))))

#++
(define-implementation process-io-bytes ()
  )

#++
(define-implementation process-room ()
  )

(cffi:defcstruct (vm-statistics :conc-name vm-statistics-)
  (free-count :uint32)
  (active-count :uint32)
  (inactive-count :uint32)
  (wire-count :uint32)
  (zero-fill-count :uint64)
  (reactivations :uint64)
  (page-ins :uint64)
  (page-outs :uint64)
  (faults :uint64)
  (cow-faults :uint64)
  (lookups :uint64)
  (hits :uint64)
  (purges :uint64)
  (purgeable-count :uint32)
  (speculative-count :uint32)
  (decompressions :uint64)
  (compressions :uint64)
  (swap-ins :uint64)
  (swap-outs :uint64)
  (compressor-page-count :uint32)
  (throttled-count :uint32)
  (external-page-count :uint32)
  (internal-page-count :uint32)
  (total-uncompressed-pages-in-compressor :uint64))

(define-implementation machine-room ()
  (cffi:with-foreign-objects ((stats '(:struct vm-statistics))
                              (count :uint))
    (setf (cffi:mem-ref count :uint)
          (/ (cffi:foreign-type-size '(:struct vm-statistics))
             (cffi:foreign-type-size :int32)))
    (cond ((/= 0 (cffi:foreign-funcall "host_statistics64"
                                       :size (cffi:foreign-funcall "mach_host_self" :size)
                                       :int 4 ; HOST_VM_INFO64
                                       :pointer stats
                                       :pointer count
                                       :int))
           (fail "Failed to retrieve host statistics"))
          (T
           (let* ((free-pages (vm-statistics-free-count stats))
                  (free (* (cffi:foreign-funcall "getpagesize" :int) free-pages))
                  (total (sysctl "hw.memsize" :int64 (cffi:mem-ref ret :int64))))
             (values (- total free) total))))))

(define-implementation machine-uptime ()
  (sysctl "kern.boottime" (:struct timeval)
    (- (- (get-universal-time)
          (encode-universal-time 0 0 0 1 1 1970 0))
       (timeval-sec ret))))

(define-implementation machine-cores ()
  (sysctl "hw.ncpu" :uint (cffi:mem-ref ret :uint)))

#++
(define-implementation storage-device (path)
  )

#++
(define-implementation storage-device-path (path)
  )

(cffi:defcstruct (statvfs :size 64 :conc-name statvfs-)
  (bsize    :uint64 :offset  0)
  (frsize   :uint64 :offset  8)
  (blocks   :uint32 :offset 16)
  (bfree    :uint32 :offset 20)
  (bavail   :uint32 :offset 24)
  (files    :uint32 :offset 28)
  (ffree    :uint32 :offset 32)
  (favail   :uint32 :offset 36)
  (fsid     :uint64 :offset 40)
  (flag     :uint64 :offset 48)
  (namemax  :uint64 :offset 56))

(define-implementation storage-room (path)
  (when (stringp path)
    (setf path (storage-device-path path)))
  (cffi:with-foreign-objects ((statvfs '(:struct statvfs)))
    (posix-call "statvfs" :string (pathname-utils:native-namestring path) :pointer statvfs :int)
    (values (* (statvfs-bavail statvfs)
               (statvfs-bsize statvfs))
            (* (statvfs-blocks statvfs)
               (statvfs-bsize statvfs)))))

#++
(define-implementation storage-io-bytes (device)
  (when (pathnamep device)
    (setf device (storage-device device)))
  )

#++
(define-implementation network-io-bytes (device)
  )
