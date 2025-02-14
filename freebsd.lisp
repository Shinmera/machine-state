(in-package #:org.shirakumo.machine-state)

(define-implementation machine-room ()
  (with-sysctls (("hw.pagesize" page-size :uint64)
                 ("hw.physmem" physmem :uint64)
                 ("vm.stats.vm.v_free_count" free :uint64))
    (let ((page-size (cffi:mem-ref page-size :uint64))
          (physmem (cffi:mem-ref physmem :uint64))
          (free (cffi:mem-ref free :uint64)))
      (values (- physmem (* free page-size)) physmem))))

(define-implementation machine-uptime ()
  (with-sysctl ("kern.boottime" tv '(:struct timeval))
    (- (get-unix-time) (timeval-sec tv))))
