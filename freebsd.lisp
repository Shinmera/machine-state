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

(define-implementation machine-cores ()
  (with-sysctl ("hw.ncpu" cores :int)
    (cffi:mem-ref cores :int)))

;;;; https://github.com/freebsd/freebsd-src/blob/main/sys/sys/resource.h#L172
(defconstant +cpustates+ 5)

(defun cpu-time ()
  (with-sysctl ("kern.cp_time" cpustates :uint64 +cpustates+)
    (cffi:mem-ref cpustates `(:array :uint64 ,+cpustates+))))

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

(define-implementation machine-core-info ()
  (let ((processor (sysctl-string "hw.model" 128)))
    (values processor
            processor ;; There doesn't seem to be a separation between those
            (arch-type)
            (sysctl-string "hw.machine" 32))))
