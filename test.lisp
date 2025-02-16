(defpackage #:org.shirakumo.machine-state.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:machine-state #:org.shirakumo.machine-state))
  (:export))

(in-package #:org.shirakumo.machine-state.test)

(define-test machine-state

;;;; Machine

  (define-test machine-time
    (flet ((test (idle total)
             (of-type 'double-float idle)
             (of-type 'double-float total)
             (is >= 0.0d0 idle)
             (is >= 0.0d0 total)
             (is > idle total)))

      ;; Test the aggregate
      (multiple-value-bind (cpu-idle cpu-total) (machine-state:machine-time t)
        (test cpu-idle cpu-total)

        (let ((nproc (machine-state:machine-cores)))
          (dotimes (core nproc)
            (multiple-value-bind (core-idle core-total) (machine-state:machine-time core)
              ;; Test core against CPU
              (is <= cpu-idle core-idle)
              (is <= cpu-total core-total)

              ;; Test cores
              (test core-idle core-total)))

          (fail (machine-state:machine-time nproc) 'machine-state:query-failed
                "Non existant core expected to fail")))))

  (define-test machine-cores
    (let ((nproc (machine-state:machine-cores)))
      (of-type '(unsigned-byte 64) nproc)
      (is >= 1 nproc)))

  (define-test machine-uptime
    (let ((uptime (machine-state:machine-uptime)))
      (of-type '(unsigned-byte 64) uptime)
      (is >= 0 uptime)))

  (define-test machine-room
    (multiple-value-bind (used total) (machine-state:machine-room)
      (of-type '(unsigned-byte 64) used)
      (of-type '(unsigned-byte 64) total)
      (is >= used total)))

  (define-test machine-info
    (multiple-value-bind (board model os version) (machine-state:machine-info)
      (of-type 'string board)
      (of-type 'string model)
      (is member '(:WINDOWS :LINUX :DARWIN :ANDROID :IOS :NETBSD :FREEBSD
                   :OPENBSD :BEOS :SOLARIS :REACT :PLAN9 :MEZZANO :NX
                   nil)
          os)
      (of-type 'string version)))

  (define-test machine-core-info
    (multiple-value-bind (vendor model arch version) (machine-state:machine-core-info)
      (of-type 'string vendor)
      (of-type 'string model)
      (is member '(:X86 :AMD64 :ARM :ARM64 :RISCV :RISCV64 :PPC :SPARC nil) arch)
      (of-type 'string version)))

  (define-test machine-battery
    (multiple-value-bind (current full state) (machine-state:machine-battery)
      (of-type 'double-float current)
      (of-type 'double-float full)
      (is member '(:CHARGING :DISCHARGING :FULL nil) state)
      (is >= 0.0d0 current)
      (is >= 0.0d0 full)))

;;;; Processes

  (define-test process-io-bytes
    (multiple-value-bind (total r w) (machine-state:process-io-bytes)
      (of-type '(unsigned-byte 64) total)
      (of-type '(unsigned-byte 64) r)
      (of-type '(unsigned-byte 64) w)

      (is = (+ r w) total)
      (is >= r total)
      (is >= w total)))

  (define-test process-time
    (let ((time (machine-state:process-time)))
      (of-type 'double-float time)
      (is >= 0.0d0 time)))

  (define-test process-room
    (let ((room (machine-state:process-room)))
      (of-type '(unsigned-byte 64) room)))

  (define-test process-priority
    (is member '(:IDLE :LOW :NORMAL :HIGH :REALTIME) (machine-state:process-priority)))

  (define-test process-info
    (multiple-value-bind (command cwd user group) (machine-state:process-info)
      (of-type 'pathname command)
      (of-type 'pathname cwd)
      (of-type 'string user)
      (of-type 'string group)))

;;;; Threads

  (define-test thread-time
    (let ((time (machine-state:thread-time t)))
      (of-type 'double-float time)
      (is >= 0.0d0 time)))

  (define-test thread-core-mask
    (let ((mask (machine-state:thread-core-mask t)))
      (of-type '(unsigned-byte 64) mask) ;; Max 64 bits
      (is > 0 mask)))

  (define-test thread-priority
    (is member '(:IDLE :LOW :NORMAL :HIGH :REALTIME) (machine-state:thread-priority t)))

;;;; GC

  (define-test gc-room
    (multiple-value-bind (free total) (machine-state:gc-room)
      (of-type '(unsigned-byte 64) free)
      (of-type '(unsigned-byte 64) total)
      (is <= total free)))

  (define-test gc-time
    (let ((time (machine-state:gc-time)))
      (of-type 'double-float time)
      (is >= 0.0d0 time)))

;;;; GPU

  (define-test gpu-room
    (multiple-value-bind (free total) (machine-state:gpu-room)
      (of-type '(unsigned-byte 64) free)
      (of-type '(unsigned-byte 64) total)
      (is <= total free)))

  (define-test gpu-time
    (let ((time (machine-state:gpu-time)))
      (of-type 'double-float time)
      (is >= 0.0d0 time)))

  (define-test gpu-info
    (multiple-value-bind (vendor model version) (machine-state:gpu-info)
      (of-type 'symbol vendor)
      (of-type 'string model)
      (of-type 'string version)))

;;;; Storage

  (define-test storage-room
    (multiple-value-bind (free total) (machine-state:storage-room *default-pathname-defaults*)
      (of-type '(unsigned-byte 64) free)
      (of-type '(unsigned-byte 64) total)
      (is <= total free)))

  (define-test storage-io-bytes
    (multiple-value-bind (total r w) (machine-state:storage-io-bytes *default-pathname-defaults*)
      (of-type '(unsigned-byte 64) total)
      (of-type '(unsigned-byte 64) r)
      (of-type '(unsigned-byte 64) w)
      (is = (+ r w) total)
      (is >= r total)
      (is >= w total)))

;;;; Network

  (define-test network-devices
    (let ((devices (machine-state:network-devices)))
      (of-type 'list devices)
      (is every devices #'stringp)))

  (define-test network-io-bytes
    (dolist (dev (machine-state:network-devices))
      (multiple-value-bind (total r w) (machine-state:network-io-bytes dev)
        (of-type '(unsigned-byte 64) total)
        (of-type '(unsigned-byte 64) r)
        (of-type '(unsigned-byte 64) w)
        (is = (+ r w) total)
        (is >= r total)
        (is >= w total))))

  (define-test network-address
    (dolist (dev (machine-state:network-devices))
      (multiple-value-bind (mac ipv4 ipv6) (machine-state:network-address dev)
        (of-type '(or null string) mac)
        (of-type '(or null string) ipv4)
        (of-type '(or null string) ipv6))))

  (define-test network-info
    (of-type '(or null string) (machine-state:network-info)))

;;;; Others

  (define-test static-room
    (let ((size (machine-state:static-room)))
      (of-type '(unsigned-byte 64) size)))

  (define-test stack-room
    (multiple-value-bind (free total) (machine-state:stack-room)
      (of-type '(unsigned-byte 64) free)
      (of-type '(unsigned-byte 64) total)
      (is <= total free))))
