(in-package #:org.shirakumo.machine-state)

(include "errno.h")
(include "fcntl.h")
(include "kvm.h")
(include "limits.h")
(include "sys/mount.h")
(include "sys/sched.h")
(include "sys/stat.h")
(include "sys/sysctl.h")
(include "sys/time.h")
(include "sys/types.h")
(include "uvm/uvmexp.h")

(cvar ("errno" errno) :int)
(constant (+posix2-line-max+ "_POSIX2_LINE_MAX") :type integer)

(constant (+cpustates+ "CPUSTATES") :type integer)
(constantenum :cpustate
  ((:user "CP_USER"))
  ((:nice "CP_NICE"))
  ((:sys "CP_SYS"))
  ((:intr "CP_INTR"))
  ((:idle "CP_IDLE")))

(constantenum :sysctl
  ((:ctl-kern "CTL_KERN"))
  ((:kern-osrelease "KERN_OSRELEASE"))
  ((:kern-clockrate "KERN_CLOCKRATE"))
  ((:kern-boottime "KERN_BOOTTIME"))
  ((:kern-cptime "KERN_CPTIME"))
  ((:kern-cptime2 "KERN_CPTIME2"))
  ((:kern-proc "KERN_PROC"))

  ((:ctl-vm "CTL_VM"))
  ((:vm-uvmexp "VM_UVMEXP"))

  ((:ctl-hw "CTL_HW"))
  ((:hw-diskstats "HW_DISKSTATS"))
  ((:hw-diskcount "HW_DISKCOUNT"))
  ((:hw-physmem64 "HW_PHYSMEM64"))
  ((:hw-ncpuonline "HW_NCPUONLINE")))

(cstruct kinfo-file "struct kinfo_file"
  (read-bytes "f_rbytes" :type :uint64)
  (written-bytes "f_wbytes" :type :uint64))

(cstruct kinfo-proc "struct kinfo_proc"
  (nice "p_nice" :type :uint8)
  (resident-set-size "p_vm_rssize" :type :int32)
  (user-time-seconds "p_uutime_sec" :type :uint32)
  (user-time-microseconds "p_uutime_usec" :type :uint32))

(constantenum :kern-file
  ((:kern-file-byfile "KERN_FILE_BYFILE"))
  ((:kern-file-bypid "KERN_FILE_BYPID"))
  ((:kern-file-byuid "KERN_FILE_BYUID")))

(constantenum :kern-proc
  ((:kern-proc-all "KERN_PROC_ALL"))
  ((:kern-proc-pid "KERN_PROC_PID"))
  ((:kern-proc-pgrp "KERN_PROC_PGRP"))
  ((:kern-proc-session "KERN_PROC_SESSION"))
  ((:kern-proc-tty "KERN_PROC_TTY")))

(constantenum :prio
  ((:prio-process "PRIO_PROCESS"))
  ((:prio-pgrp "PRIO_PGRP"))
  ((:prio-user "PRIO_USER")))

(cstruct uvmexp "struct uvmexp"
  (pagesize "pagesize" :type :int)
  (npages "npages" :type :int)
  (free "free" :type :int)
  (inactive "inactive" :type :int))

(ctype :time "time_t")
(ctype :suseconds "suseconds_t")
(cstruct timeval "struct timeval"
  (sec "tv_sec" :type :time)
  (usec "tv_usec" :type :suseconds))

(cstruct clockinfo "struct clockinfo"
  (hz "hz" :type :int))

(ctype :mode "mode_t")
(ctype :dev "dev_t")
(cstruct stat "struct stat"
  (mode "st_mode" :type :mode)
  (dev "st_dev" :type :dev))

(constant (+mfsnamelen+ "MFSNAMELEN") :type integer)
(constant (+mnamelen+ "MNAMELEN") :type integer)
(cstruct statfs "struct statfs"
  (block-size "f_bsize" :type :uint32)
  (blocks "f_blocks" :type :uint64)
  (free-blocks "f_bfree" :type :uint64) ;; All free blocks
  (available-blocks "f_bavail" :type :int64) ;; Blocks available to non-superuser
  (synchronous-writes "f_syncwrites" :type :uint64)
  (synchronous-reads "f_syncreads" :type :uint64)
  (asynchronous-writes "f_asyncwrites" :type :uint64)
  (asynchronous-reads "f_asyncreads" :type :uint64)
  (max-filename-size "f_namemax" :type :uint32)
  (filesystem-type "f_fstypename" :type (:array :char #.+mfsnamelen+))
  (mountpoint "f_mntonname" :type (:array :char #.+mnamelen+))
  (device "f_mntfromname" :type (:array :char #.+mnamelen+)))

(constantenum :mnt
  ((:wait "MNT_WAIT"))
  ((:nowait "MNT_NOWAIT"))
  ((:lazy "MNT_LAZY")))
