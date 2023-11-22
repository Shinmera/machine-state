(in-package #:org.shirakumo.machine-state)

(docs:define-docs
  (function process-io-bytes
    "Returns the number of bytes of IO performed by the process.

IO in this context refers to any activity to external devices such as
drives, networking, etc.

If the function is unsupported a constant 0 is returned.")
  
  (function process-time
    "Returns the amount of processing time spent by this process in seconds.

This does not include time spent in the kernel.

If the function is unsupported a constant 0.0d0 is returned.

See THREAD-TIME
See GC-TIME
See GPU-TIME")
  
  (function process-room
    "Returns the process' memory usage statistics.

This includes foreign memory usage.

Returns the number of bytes occupied.

If the function is unsupported a constant 0 is returned.

See GC-ROOM
See MACHINE-ROOM")
  
  (function machine-room
    "Returns the machine's primary memory usage statistics.

Returns two values:
  The number of physical bytes occupied
  The total number of physical bytes available

If the function is unsupported a constant 0 is returned for both
values.

See GC-ROOM
See GPU-ROOM
See PROCESS-ROOM")

  (function machine-cores
    "Returns the number of cores available on the machine.")
  
  (function thread-time
    "Returns the amount of processing time spent by this thread in seconds.

This does not include time spent in the kernel.

Thread may be T for the current thread, or a BT:THREAD.

If the function is unsupported a constant 0.0d0 is returned.

See PROCESS-TIME
See GC-TIME
See GPU-TIME")

  (function thread-core-mask
    "Accessor to the CPU core affinity mask of the thread.

The mask is a bitfield where each set bit in the integer designates a
core that the thread may be executed on. For compatibility reasons
only integers up to 64 bits are supported.

Thread may be T for the current thread, or a BT:THREAD.

If the function is unsupported a constant of all 1s is returned.

When setting this place, the *actual* affinity mask of the thread is
returned, which may differ from the one you tried to set.")

  (function gc-room
    "Returns the GC's memory usage statistics.

This does not include foreign memory usage.

Returns two values:
  The number of bytes occupied
  The total number of bytes available

If the function is unsupported a constant 0 is returned for both
values.

See MACHINE-ROOM
See PROCESS-ROOM
See GPU-ROOM")
  
  (function gc-time
    "Returns the amount of processing time spent in the GC.

If the function is unsupported a constant 0.0d0 is returned.

See PROCESS-TIME
See GC-TIME
See THREAD-TIME")
  
  (function gpu-room
    "Returns the GPU's memory usage statistics.

Returns two values:
  The number of bytes occupied
  The total number of bytes available

If the function is unsupported a constant 0 is returned for both
values.

See MACHINE-ROOM
See PROCESS-ROOM
See GC-ROOM")
  
  (function gpu-time
    "Returns the amount of processing time spent on the GPU by this process.

If the function is unsupported a constant 0.0d0 is returned.

See PROCESS-TIME
See GC-TIME
See THREAD-TIME"))
