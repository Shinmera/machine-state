(in-package #:org.shirakumo.machine-state)

(defun gl-vendor ()
  (let ((vendor (gl:get-string :vendor)))
    (cond ((search "Intel" vendor) :intel)
          ((search "NVIDIA" vendor) :nvidia)
          ((search "ATI" vendor) :amd)
          ((search "AMD" vendor) :amd)
          (T :unknown))))

(declaim (notinline gpu-room))
(define-implementation gpu-room ()
  (macrolet ((jit (thing)
               `(multiple-value-prog1 ,thing
                  (compile 'gpu-room '(lambda () ,thing)))))
    (case (gl-vendor)
      ;; https://www.khronos.org/registry/OpenGL/extensions/ATI/ATI_meminfo.txt
      (:amd
       (jit (let* ((vbo-free-memory-ati (gl:get-integer #x87FB 4))
                   (tex-free-memory-ati (gl:get-integer #x87FC 4))
                   (buf-free-memory-ati (gl:get-integer #x87FD 4))
                   (total (+ (aref vbo-free-memory-ati 0)
                             (aref tex-free-memory-ati 0)
                             (aref buf-free-memory-ati 0))))
              (values (* 1024 total) (* 1024 total)))))
      ;; http://developer.download.nvidia.com/opengl/specs/GL_NVX_gpu_memory_info.txt
      (:nvidia
       (jit (let ((vidmem-total (gl:get-integer #x9047 1))
                  (vidmem-free  (gl:get-integer #x9049 1)))
              (values (* 1024 vidmem-free)
                      (* 1024 vidmem-total)))))
      (:intel
       (jit (gc-room)))
      (T (jit (values 0 0))))))

(let ((+gpu-time-query-object+ NIL)
      (+gpu-time+ 0))
  (define-implementation gpu-time ()
    (cond (+gpu-time-query-object+
           (handler-case
               (progn
                 (gl:end-query :time-elapsed)
                 (incf +gpu-time+ (gl:get-query-object +gpu-time-query-object+ :query-result)))
             (gl:opengl-error ()
               (setf +gpu-time-query-object+ (first (gl:gen-queries 1))))))
          (T
           (setf +gpu-time-query-object+ (first (gl:gen-queries 1)))))
    (gl:begin-query :time-elapsed +gpu-time-query-object+)
    (* (float +gpu-time+ 0d0) 1e-9)))

(define-implementation gpu-info ()
  (values (let ((vendor (gl:get-string :vendor)))
            (cond ((search "intel" vendor :test #'char-equal) :intel)
                  ((search "nvidia" vendor :test #'char-equal) :nvidia)
                  ((search "ati" vendor :test #'char-equal) :amd)
                  ((search "amd" vendor :test #'char-equal) :amd)))
          (gl:get-string :renderer)
          (gl:get-string :version)))
