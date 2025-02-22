(asdf:defsystem machine-state
  :version "1.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Retrieve machine state information about CPU time, memory usage, etc."
  :homepage "https://shinmera.github.io/machine-state/"
  :bug-tracker "https://github.com/shinmera/machine-state/issues"
  :source-control (:git "https://github.com/shinmera/machine-state.git")
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "windows" :if-feature (:or :windows :win32))
               (:file "posix" :if-feature (:and (:not :openbsd)
                                                (:or :posix :linux :darwin :bsd)))
               (:file "darwin" :if-feature :darwin)
               (:file "bsd" :if-feature :bsd)
               (:file "freebsd" :if-feature :freebsd)
               (:file "openbsd" :if-feature :openbsd)
               (:file "linux" :if-feature :linux)
               (:file "nx" :if-feature :nx)
               (:file "mezzano" :if-feature :mezzano)
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:documentation-utils
               (:feature (:not :mezzano) :cffi)
               ;; 32bit SBCL does not support threads on OpenBSD
               (:feature (:not (:and :sbcl :openbsd :32-bit)) :bordeaux-threads)
               :pathname-utils
               (:feature :windows :com-on))
  :in-order-to ((asdf:test-op (asdf:test-op :machine-state/test))))

(asdf:defsystem machine-state/opengl
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Additions for GPU state information using OpenGL"
  :components ((:file "opengl"))
  :depends-on (:machine-state :cl-opengl))

(asdf:defsystem machine-state/test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the machine-state library"
  :components ((:file "test"))
  :depends-on (:machine-state :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.machine-state.test)))
