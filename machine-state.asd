(asdf:defsystem machine-state
  :version "1.0.0"
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
               (:file "posix" :if-feature (:or :posix :linux :darwin :bsd))
               (:file "linux" :if-feature :linux)
               (:file "nx" :if-feature :nx)
               (:file "mezzano" :if-feature :mezzano)
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:documentation-utils
               :cffi
               :bordeaux-threads
               (:feature :windows :com-on))
  :in-order-to ((asdf:test-op (asdf:test-op :machine-state-test))))

(asdf:defsystem machine-state/opengl
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Additions for GPU state information using OpenGL"
  :components ((:file "opengl"))
  :depends-on (:machine-state :cl-opengl))
