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
               (:file "windows" :if-feature :windows)
               (:file "posix" :if-feature :posix)
               (:file "linux" :if-feature :linux)
               (:file "documentation"))
  :depends-on (:documentation-utils
               :cffi)
  :in-order-to ((asdf:test-op (asdf:test-op :machine-state-test))))
