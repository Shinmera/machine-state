(defmethod staple:subsystems ((system (eql (asdf:find-system :machine-state)))) ())
(defmethod staple:packages ((system (eql (asdf:find-system :machine-state)))) (list (find-package :org.shirakumo.machine-state)))
