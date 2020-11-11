(require 'ersatz-pastery)

(defconst debug-keys '("key1" "key2"))

(defmacro with-debug-server (&rest forms)
  (let ((result (make-symbol "result")))
    `(progn
       (start-ersatz-server debug-keys t)
       (let ((,result (progn ,@forms)))
         (stop-ersatz-server)
         ,result))))

(defun ersatz-debug--delete-pastes ()
  (setq ersatz-storage nil))

(defun ersatz-debug--set-pastes (&rest pastes)
  (setq ersatz-storage pastes))

(provide 'ersatz-pastery-debug)
