(require 'dash)

(defvar ersatz-connection-buffers (make-hash-table))

(defun ersatz-clean-connection-buffer! ()
  (--each (--filter (eq 'closed (process-status it))
                    (hash-table-keys ersatz-connection-buffers))
    (progn
      (remhash it ersatz-connection-buffers))))

(defun ersatz-received-size (process)
  (--reduce-from (+ (length it) acc) 0 (gethash process ersatz-connection-buffers)))

(defun ersatz-continue-callback (expected-size send-answer-f process string)
  (ersatz-clean-connection-buffer!)
  (let ((current-data (gethash process ersatz-connection-buffers)))
    (push string current-data)
    (puthash process current-data ersatz-connection-buffers)
    (when (>= (ersatz-received-size process) expected-size)
      (funcall send-answer-f process (apply #'concat (reverse current-data))))))

(provide 'ersatz-continue-handling)
