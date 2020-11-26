(require 'dash)

(defvar ersatz-connection-buffers (make-hash-table))

(defun ersatz-clean-connection-buffer! ()
  (--each (--filter (eq 'closed (process-status it))
                    (hash-table-keys ersatz-connection-buffers))
    (progn
      (remhash it ersatz-connection-buffers))))

(defun ersatz-received-size (process)
  (--reduce-from (+ (length it) acc) 0 (gethash process ersatz-connection-buffers)))

(defun ersatz-continue-callback (expected-size answer-arguments process string) ;;; TODO/FIXME pass a curried answer constructor instead? :)
  (ersatz-clean-connection-buffer!)
  (let ((current-data (gethash process ersatz-connection-buffers)))
    (push string current-data)
    (puthash process current-data ersatz-connection-buffers)
    (when (>= (ersatz-received-size process) expected-size)
      (let* ((content (apply #'concat (reverse current-data)))
             (new-id (ersatz-paste-from-arguments! (append (list :body content) answer-arguments))))
        (ersatz-send-answer (new-server-answer :message (ersatz-paste-json-from-storage new-id)) process)))))

(provide 'ersatz-continue-handling)
