(require 'web-server)
(require 'cl)
(require 'json)
(eval-when-compile (require 'subr-x))
(require 'ersatz-constants)
(require 'ersatz-paste)
(require 'ersatz-utils)
(require 'ersatz-continue-handling)
(require 'pastery-consts)

;;; TODO/FIXME wrong permissions! The pastes should be stored by key!!!
;;; TODO/FIXME just for curiosity, try passing "ersatz-storage" everywhere instead of having it a global

(defvar ersatz-valid-keys nil
  "Keys accepted by the current server")

(defvar ersatz-storage '())

(cl-defstruct (ersatz-answer (:constructor new-server-answer))
  (keep-alive nil :read-only t)
  (HTTP-code HTTP-ok :read-only t)
  (message "" :read-only t))

(defun ersatz-send-answer (answer process)
  (if (ersatz-answer-keep-alive answer)
      (prog1 :keep-alive
        (ws-response-header process 100))
    (let ((message (ersatz-answer-message answer)))
      (ws-response-header process (ersatz-answer-HTTP-code answer)
                          (cons "Content Type" "application/json")
                          (cons "Content-Length" (number-to-string (length message))))
      (process-send-string process message))))

(defun ersatz-remove-from-storage! (value-predicate &optional message-template)
  "Remove from storage all paste matching the predicate.

message-template, if provided, will write a message passing the paste id as an argument"
  (--each (--filter (funcall value-predicate (cdr it)) ersatz-storage)
    (let ((paste-id (car it)))
      (and message-template (message message-template paste-id))
      (setq ersatz-storage (assoc-delete-all paste-id ersatz-storage)))))

(defun ersatz-clean-storage! ()
  "Remove all pastes that are overdue"
  (ersatz-remove-from-storage! (lambda (paste)
                                (<= (or (paste-max_views paste) 1) 0))
                              "Deleting paste '%s' due to view limit exceeded")
  (ersatz-remove-from-storage! #'ersatz-paste-overdue?
                              "Deleting paste '%s' due to duration exceeded"))

;;;;;;;;
;;; POST
(defun ersatz-create-paste-id ()
  "Take the last 6 characters of the integer UNIX time"
  (let ((timestamp-as-string (number-to-string (truncate (time-to-seconds (current-time))))))
    (substring (reverse timestamp-as-string)
               0 6)))

(cl-defstruct (ersatz-operation-result (:constructor new-ersatz-operation-result))
  (value nil :read-only t)
  (error-message nil :read-only t))

(defun ersatz-extract-max-views (headers)
  "Returns the result of parsing max views from the headers"
  (if-let ((max-views (cdr (assoc pastery-max-views-key headers))))
      (let ((converted (ersatz-to-integer max-views)))
        (if converted
            (new-ersatz-operation-result :value (unless (zerop converted)
                                                    converted))
          (new-ersatz-operation-result :error-message "\"max_views\" should be a non-negative integer number of views before the paste is deleted.")))
    (new-ersatz-operation-result :value nil)))

;;; TODO/FIXME too complex
(defun ersatz-add-max-views (headers arguments)
  "Return an ersatz-operation-result with as value the arguments updated with the max_views value.

An error operation-result is return if the max_value parsing fails"
  (let ((result (ersatz-extract-max-views headers)))
    (if (ersatz-operation-result-error-message result)
        result
      (let ((max-views-argument (list :max_views (ersatz-operation-result-value result))))
        (new-ersatz-operation-result :value (append max-views-argument arguments))))))

(defun ersatz-add-duration-or-string (headers arguments)
  "Return the duration specified by the user as an integer, or a string if the value is not a valid non-negative integer"
  (if-let ((duration (cdr (assoc pastery-duration-key headers))))
      (let ((converted (ersatz-to-integer duration)))
        (if (not converted)
            "\"duration\" should be a positive integer number of minutes before the paste is deleted."
          (append (list :initial-duration converted) arguments)))
    arguments))

(defun ersatz-validate-language (user-specified-language)
  "Return the language specified by the user if valid, or \"text\""
  (or (find user-specified-language valid-languages :test 'equal)
      "text"))

(defun ersatz-add-language (headers arguments)
  "Return the arguments with the language specified by the user, or text if none"
  (let ((user-specified-language (cdr (assoc pastery-language-key headers))))
    (append (list :language (ersatz-validate-language user-specified-language))
            arguments)))

(defun ersatz-add-title (headers arguments)
  "Return a new set of arguements with the title specified by the user"
  (if-let ((title (cdr (assoc pastery-title-key headers))))
      (append (list :title title) arguments)
    arguments))

(defun ersatz-create-paste-arguments (headers)
  "Extract the arguments from the headers, returns an ersatz-operation-result with the list of arguments"
  (let ((partial-headers-or-error (ersatz-add-duration-or-string
                                   headers
                                   (ersatz-add-language
                                    headers
                                    (ersatz-add-title headers nil)))))
    (if (stringp partial-headers-or-error)
        partial-headers-or-error
      (ersatz-add-max-views headers partial-headers-or-error))))

(defun ersatz-paste-from-arguments! (arguments)
  (let* ((paste (apply #'new-paste arguments))
         (id (ersatz-create-paste-id))
         (alist-element (cons id paste)))
    (setq ersatz-storage (cons alist-element ersatz-storage))
    id))

(defun ersatz-paste-json-from-storage (id)
  (ersatz-paste-to-json id (cdr (assoc id ersatz-storage))))

(defun ersatz-100-continue? (headers)
  (string= "100-continue"
           (downcase
            (or (cdr (assoc :EXPECT headers)) ""))))

(defun ersatz-handle-post-small-body ())

;;; TODO/FIXME should I need to add a \n at the end?
(defun ersatz-extract-post-body (headers)
  "Return the content of the POST message body"
  (caar (last headers)))

(defun ersatz-get-arguments-with-paste-body (headers arguments)
  (let ((body (ersatz-extract-post-body headers)))
    (append (list :body body) arguments)))

(defun ersatz-get-content-length (headers)
  (string-to-number (cdr (assoc :CONTENT-LENGTH headers))))

(defun ersatz-handle-post-valid-arguments (headers arguments)
  (if (ersatz-100-continue? headers)
      (prog1 (new-server-answer :keep-alive t)
        (set-process-filter process
                            (-partial #'ersatz-continue-callback
                                      (ersatz-get-content-length headers)
                                      arguments)))
    (let ((arguments-with-body (ersatz-get-arguments-with-paste-body headers arguments)))
      (new-server-answer :message (ersatz-paste-json-from-storage (ersatz-paste-from-arguments! arguments-with-body))))))

(defun ersatz-handle-post-argument-parsing-error (error-message)
  (new-server-answer :HTTP-code HTTP-unprocessable-entity
                     :message (ersatz-create-json-error error-message)))

(defun ersatz-handle-post (process headers)
  (if-let ((argument-parsing-result (ersatz-create-paste-arguments headers))
           (error-message (ersatz-operation-result-error-message argument-parsing-result)))
      (ersatz-handle-post-argument-parsing-error error-message)
    (ersatz-handle-post-valid-arguments headers (ersatz-operation-result-value argument-parsing-result))))

;;;;;;;
;;; GET
(defun ersatz-handle-get-paste! (id)
  "Answer with the paste, if it exists.

Decrement the max-views count"
  (let ((result (ersatz-pastes-to-json (list id))))
    (if-let ((paste (cdr (assoc id ersatz-storage))))
        (when (paste-max_views paste)
          (setq ersatz-storage (assoc-delete-all id ersatz-storage))
          (push (cons id (ersatz-paste-with-decremented-view paste)) ersatz-storage)))
    result))

(defun ersatz-handle-get-list ()
  "Answer with a list of the pastes.

There is a bug/curious feature in the original server where listing the pastes will decrement the 'views' counter, this implementation will not do it."
  (ersatz-storage-to-json))

(defun ersatz-handle-get! (path headers)
  (if-let (id (ersatz-get-paste-id path))
      (if (string-empty-p id)
          (ersatz-handle-get-list)
        (ersatz-handle-get-paste! id))
      ;;; TODO/FIXME handle this 301 no content
    (error "INVALID SOMETHING. Handle this")))

;;;;;;;;;;
;;; DELETE
(defun ersatz-delete-paste! (id)
  (let ((paste (cdr (assoc id ersatz-storage))))
    (if (not paste)
        "{\"result\": \"error\", \"error_msg\": \"That paste does not belong to you.\"}"
      (setq ersatz-storage (assoc-delete-all id ersatz-storage))
      "{\"result\": \"success\"}")))

(defun ersatz-handle-delete! (path)
  ;;; TODO/FIXME handle the 301 case
  (ersatz-delete-paste! (ersatz-get-paste-id path)))

(defun read-sample (name)
  (with-temp-buffer
    (insert-file-contents (format "test-data/%s" name))
    (forward-line)
    (buffer-substring-no-properties (point) (point-max))))

;;; TODO/FIXME duplicated code with the path splitting function
(defun ersatz-get-path-error (path)
  "Returns nil if the path 'looks ok' or (301 . '') if it does not"
  (let ((components (split-string path "/")))
    (unless
        (and
         (string= "" (car components))
         (string= "api" (elt components 1))
         (string= "paste" (elt components 2))
         (string= "" (car (last components)))
         (<= (length components) 5))
      (new-server-answer :HTTP-code HTTP-moved-permanently))))

(defun ersatz-get-api-key-error (headers)
  (let ((key (assoc pastery-api-key headers)))
    (if (not key)
        (new-server-answer :HTTP-code HTTP-unprocessable-entity
                           :message (ersatz-create-json-error "Missing keys: 'api_key'"))
      (unless (find (cdr key) ersatz-valid-keys :test 'equal )
        (new-server-answer :HTTP-code HTTP-unprocessable-entity
                           :message (ersatz-create-json-error (format "%S must be a valid API key." pastery-api-key)))))))

(defun ersatz-create-response (process headers)
  (or       
   (let ((get-path (alist-get ':GET headers)))
     (and get-path
          (or (ersatz-get-api-key-error headers)
              (ersatz-get-path-error get-path)
              (ersatz-validate-key-names headers (list pastery-api-key))
              (new-server-answer :message (ersatz-handle-get! get-path headers)))))
   (let ((delete-path (alist-get ':DELETE headers)))
     (and delete-path
          (or (ersatz-get-api-key-error headers)
              (ersatz-get-path-error delete-path)
              (ersatz-validate-key-names headers (list pastery-api-key))
              (new-server-answer :message (ersatz-handle-delete! delete-path)))))
   (let ((post-path (alist-get ':POST headers)))
     (and post-path
          (or (ersatz-get-api-key-error headers)
              (ersatz-get-path-error post-path)
              (ersatz-validate-key-names headers (list pastery-api-key
                                                       pastery-title-key
                                                       pastery-language-key
                                                       pastery-duration-key
                                                       pastery-max-views-key))
              (ersatz-handle-post process headers))))
   (new-server-answer :HTTP-code HTTP-moved-permanently)))

(defun ersatz-handle-request (process headers send-response-f)
  (funcall send-response-f (ersatz-create-response process headers)))

(defun ersatz-pastery-handler (request)
  (ersatz-clean-storage!)
  (with-slots (process headers) request
    (ersatz-handle-request process
                           headers
                           (lambda (answer)
                             (ersatz-send-answer answer process)))))

(defun stop-ersatz-server ()
  (interactive)
  (ws-stop-all))

(defun start-ersatz-server (valid-keys &optional try-kill)
  (interactive)
  (setq ersatz-valid-keys valid-keys)
  (when try-kill
    (stop-ersatz-server))
  (setq ersatz-storage '())
  (ws-start #'ersatz-pastery-handler ersatz-pastery-server-port))

(provide 'ersatz-pastery)
