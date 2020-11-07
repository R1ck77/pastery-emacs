(require 'web-server)
(require 'cl)
(require 'json)
(require 'subr-x) ; (eval-when-compile (require 'subr-x)) could work too
(require 'ersatz-constants)
(require 'ersatz-paste)
(require 'ersatz-utils)

;;; TODO/FIXME
;;; if I specified the wrong keys with any call i get:
;;; 422 - {"result": "error", "error_msg": "Wrong keys 'foobar' in {'api_key': ['R89j54QSLjgL6kM2pbewzGgg5wkFASfS'], 'foobar': ['12']}"}
;;; 422 per missing api_key
;;; 301 per path invalido

(defvar ersatz-valid-keys nil
  "Keys accepted by the current server")

(defvar ersatz-storage '())

;;;;;;;;
;;; POST
(defun ersatz-create-paste-id ()
  "Take the last 6 characters of the integer UNIX time"
  (let ((timestamp-as-string (number-to-string (truncate (time-to-seconds (current-time))))))
    (substring (reverse timestamp-as-string)
               0 6)))

;;; TODO/FIXME isn't there a better way? Sad…
(defun ersatz-to-integer (string)
  "Doesn't work for values like 0100 or 00, good enough for testing"
  (let ((converted (truncate (string-to-number string))))
    (if (and (>= converted 0)
             (equal (number-to-string converted) string))
        converted)))

(defun ersatz-add-duration-or-string (headers arguments)
  "Return the duration specified by the user as an integer, or a string if the value is not a valid non-negative integer"
  (if-let ((duration (cdr (assoc "duration" headers))))
      (let ((converted (ersatz-to-integer (cdr duration))))
        (if (not converted)
            "\"duration\" should be a positive integer number of minutes before the paste is deleted."
          (append (list :duration converted) arguments)))))

(defun ersatz-validate-language (user-specified-language)
  "Return the language specified by the user if valid, or \"text\""
  (or (find user-specified-language valid-languages :test 'equal)
      "text"))

(defun ersatz-add-language (headers arguments)
  "Return the arguments with the language specified by the user, or text if none"
  (let ((user-specified-language (cdr (assoc "language" headers))))
    (append (list :language (ersatz-validate-language user-specified-language))
            arguments)))

(defun ersatz-add-url (headers arguments)
  "Return a new set of arguments with the url specified by the user"
  (if-let ((url (cdr (assoc "url" headers))))
      (append (list :url url) arguments)
    arguments))

(defun ersatz-add-title (headers arguments)
  "Return a new set of arguements with the title specified by the user"
  (if-let ((title (cdr (assoc "title" headers))))
      (append (list :title title) arguments)
    arguments))

(defun ersatz-create-paste-arguments (headers)
  "Extract the arguments from the headers, returns an alist of header values or a string with an error"
  (ersatz-add-duration-or-string
   headers
   (ersatz-add-language
    headers
    (ersatz-add-url
     headers
     (ersatz-add-title headers arguments)))))

(defun ersatz-paste-from-arguments (arguments)
  (let ((paste (apply #'new-paste arguments)))
    ;;; TODO/FIXME cons cons cons cons?
    (setq ersatz-storage (cons (cons (ersatz-create-paste-id) paste) ersatz-storage))
    (ersatz-paste-to-json id paste)))

(defun ersatz-handle-post (path headers) 
  (let ((arguments-or-error (ersatz-create-paste-arguments headers)))
    (if (stringp arguments-or-error)
        (cons 422 (ersatz-create-json-error arguments-or-error))
      (cons 200 (ersatz-paste-from-arguments argument-or-error)))))

;;;;;;;
;;; GET
(defun ersatz-handle-get-paste (id)
  (ersatz-pastes-to-json (list id)))

(defun ersatz-handle-get-list ()
  (ersatz-storage-to-json))

(defun ersatz-handle-get (path headers)
  (if-let (id (ersatz-get-paste-id path))
      (if (string-empty-p id)
          (ersatz-handle-get-list)
        (ersatz-handle-get-paste id))
      ;;; TODO/FIXME handle this 301 no content
    (error "INVALID SOMETHING. Handle this")))

;;;;;;;;;;
;;; DELETE
(defun ersatz-delete-paste (id)
  (let ((paste (cdr (assoc id ersatz-storage))))
    (if (not paste)
        "{\"result\": \"error\", \"error_msg\": \"That paste does not belong to you.\"}"
      (setq ersatz-storage (assoc-delete-all id ersatz-storage))
      "{\"result\": \"success\"}")))

(defun ersatz-handle-delete (path)
  ;;; TODO/FIXME handle the 301 case
  (ersatz-delete-paste (ersatz-get-paste-id path)))

(defun read-sample (name)
  (with-temp-buffer
    (insert-file-contents (format "test-data/%s" name))
    (forward-line)
    (buffer-substring-no-properties (point) (point-max))))

(defun ersatz-get-api-key-error (headers)
  (let ((key (assoc "api_key" headers)))
    (if (not key)
        (read-sample "missing_api_key.txt")
        (if (find key ersatz-valid-keys :test 'equal )
            (read-sample "invalid_api_key.txt")))))

(defun ersatz-handle-request (process headers)
  (or (ersatz-get-api-key-error headers)
      (let ((get-path (alist-get ':GET headers)))
        (cons 200 (and get-path (ersatz-handle-get get-path headers))))
      (let ((delete-path (alist-get ':DELETE headers)))
        (cons 200 (and delete-path (ersatz-handle-delete delete-path))))
      (let ((post-path (alist-get ':POST headers)))
        (and post-path (ersatz-handle-post post-path headers)))))

(defun ersatz-pastery-handler (request)
  (with-slots (process headers) request
    (let* ((result (ersatz-handle-request process headers))
           (http-code (car result))
           (code-content (cdr result)))
      (ws-response-header process http-code
                          '("Content Type" . "application/json")
                          (cons "Content-Length" (number-to-string (length code-content))))
      (process-send-string process code-content))))

(defun stop-ersatz-server ()
  (interactive)
  (ws-stop-all))

(defun start-ersatz-server (valid-keys &optional try-kill)
  (interactive)
  (setq ersatz-valid-keys valid-keys)
  (when try-kill
    (stop-ersatz-server))
  (setq ersatz-storage '())
  (ws-start #'ersatz-pastery-handler 8080))

(provide 'ersatz-pastery)
