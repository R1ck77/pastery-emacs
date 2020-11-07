
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic utility functions

(defun ersatz-storage-to-json ()
  "Returns a JSON representation of the whole storage"
  (ersatz-pastes-to-json (--map (car it) ersatz-storage)))

(defun ersatz-path-surrounded-with-/? (path)
  (let ((characters (string-to-list path)))
   (and (= ?/ (car characters))
        (= ?/ (car (last characters))))))

(defun ersatz-remove-first-last-characters (path)
  (substring path 1 (1- (length path))))

(defun ersatz-get-path-components (path)
  "Returns a list with the path components or nil if the path is not valid"
  (when (ersatz-path-surrounded-with-/? path)
    (let* ((path-without-trailing-/ (ersatz-remove-first-last-characters path))
           (path-components (split-string path-without-trailing-/ "/"))
           (interesting-components (-take 3 path-components)))
      (when (equal '("api" "paste") (-take 2 interesting-components))
        interesting-components))))

(defun ersatz-get-paste-id (path)
  "Returns nil if the path is invalid, or string with the ID (empty string meaning \"no ID present\")"
  (when-let ((path-components (ersatz-get-path-components path)))
    (or (elt path-components 2) "")))

(defun ersatz-create-json-error (message)
  (let ((map (make-hash-table)))
    (puthash "result" "error" map)
    (puthash "error_msg" message map)
    (json-encode map)))

;; Keys validation
(defun ersatz-replace-in-string (what with in)
  "SO for the win!"
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun ersatz-alist-to-hash-table (alist)
  (let ((table (make-hash-table)))
    (--each alist (puthash (car it) (cadr it) table))
    table))

(defun ersatz-double-quotes-to-quotes (string)
  (ersatz-replace-in-string "\"" "'" string))

(defun ersatz-unexpected-keys-message (headers header-keys unexpected-keys)
  (let ((keys-json-repr (json-encode
                         (ersatz-alist-to-hash-table
                          (--map (list it (vector (cadr (assoc it headers)))) header-keys))))
        (unexp-keys-formatted (-reduce-from #'concat "" (-map #'ersatz-double-quotes-to-quotes (-map #'json-encode (-sort #'string< unexpected-keys))))))
    (format "Wrong keys %s in %s"
            unexp-keys-formatted
            (ersatz-double-quotes-to-quotes keys-json-repr))))

(defun ersatz-list-to-set (values)
  "Convert a list to a set-like hash table"
  (let ((table (make-hash-table :test 'equal)))
    (--each values (puthash it t table))
    table))

(defun ersatz-get-unexpected-keys (header-keys valid-keys)
  "Return an error similar to pastery's one if the header contains extra keys that are not allowed"
  (let ((valid-keys-table (ersatz-list-to-set valid-keys)))
    (--filter (null (gethash it valid-keys-table)) header-keys)))

(defun ersatz-validate-key-names (headers valid-keys)
  "Return nil if no error is detected or a cons cell if extra keys where found

The cons cell is in the form (422 . error_message)"
  (let* ((header-keys (-filter #'stringp (-map #'car headers)))
        (unexpected-keys (ersatz-get-unexpected-keys header-keys valid-keys)))
    (and unexpected-keys
         (cons 422 (ersatz-create-json-error (ersatz-unexpected-keys-message headers header-keys unexpected-keys))))))

(provide 'ersatz-utils)
