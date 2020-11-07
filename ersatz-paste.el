;;; Paste definition

(cl-defstruct (paste (:constructor new-paste))
  (duration 43200 :read-only t)
  (title "" :read-only t)
  (language "text" :read-only t)
  (max_views 0 :read-only t)
  (body "" :read-only t))

(defun ersatz-paste-to-table (id paste)
  (let ((table (make-hash-table)))
    (puthash "id" id table)
    (puthash "title" (paste-title paste) table)
    (puthash "url" (format "https://www.pastery.net/%s/" id) table) ; TODO/FIXME mock part
    (puthash "language" (paste-language paste) table)
    (puthash "duration" (paste-duration paste) table)
    table))

(defun ersatz-paste-to-json (id paste)
  (json-encode (ersatz-paste-to-table id paste)))

(defun ersatz-create-pastes-list (id-list)
  (--map (ersatz-paste-to-table (car it) (cdr it))
         (--filter (cdr it)
                   (--map (assoc it ersatz-storage)
                          id-list))))

(defun ersatz-pastes-to-json (id-list)
  "Returns a JSON representation of the valid pastes in the storage with the specified ID

Invalid ID are silently discarded"
  (let ((table (make-hash-table))
        (pastes (or (ersatz-create-pastes-list id-list) [])))
    (puthash "pastes" pastes table)
    (json-encode table)))

(provide 'ersatz-paste)
