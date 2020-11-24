;;; Paste definition

(cl-defstruct (paste (:constructor new-paste))
  (initial-duration 43200 :read-only t)
  (created (float-time) :read-only t)
  (title "" :read-only t)
  (language "text" :read-only t)
  (max_views 0 :read-only nil) ;;; TODO/FIXME decrease the count and silently delete if the case
  (body "" :read-only t))

;;; TODO/FIXME function to clean the paste if outdated!

(defun ersatz-paste-compute-duration (paste)
  (truncate
   (/ (let ((now (float-time))
            (initial-duration-s (* 60 (paste-initial-duration paste))))
        (- initial-duration-s (- now (paste-created paste))))
      60)))

(defun ersatz-paste-to-table (id paste)
  (let ((table (make-hash-table)))
    (puthash "id" id table)
    (puthash "title" (paste-title paste) table)
    ;; This isn't really accurate if the server is started on a different interface
    (puthash "url" (format "http://localhost:%d/%s/" ersatz-pastery-server-port id) table)
    (puthash "language" (paste-language paste) table)
    (puthash "duration"
             (ersatz-paste-compute-duration paste)
             table)
    (puthash "max_views" (paste-max_views paste) table)
    (puthash "body" (paste-body paste) table)
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
