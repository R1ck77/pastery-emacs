;;; Paste definition

;; TODO/FIXME this paste contains both paste related stuff, and handling related stuff. Use two structures
(cl-defstruct (paste (:constructor new-paste))
  (initial-duration 43200 :read-only t)
  (created (float-time) :read-only t)
  (title "" :read-only t)
  (language "text" :read-only t)
  (max_views nil :read-only t)
  (body "" :read-only t)
  (owner nil :read-only t))

(defun ersatz-paste-with-decremented-view (paste)
  "Stridently functional approach to decrementing the paste

It strives to keep paste a value, despite not being ELISP truly a functional language."
  (let ((old-max-views (paste-max_views paste)))
    (new-paste :initial-duration (paste-initial-duration paste)
               :created (paste-created paste)
               :title (paste-title paste)
               :language (paste-language paste)
               :max_views (and old-max-views (1- old-max-views))
               :body (paste-body paste)
               :owner (paste-owner paste))))

(defun ersatz-paste-compute-float-duration (paste)
  "Compute how many seconds remain before a paste is overdue"
  (let ((now (float-time))
        (initial-duration-s (* 60 (paste-initial-duration paste))))
    (- initial-duration-s (- now (paste-created paste)))))

(defun ersatz-paste-compute-duration (paste)
  "Compute the value of 'paste validity' in minutes as the user should see it"
  (truncate
   (/ (ersatz-paste-compute-float-duration paste) 60)))

(defun ersatz-paste-overdue? (paste)
  (< (ersatz-paste-compute-float-duration paste) 1))

(defun ersatz-paste-to-table (id paste &optional hide-body)
  (let ((table (make-hash-table)))
    (puthash "id" id table)
    (puthash "title" (paste-title paste) table)
    ;; This isn't really accurate if the server is started on a different interface
    (puthash "url" (format "http://localhost:%d/%s/" ersatz-pastery-server-port id) table)
    (puthash "language" (paste-language paste) table)
    (puthash "duration"
             (ersatz-paste-compute-duration paste)
             table)
    (unless hide-body
      (puthash "body" (paste-body paste) table))
    table))

(defun ersatz-paste-to-json (id paste &optional hide-body)
  (json-encode (ersatz-paste-to-table id paste hide-body)))

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
