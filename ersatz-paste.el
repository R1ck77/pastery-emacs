;;; Paste definition

(cl-defstruct (paste-data (:constructor new-paste-data))
              (title "" :read-only t)
              (language "text" :read-only t)
              (body "" :read-only t))

(cl-defstruct (paste-metadata (:constructor new-paste-metadata))
              (initial-duration 43200 :read-only t)
              (created (float-time) :read-only t)
              (max_views nil :read-only t)
              (owner nil :read-only t))

(defun ersatz-filter-arguments (arguments allowed-keys)
  (let ((pairs (-partition-all 2 arguments)))
    (-flatten-n 1(--map (list (car it) (cadr it)) (--filter (member (car it) allowed-keys) pairs)))))

(defun new-paste (&rest arguments)
  (cons
   (apply #'new-paste-data (ersatz-filter-arguments arguments'(:title :language :body)))
   (apply #'new-paste-metadata (ersatz-filter-arguments arguments '(:initial-duration :created :max_views :owner)))))

(defun ersatz-get-paste-owner (paste)
  (paste-metadata-owner (cdr paste)))

(defun ersatz-paste-with-decremented-view (paste-cons)
  "Stridently functional approach to decrementing the paste

It strives to keep paste a value, despite not being ELISP truly a functional language."
  (let* ((metadata (cdr paste-cons))
         (old-max-views (paste-metadata-max_views metadata)))    
    (cons (car paste-cons)
          (new-paste-metadata :initial-duration (paste-metadata-initial-duration metadata)
                              :created (paste-metadata-created metadata)
                              :max_views (and old-max-views (1- old-max-views))
                              :owner (paste-metadata-owner metadata)))))

(defun ersatz-paste-compute-float-duration (metadata)
  "Compute how many seconds remain before a paste is overdue"
  (let ((now (float-time))
        (initial-duration-s (* 60 (paste-metadata-initial-duration metadata))))
    (- initial-duration-s (- now (paste-metadata-created metadata)))))

(defun ersatz-paste-compute-duration (metadata)
  "Compute the value of 'paste validity' in minutes as the user should see it"
  (truncate
   (/ (ersatz-paste-compute-float-duration metadata) 60)))

(defun ersatz-paste-overdue? (metadata)
  (< (ersatz-paste-compute-float-duration metadata) 1))

(defun ersatz-paste-to-table (id paste &optional hide-body)
  (let ((table (make-hash-table))
        (data (car paste))
        (metadata (cdr paste)))
    (puthash "id" id table)
    (puthash "title" (paste-data-title data) table)
    ;; This isn't really accurate if the server is started on a different interface
    (puthash "url" (format "http://localhost:%d/%s/" ersatz-pastery-server-port id) table)
    (puthash "language" (paste-data-language data) table)
    (puthash "duration"
             (ersatz-paste-compute-duration metadata)
             table)
    (unless hide-body
      (puthash "body" (paste-data-body data) table))
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
