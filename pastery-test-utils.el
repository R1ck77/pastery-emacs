(require 'buttercup)
(require 'dash)

(defun association-list-matcher (actual expected)
  (and
   (= (length expected)
      (length actual))
   (--all? (let ((key (car it)))
             (equal (cdr it) (cdr (assoc key actual))))
           expected)))

(buttercup-define-matcher-for-binary-function :to-be-same-alist association-list-matcher)

(defun compare-paste (actual-paste expected-paste)
  (not
   (unless (association-list-matcher expected-paste actual-paste)
     (message (format "Comparison failed for id %s:\nExpected: %s\nActual: %s\n" (cdr (assoc 'id expected-paste)) expected-paste actual-paste)))))

(defun paste-list-matcher (actual expected)
  (compare-paste actual expected))

(buttercup-define-matcher-for-binary-function :to-be-paste-like paste-list-matcher)

(defun pastes-lists-matcher (expected-list actual-list)
  (and
   (= (length expected-list) (length actual-list))
   (--all? (not (not it))
           (--map (let* ((id (cdr (assoc 'id it)))
                         (expected-paste it)
                         (actual-paste (car (--filter (string= (cdr (assoc 'id it)) id) actual-list))))
                    (compare-paste expected-paste actual-paste))
                  expected-list))))

(buttercup-define-matcher-for-binary-function :to-be-a-list-of-pastes-like pastes-lists-matcher)

(defun pastes-vector-as-list (pastes-list)
  (append (cdr (assoc 'pastes pastes-list)) '()))

(defun compare-paste-results (expected-pastes-list actual-pastes-list)
  (and (= (length expected-pastes-list)
          (length actual-pastes-list))       
       (compare-paste-lists (pastes-vector-as-list expected-pastes-list)
                            (pastes-vector-as-list actual-pastes-list))))

(defun random-letter ()
  "Generate a random character in the range [90, 65]"
  (char-to-string (+ 65 (random 26))))

(defun create-random-string (&optional length)
  (apply #'concat (--map (random-letter) (number-sequence 1 length))))

(provide 'pastery-test-utils)
