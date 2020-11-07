(require 'web-server)
(require 'cl)
(require 'json)

;;; TODO/FIXME
;;; if I specified the wrong keys with any call i get:
;;; 422 - {"result": "error", "error_msg": "Wrong keys 'foobar' in {'api_key': ['R89j54QSLjgL6kM2pbewzGgg5wkFASfS'], 'foobar': ['12']}"}
;;; 422 per missing api_key
;;; 301 per path invalido

(defconst valid-languages '("autodetect" "bash" "c" "cpp" "csharp" "css" "html"
                            "java" "js" "json" "lua" "markdown" "objective-c"
                            "perl" "php" "python" "swift" "text" "autodetect"
                            "abap" "ada" "agda" "ahk" "alloy" "antlr" "antlr-as"
                            "antlr-cpp" "antlr-csharp" "antlr-java" "antlr-objc"
                            "antlr-perl" "antlr-python" "antlr-ruby" "apacheconf"
                            "apl" "applescript" "as" "as3" "aspectj" "aspx-cs"
                            "aspx-vb" "asy" "at" "autoit" "awk" "basemake" "bat"
                            "bbcode" "befunge" "blitzbasic" "blitzmax" "boo"
                            "brainfuck" "bro" "bugs" "c-objdump" "ca65" "cbmbas"
                            "ceylon" "cfc" "cfengine3" "cfm" "cfs" "chai" "chapel"
                            "cheetah" "cirru" "clay" "clojure" "clojurescript"
                            "cmake" "cobol" "cobolfree" "coffee-script" "common-lisp"
                            "console" "control" "coq" "cpp-objdump" "croc"
                            "cryptol" "css+django" "css+erb" "css+genshitext"
                            "css+lasso" "css+mako" "css+mozpreproc" "css+myghty"
                            "css+php" "css+smarty" "cucumber" "cuda" "cypher"
                            "cython" "d" "d-objdump" "dart" "delphi" "dg" "diff"
                            "django" "docker" "dpatch" "dtd" "duel" "dylan"
                            "dylan-console" "dylan-lid" "ebnf" "ec" "ecl" "eiffel"
                            "elixir" "erb" "erl" "erlang" "evoque" "factor"
                            "fan" "fancy" "felix" "fortran" "foxpro" "fsharp"
                            "gap" "gas" "genshi" "genshitext" "glsl" "gnuplot"
                            "go" "golo" "gooddata-cl" "gosu" "groff" "groovy"
                            "gst" "haml" "handlebars" "haskell" "haxeml" "html+cheetah"
                            "html+django" "html+evoque" "html+genshi" "html+handlebars"
                            "html+lasso" "html+mako" "html+myghty" "html+php"
                            "html+smarty" "html+twig" "html+velocity" "http"
                            "hx" "hybris" "hylang" "i6t" "idl" "idris" "iex"
                            "igor" "inform6" "inform7" "ini" "io" "ioke" "ipython2"
                            "ipython3" "ipythonconsole" "irc" "isabelle" "jade"
                            "jags" "jasmin" "javascript+mozpreproc" "jlcon"
                            "js+cheetah" "js+django" "js+erb" "js+genshitext"
                            "js+lasso" "js+mako" "js+myghty" "js+php" "js+smarty"
                            "jsonld" "jsp" "julia" "kal" "kconfig" "koka" "kotlin"
                            "lagda" "lasso" "lcry" "lean" "lhs" "lidr" "lighty"
                            "limbo" "liquid" "live-script" "llvm" "logos" "logtalk"
                            "lsl" "make" "mako" "maql" "mask" "mason" "mathematica"
                            "matlab" "matlabsession" "minid" "modelica" "modula2"
                            "monkey" "moocode" "moon" "mozhashpreproc" "mozpercentpreproc"
                            "mql" "mscgen" "mupad" "mxml" "myghty" "mysql"
                            "nasm" "nemerle" "nesc" "newlisp" "newspeak" "nginx"
                            "nimrod" "nit" "nixos" "nsis" "numpy" "objdump"
                            "objdump-nasm" "objective-c++" "objective-j" "ocaml"
                            "octave" "ooc" "opa" "openedge" "pan" "pawn" "perl6"
                            "pig" "pike" "plpgsql" "postgresql" "postscript"
                            "pot" "pov" "powershell" "prolog" "properties"
                            "protobuf" "psql" "puppet" "py3tb" "pycon" "pypylog"
                            "pytb" "python3" "qbasic" "qml" "racket" "ragel"
                            "ragel-c" "ragel-cpp" "ragel-d" "ragel-em" "ragel-java"
                            "ragel-objc" "ragel-ruby" "raw" "rb" "rbcon" "rconsole"
                            "rd" "rebol" "red" "redcode" "registry" "resource"
                            "rexx" "rhtml" "robotframework" "rql" "rsl" "rst"
                            "rust" "sass" "scala" "scaml" "scheme" "scilab"
                            "scss" "shell-session" "slim" "smali" "smalltalk"
                            "smarty" "sml" "snobol" "sourceslist" "sp" "sparql"
                            "spec" "splus" "sql" "sqlite3" "squidconf" "ssp"
                            "stan" "swig" "systemverilog" "tads3" "tcl" "tcsh"
                            "tea" "tex" "textile" "todotxt" "trac-wiki" "treetop"
                            "ts" "twig" "urbiscript" "vala" "vb.net" "vctreestatus"
                            "velocity" "verilog" "vgl" "vhdl" "vim" "xml" "xml+cheetah"
                            "xml+django" "xml+erb" "xml+evoque" "xml+lasso"
                            "xml+mako" "xml+myghty" "xml+php" "xml+smarty"
                            "xml+velocity" "xquery" "xslt" "xtend" "xul+mozpreproc"
                            "yaml" "yaml+jinja" "zephir"))

(defvar valid-keys '("key1" "key2"))

(defvar ersatz-storage '())

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

(defun ersatz-storage-to-json ()
  "Returns a JSON representation of the whole storage"
  (ersatz-pastes-to-json (--map (car it) ersatz-storage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic utility functions
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
        (if (find key valid-keys :test 'equal )
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

(defun start-ersatz-server (&optional try-kill)
  (interactive)
  (when try-kill
    (stop-ersatz-server))
  (setq ersatz-storage '())
  (ws-start #'ersatz-pastery-handler 8080))

(defmacro with-debug-server (&rest forms)
  (let ((result (make-symbol "result")))
    `(progn
       (start-ersatz-server t)
       (let ((,result (progn ,@forms)))
         (stop-ersatz-server)
         ,result))))

(provide 'esatz-pastery)
