(asdf:defsystem sql-utils
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :description "SQL utilities in Common Lisp using cl-dbi."
  :depends-on (:alexandria :str :cl-dbi)
  :serial t
  :components ((:module "src"
                :components ((:file "packages")
                             (:module "utils"
                              :components ((:file "query-utils")
                                           (:file "insert-utils")
                                           (:file "table-utils")))
                             (:file "datatype-definitions")
                             (:file "protocols")
                             (:file "db-implementation")
                             (:file "table-implementation"))))
  :in-order-to ((test-op (load-op "sql-utils/tests")))
  :perform (asdf:test-op (op c)
                         (unless
                             (uiop:symbol-call
                              :fiveam :run!
                              (uiop:find-symbol* :sql-utils-suite
                                                 :sql-utils-test.sql-utils-tests))
                           (error "test failure"))))

(asdf:defsystem sql-utils/sqlite-utils
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :description "Sqlite3 utilities in Common Lisp using cl-dbi. Implementation of sql-utils protocols for sqlite3 databases."
  :depends-on (:alexandria :str :cl-dbi :sql-utils)
  :serial t
  :components ((:module "src/backend/sqlite/"
                :components ((:file "packages")
                             (:file "sqlite-utilities")
                             (:file "sqlite-db-implementation")
                             (:file "sqlite-table-implementation"))))
  :in-order-to ((test-op (load-op "sqlite-utils/tests")))
  :perform (asdf:test-op (op c)
                         (unless
                             (uiop:symbol-call
                              :fiveam :run!
                              (uiop:find-symbol* :sqlite-utils-suite
                                                 :sqlite-utils-test.sqlite-utils-tests))
                           (error "test failure"))))

(asdf:defsystem sql-utils/sqlite-cli
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :description "Sqlite3 CLI utility binary in Common Lisp. Implementation of sql-utils protocols for sqlite3 databases."
  :depends-on (:alexandria :str :clingon :yason
                           :cl-dbi :sql-utils :sql-utils/sqlite-utils)
  :serial t
  :components ((:module "src/cli/"
                :components ((:file "packages")
                             (:file "sqlite-cli"))))
  :build-operation "program-op"
  :build-pathname "bin/sql-utils" ;; Should this be sqlite-utils?
  :entry-point "sql-utils/sqlite-cli:main")

(asdf:defsystem sql-utils/tests
  :author  "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :depends-on (:sql-utils :sql-utils/sqlite-utils :fiveam)
  :serial t
  :components ((:module "test"
                :components ((:file "packages")
                             (:file "sql-utils-tests"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call
                          :fiveam :run!
                          (uiop:find-symbol* :sql-utils-suite
                                             :sql-utils-test.sql-utils-tests))))

(defun %this-file ()
  (asdf:system-relative-pathname
   (asdf:find-system :sql-utils) "sql-utils.asd"))
;; (%this-file)

(defun dump-system-executable (system &key (lisp-exe "sbcl"))
  (let* (;; (system (find-system system))
         (this-file (%this-file))
         (cmd (format nil "~a --load '~a' --eval '~a' --eval '~a' --eval '~a'"
                      lisp-exe
                      (namestring this-file)
                      (format nil "(ql:quickload :~a)" system)
                      (format nil "(asdf:make :~a)" system)
                      "(uiop:quit)")))
    (format t "~&Dumping ~a executable from ~a~%" system this-file)
    (format t "~&Running ~a~%" cmd)
    (uiop:run-program cmd)))

;; (dump-system-executable :sql-utils/sqlite-cli)
