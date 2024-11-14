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
  :description "Sqlite3 CLI utilities in Common Lisp. Implementation of sql-utils protocols for sqlite3 databases."
  :depends-on (:alexandria :str :clingon
                           :cl-dbi :sql-utils :sql-utils/sqlite-utils)
  :serial t
  :components ((:module "src/cli/"
                :components ((:file "packages")
                             (:file "sqlite-cli")))))

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
