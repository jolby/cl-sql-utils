(in-package :sql-utils-test.cli-tests)

(def-suite cli-suite
  :description "Test suite for SQL Utils CLI")

(in-suite cli-suite)

(test cli-suite-exists
  (is-true t))

(defvar *test-db* nil)
(defvar *cli-path* nil)

(defun setup-cli-tests ()
  "Setup function run before CLI tests"
  ;; Find CLI executable
  (setf *cli-path* 
        (merge-pathnames #p"bin/sql-utils"
                        (asdf:system-source-directory :sql-utils)))
  (unless (probe-file *cli-path*)
    (error "CLI executable not found at ~A - run (dump-system-executable :sql-utils/sqlite-cli) first"
           *cli-path*))
  
  ;; Create temp test database
  (setf *test-db* (uiop:tmpize-pathname "/tmp/sql-utils-test.db"))
  (let ((db (sql-utils:make-db-connection :sqlite :filename *test-db*)))
    ;; Create test tables
    (sql-utils:create-table db "test1" 
                           '(("id" . "INTEGER")
                             ("name" . "TEXT"))
                           :pk "id")
    ;; Insert some test data
    (sql-utils:insert (sql-utils:make-table db "test1")
                     '(:id 1 :name "test1"))
    (sql-utils:insert (sql-utils:make-table db "test1")
                     '(:id 2 :name "test2"))))

(defun teardown-cli-tests ()
  "Cleanup after CLI tests"
  (when (and *test-db* (probe-file *test-db*))
    (delete-file *test-db*))
  (setf *test-db* nil))

(defun run-cli (args &key (input nil))
  "Run CLI command and return (values stdout stderr exit-code)"
  (let ((command (format nil "~A ~A" 
                        (namestring *cli-path*)
                        (if (stringp args) args
                            (format nil "~{~A~^ ~}" args)))))
    (format t "~&Running:~%~A~%" command)
    (multiple-value-bind (out err code)
        (uiop:run-program command
                         :input input
                         :output '(:string :stripped t)
                         :error-output '(:string :stripped t)
                         :ignore-error-status t)
      (values out err code))))

(def-fixture cli-env ()
  (setup-cli-tests)
  (&body)
  (teardown-cli-tests))

;; Basic CLI tests
(test cli-help
  "Test basic CLI help output"
  (with-fixture cli-env ()
    (multiple-value-bind (out err code) (run-cli "--help")
      (declare (ignore err))
      (is (= 0 code))
      (is (str:containsp "SQL database utility tool" out)))))

(test cli-tables
  "Test tables command"
  (with-fixture cli-env ()
    (multiple-value-bind (out err code) 
        (run-cli (list "tables" (namestring *test-db*)))
      (declare (ignore err))
      (is (= 0 code))
      (is (str:containsp "test1" out)))))

(test cli-rows
  "Test rows command"
  (with-fixture cli-env ()
    (multiple-value-bind (out err code)
        (run-cli (list "rows" (namestring *test-db*) "test1"))
      (declare (ignore err))
      (is (= 0 code))
      (is (str:containsp "test1" out))
      (is (str:containsp "test2" out)))))

(test cli-insert
  "Test insert command"
  (with-fixture cli-env ()
    ;; Test inserting a single record
    (multiple-value-bind (out err code)
        (run-cli (list "insert" (namestring *test-db*) "test1" "'(:id 3 :name \"test3\")'"))
      (declare (ignore out err))
      (is (= 0 code)))
    ;; Test inserting multiple records (list of lists)
    (multiple-value-bind (out err code)
        (run-cli (list "insert" (namestring *test-db*) "test1"
                       "'((:id 4 :name \"test4\") (:id 5 :name \"test5\"))'"))
      (declare (ignore out err))
      (is (= 0 code)))

    ;; Do the same as above but with JSON arguments
    (multiple-value-bind (out err code)
        (run-cli (list "insert" (namestring *test-db*) "test1" "'{\"id\": 6, \"name\": \"test6\"}'"))
      (declare (ignore out err))
      (is (= 0 code)))
    ;; Test inserting multiple records (array of JSON objects)
    (multiple-value-bind (out err code)
        (run-cli (list "insert" (namestring *test-db*) "test1" 
                      (format nil "'~A'" "[{\"id\": 7, \"name\": \"test7\"}, {\"id\": 8, \"name\": \"test8\"}, {\"id\": 9, \"name\": \"test9\"}]")))
      ;; (declare (ignore out err))
      (format t "~&INSERT OUT: ~A~%" out)
      (format t "~&ERR ~A~%" err)
      (is (= 0 code)))
    
    ;; Verify the insert
    (let* ((db (sql-utils:make-db-connection :sqlite :filename *test-db*))
           (table (sql-utils:make-table-from-db db "test1"))
           (rows (sql-utils:rows table)))
      (format t "~&Rows: ~A~%" rows)
      (is (= 9 (sql-utils:row-count (sql-utils:make-table db "test1")))))))

;; Predicates: is is-every is-false is-true signals finishes

;; (ql:quickload '(:sql-utils :sql-utils/sqlite-utils :sql-utils/tests :sql-utils/cli-tests))
;; (run! 'sql-utils-test.cli-tests::cli-suite-exists)
;; (run! 'sql-utils-test.cli-tests::cli-help)
;; (run! 'sql-utils-test.cli-tests::cli-tables)
;; (run! 'sql-utils-test.cli-tests::cli-rows)
;; (run! 'sql-utils-test.cli-tests::cli-insert)
;; (run! 'sql-utils-test.cli-tests:cli-suite)
