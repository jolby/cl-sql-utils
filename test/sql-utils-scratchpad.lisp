(in-package :sql-utils-test.sql-utils-tests)


(defun test-transform-1 ()
  (with-test-connection (conn)
    (let ((table (sql-utils:create-table
                  conn "test_transform"
                  '(("id" . "INTEGER")
                    ("name" . "TEXT")
                    ("age" . "INTEGER"))
                  :pk "id")))

      ;; Insert some test data
      (sql-utils:insert-all table
                            (list
                             '(:id 1 :name "Alice" :age 30)
                             '(:id 2 :name "Bob" :age 25)))

      ;; Test renaming columns
      (sql-utils:transform table :rename '(("name" . "full_name")))
      (let ((cols (mapcar #'sql-utils:name (sql-utils:columns table))))
        (format t "Cols: ~A ~%" cols))

      ;; Test changing column types
      (sql-utils:transform table :types '(("age" . "TEXT")))
      (let ((age-col (find "age" (sql-utils:columns table)
                           :key #'sql-utils:name :test #'string=)))
        (format t "Age Col: ~A, ColType: ~A~%" age-col (sql-utils:column-type age-col)))

      ;; Test dropping columns
      (sql-utils:transform table :drop '("age"))
      (let ((cols (mapcar #'sql-utils:name (sql-utils:columns table))))
        (format t "Cols: ~A ~%" cols))

      ;; Verify data was preserved
      (let ((rows (sql-utils:rows table)))
        (format t "Rows: ~A ~%" rows)))))
;; (test-transform-1)

(defun %test-transform-with-foreign-keys ()
  (with-test-connection (conn)
    ;; Create parent table
    (sql-utils:create-table conn "parent"
                            '(("id" . "INTEGER")
                              ("name" . "TEXT"))
                            :pk "id")

    ;; Create child table with foreign key
    (let ((table (sql-utils:create-table
                  conn "child"
                  '(("id" . "INTEGER")
                    ("parent_id" . "INTEGER")
                    ("data" . "TEXT"))
                  :pk "id"
                  :foreign-keys '(("parent_id" "parent" "id")))))

      ;; Test dropping foreign key
      (sql-utils:transform table :drop-foreign-keys '("parent_id"))
      ;; Verify foreign key was dropped
      (assert (= 0 (length (sql-utils:foreign-keys table)))
              (table (length (sql-utils:foreign-keys table)))
              "Should be 0 after dropping")

      ;; Test adding foreign key back
      (sql-utils:transform table
                           :add-foreign-keys '(("parent_id" "parent" "id")))
      ;; Verify foreign key was added
      (assert (= 1 (length (sql-utils:foreign-keys table)))
                 (table (length (sql-utils:foreign-keys table)))
                 "Should be 1 after adding"))))

#+(or)
(let* ((dbconn (%create-test-connection :sqlite :filename "/tmp/test.db" :recreate t))
       (test-table-1 (sql-utils:create-table
                  dbconn
                  "test_table_2"
                  '(("id" . "INTEGER")
                    ("name" . "TEXT")
                    ("parent_id" . "INTEGER")
                    ("dumb_text" . "TEXT"))
                  :pk "id"
                  :foreign-keys '(("parent_id" "test_table" "id"))
                  :defaults '(("dumb_text" . "DEFAULT DUMB TEXT")))))
      (setf *last-table* test-table-1))
;; (squ:columns *last-table*)

#+(or)
  (with-test-connection (conn)
    ;; Test table with compound primary key
    (let ((table-sql (sql-utils:create-table-sql
                      conn "test_compound_pk"
                      '(("year" . "INTEGER")
                        ("month" . "INTEGER")
                        ("name" . "TEXT"))
                      :pk '("year" "month")))
          (table (sql-utils:create-table
                  conn "test_compound_pk"
                  '(("year" . "INTEGER")
                    ("month" . "INTEGER")
                    ("name" . "TEXT"))
                  :pk '("year" "month"))))
      (setf *last-table* table)
      (format t "CREATE: ~% ~A~%" table-sql)
      (format t "PKS: ~A ~%" (sql-utils:pks table))
      (format t "COLS: ~A ~%" (sql-utils:columns table))
      (format t "TABLE INFO: ~A ~%" (sql-utils::%table-info table))
      ;; (is (equal '("year" "month") (sql-utils:pks table)))
      ;; (is-false (sql-utils:use-rowid table))
      ))
;; (sql-utils::%table-info *last-table*)

#+(or)
(with-test-connection (conn)
    (let ((table (sql-utils:create-table
                  conn "test_insert_table"
                  '(("id" . "INTEGER")
                    ("name" . "TEXT")
                    ("age" . "INTEGER"))
                  :pk "id")))
      ;; Insert a single record
      (let ((record '(:id 998 :name "Alice" :age 30)))
        (sql-utils:insert table record)
        (format t "Row Cnt: ~A ~%" (sql-utils:row-count table))
        (let* ((rows (sql-utils:rows table))
               (row (first rows)))
          (format t "Rows: ~A ~%" rows)
          (format t "Row: ~A ~%" row))
        (format t "DEFAULT: ~A ~%" (sql-utils:value-or-default table 999 :default)))))
