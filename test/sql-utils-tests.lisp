(in-package :sql-utils-test.sql-utils-tests)

(def-suite sql-utils-suite)

(in-suite sql-utils-suite)

(test sql-utils-suite-exists
  (is-true t))

;;;; Test db connection creation

(defparameter *last-connection* nil)

(defparameter *last-table* nil)

(defun %create-test-connection (&rest args)
  (setf *last-connection*
          (if (null args)
              (sql-utils:make-db-connection :sqlite :memory t)
              ;; (sql-utils:make-db-connection :sqlite :filename "/tmp/test.db")
              (apply #'sql-utils:make-db-connection args))))

(defmacro with-test-connection ((connection &rest conn-args) &body body)
  `(let ((,connection (%create-test-connection ,@conn-args)))
     (unwind-protect (progn ,@body)
       (dbi:disconnect (sql-utils::connection ,connection)))))

(defun %create-test-table (db &key (table-name "test_table_1"))
  (let* ((create-sql
          (format nil
                  "CREATE TABLE IF NOT EXISTS ~a (id INTEGER PRIMARY KEY, name TEXT)"
                  table-name)))
    (sql-utils:execute db create-sql)))

(defmacro with-test-table ((&key (table-name "test_table_1")) &body body)
  `(with-test-connection (conn)
     (%create-test-table conn :table-name ,table-name)
     (progn ,@body)))

(defun %create-test-view (db &key (view-name "test_view_1"))
  (let* ((create-sql
          (format nil
                  "CREATE VIEW IF NOT EXISTS ~a AS SELECT * FROM test_table_1"
                  view-name)))
    (sql-utils:execute db create-sql)))

(defmacro with-test-view ((&key (view-name "test_view_1")) &body body)
  `(with-test-connection (conn)
     (%create-test-view conn :view-name ,view-name)
     (progn ,@body)))

(test creating-db-memory-connection
  (let ((db-connection (sql-utils:make-db-connection :sqlite :memory t)))
    (is (sql-utils:database-p db-connection))
    (is (sql-utils:open-p db-connection))
    (dbi:disconnect (sql-utils::connection db-connection))
    (is (not (sql-utils:open-p db-connection)))))

(test creating-db-named-memory-connection
  (let ((db-connection
         (sql-utils:make-db-connection :sqlite :memory-name "test-memory-db")))
    (is (sql-utils:database-p db-connection))
    (is (sql-utils:open-p db-connection))
    (dbi:disconnect (sql-utils::connection db-connection))
    (is (not (sql-utils:open-p db-connection)))))

(test creating-db-file-connection
  ;; make sure to delete the test db file if it exists
  (unwind-protect
      (let ((db-connection
             (sql-utils:make-db-connection :sqlite :filename "/tmp/test.db")))
        (is (sql-utils:database-p db-connection))
        (is (sql-utils:open-p db-connection))
        (dbi:disconnect (sql-utils::connection db-connection))
        (is (not (sql-utils:open-p db-connection))))
    (ignore-errors (delete-file "/tmp/test.db"))))

(defun test-conn-1 ()
  (let ((db-connection (%create-test-connection)))
    (assert (not (null (sql-utils:database-p db-connection))))
    (assert (sql-utils:open-p db-connection))
    (dbi:disconnect (sql-utils::connection db-connection))
    (assert (not (sql-utils:open-p db-connection)))))

;; (test-conn-1)

(test create-test-table-1
  (with-test-connection (conn)
    (let ((before-tables (sql-utils:table-names conn)))
      (is (length before-tables) 0)
      (%create-test-table conn :table-name "test_table_1")
      (is (= (length (sql-utils:table-names conn)) 1))
      (let ((after-tables (sql-utils:table-names conn)))
        (is (not (null (set-difference after-tables before-tables))))))))

(defun test-create-test-table-2 ()
  (with-test-connection (conn)
    (let ((before-tables (sql-utils:table-names conn)))
      (assert (null before-tables))
      (%create-test-table conn :table-name "test_table_1")
      (let ((after-tables (sql-utils:table-names conn)))
        (assert (= (length after-tables) 1))
        (format t "Before tables: ~a, After tables: ~a~%" before-tables
                after-tables)
        (assert (not (null (set-difference after-tables before-tables))))))))

;; (test-create-test-table-2)

(defun test-create-test-view-1 ()
  (with-test-connection (conn)
    (with-test-table nil
      (let ((before-views (sql-utils:view-names conn)))
        (assert (= (length before-views) 0))
        (with-test-view nil
          (let ((after-views (sql-utils:view-names conn)))
            (format t "Before views: ~a, After views: ~a~%" before-views
                    after-views)
            (assert (= (length after-views) 1))
            (assert (not
                      (null (set-difference after-views before-views))))))))))

;; (test-create-test-view-1)

(test create-test-view-1
  (with-test-connection (conn)
    (with-test-table nil
      (let ((before-views (sql-utils:view-names conn)))
        (with-test-view nil
          (is (= (length before-views) 0))
          (let ((after-views (sql-utils:view-names conn)))
            (is (= (length after-views) 1))
            (is (not (null (set-difference after-views before-views))))))))))

(test create-table-sql-basic
  (with-test-connection (conn)
    (let ((sql
           (sql-utils:create-table-sql conn "test_table"
                                       '(("id" . "INTEGER") ("name" . "TEXT"))
                                       :pk "id")))
      (is
       (string= sql
                "CREATE TABLE [test_table] ([id] INTEGER, [name] TEXT, PRIMARY KEY([id]))")))))

(test create-table-sql-if-not-exists
  (with-test-connection (conn)
    (let ((sql
           (sql-utils:create-table-sql conn "test_table"
                                       '(("id" . "INTEGER") ("name" . "TEXT"))
                                       :if-not-exists t
                                       :pk "id")))
      (is
       (string= sql
                "CREATE TABLE IF NOT EXISTS [test_table] ([id] INTEGER, [name] TEXT, PRIMARY KEY([id]))")))))

(test create-table-with-foreign-key
  (with-test-connection (conn)
    (let ((sql
           (sql-utils:create-table-sql conn "test_table"
                                       '(("id" . "INTEGER") ("name" . "TEXT")
                                         ("parent_id" . "INTEGER"))
                                       :pk "id"
                                       :foreign-keys '(("parent_id"
                                                        "test_table" "id")))))
      (is
       (string= sql
                (format nil "CREATE TABLE [test_table] (~
                    [id] INTEGER, ~
                    [name] TEXT, ~
                    [parent_id] INTEGER, PRIMARY KEY([id]), ~
                    FOREIGN KEY([parent_id]) REFERENCES [test_table]([id]))"))))))

(test create-table-execution
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_table"
                                   '(("id" . "INTEGER") ("name" . "TEXT"))
                                   :pk "id")))
      (is (sql-utils:exists-p table))
      (is (= 2 (length (sql-utils:columns table)))))))

(test create-table-with-constraints
  (with-test-connection (conn)
    (let ((sql
           (sql-utils:create-table-sql conn "test_table"
                                       '(("id" . "INTEGER") ("name" . "TEXT")
                                         ("email" . "TEXT")
                                         ("created_at" . "TEXT"))
                                       :pk "id"
                                       :not-null '("name" "email")
                                       :defaults '(("created_at" .
                                                    "CURRENT_TIMESTAMP")))))
      (is
       (string= sql
                "CREATE TABLE [test_table] ([id] INTEGER, [name] TEXT NOT NULL, [email] TEXT NOT NULL, [created_at] TEXT DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY([id]))")))))

(test quote-default-value
  (with-test-connection (conn)
    ;; Test already quoted strings
    (is (string= "'foo'" (squ:quote-default-value conn "'foo'")))
    (is (string= "'foo)'" (squ:quote-default-value conn "'foo)'")))
    (is (string= "'1'" (squ:quote-default-value conn "'1'")))
    (is (string= "'1'" (squ:quote-default-value conn "'1'")))
    (is (string= "'1'" (squ:quote-default-value conn "'1'")))
    ;; Test expressions
    (is
     (string= "(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))"
              (squ:quote-default-value conn
                                       "STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')")))
    ;; Test special SQLite keywords
    (is (string= "CURRENT_TIME" (squ:quote-default-value conn "CURRENT_TIME")))
    (is (string= "CURRENT_DATE" (squ:quote-default-value conn "CURRENT_DATE")))
    (is
     (string= "CURRENT_TIMESTAMP"
              (squ:quote-default-value conn "CURRENT_TIMESTAMP")))
    (is
     (string= "current_timestamp"
              (squ:quote-default-value conn "current_timestamp")))
    (is
     (string= "((CURRENT_TIMESTAMP))"
              (squ:quote-default-value conn "(CURRENT_TIMESTAMP)")))
    ;; Test quoted strings
    (is
     (string= "'CURRENT_TIMESTAMP'"
              (squ:quote-default-value conn "'CURRENT_TIMESTAMP'")))
    (is
     (string= "\"CURRENT_TIMESTAMP\""
              (squ:quote-default-value conn "\"CURRENT_TIMESTAMP\"")))))

(test default-value-in-table
  (with-test-connection (conn)
    (flet ((create-and-check (col-def initial expected)
             (let ((table-name (format nil "test_table_~A" (random 1000000))))
               ;; Create table with the column definition
               (sql-utils:execute conn
                                  (format nil "CREATE TABLE ~A (col ~A)"
                                          table-name col-def))
               ;; Get the default value
               (let* ((table (sql-utils:make-table conn table-name))
                      (cols (sql-utils:columns table))
                      (default-val (sql-utils:default-value (first cols))))
                 (setf *last-table* table)
                 ;; Check both raw and quoted versions
                 (is (string= initial default-val))
                 (is
                  (string= expected
                           (squ:quote-default-value conn default-val)))))))
      ;; Test various column definitions
      (create-and-check "TEXT DEFAULT 'foo'" "'foo'" "'foo'")
      (create-and-check "TEXT DEFAULT 'foo)'" "'foo)'" "'foo)'")
      (create-and-check "INTEGER DEFAULT '1'" "'1'" "'1'")
      (create-and-check "INTEGER DEFAULT 1" "1" "'1'")
      (create-and-check "INTEGER DEFAULT (1)" "1" "'1'")
      (create-and-check "TEXT DEFAULT (STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))"
                        "STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')"
                        "(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))")
      (create-and-check "TEXT DEFAULT CURRENT_TIME" "CURRENT_TIME"
                        "CURRENT_TIME")
      (create-and-check "TEXT DEFAULT CURRENT_DATE" "CURRENT_DATE"
                        "CURRENT_DATE")
      (create-and-check "TEXT DEFAULT CURRENT_TIMESTAMP" "CURRENT_TIMESTAMP"
                        "CURRENT_TIMESTAMP")
      (create-and-check "TEXT DEFAULT current_timestamp" "current_timestamp"
                        "current_timestamp")
      (create-and-check "TEXT DEFAULT (CURRENT_TIMESTAMP)" "CURRENT_TIMESTAMP"
                        "CURRENT_TIMESTAMP")
      (create-and-check "TEXT DEFAULT 'CURRENT_TIMESTAMP'"
                        "'CURRENT_TIMESTAMP'" "'CURRENT_TIMESTAMP'")
      (create-and-check "TEXT DEFAULT \"CURRENT_TIMESTAMP\""
                        "\"CURRENT_TIMESTAMP\"" "\"CURRENT_TIMESTAMP\""))))

(test row-count-basics
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_table"
                                   '(("id" . "INTEGER") ("name" . "TEXT"))
                                   :pk "id")))
      ;; Empty table should have count 0
      (is (= 0 (sql-utils:row-count table)))
      ;; Insert some rows
      (sql-utils:execute conn "INSERT INTO test_table (name) VALUES ('Alice')")
      (sql-utils:execute conn "INSERT INTO test_table (name) VALUES ('Bob')")
      (sql-utils:execute conn
                         "INSERT INTO test_table (name) VALUES ('Charlie')")
      ;; Count should now be 3
      (is (= 3 (sql-utils:row-count table)))
      ;; Test count-where
      (is
       (= 1
          (sql-utils:row-count-where table
                                     :where "name = ?"
                                     :where-args '("Alice"))))
      (is
       (= 2
          (sql-utils:row-count-where table
                                     :where "name LIKE ?"
                                     :where-args '("%i%"))))
      (is
       (= 0
          (sql-utils:row-count-where table
                                     :where "name = ?"
                                     :where-args '("David")))))))

(test rows-basics
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_table"
                                   '(("id" . "INTEGER") ("name" . "TEXT")
                                     ("age" . "INTEGER"))
                                   :pk "id")))
      ;; Empty table should return empty list
      (is (null (squ:rows table)))
      ;; Insert test data
      (sql-utils:execute conn
                         "INSERT INTO test_table (name, age) VALUES ('Alice', 25)")
      (sql-utils:execute conn
                         "INSERT INTO test_table (name, age) VALUES ('Bob', 30)")
      (sql-utils:execute conn
                         "INSERT INTO test_table (name, age) VALUES ('Charlie', 35)")
      (sql-utils:execute conn
                         "INSERT INTO test_table (name, age) VALUES ('David', 40)")
      (sql-utils:execute conn
                         "INSERT INTO test_table (name, age) VALUES ('Eve', 45)")
      ;; Test basic row retrieval
      (let ((all-rows (squ:rows table)))
        (is (= 5 (length all-rows)))
        (is (string= "Alice" (getf (first all-rows) :|name|))))
      ;; Test rows-where with WHERE clause
      (let ((young-people
             (sql-utils:rows-where table :where "age < ?" :where-args '(35))))
        (is (= 2 (length young-people)))
        (is (every (lambda (row) (< (getf row :|age|) 35)) young-people)))
      ;; Test ORDER BY
      (let ((ordered-rows (sql-utils:rows-where table :order-by "age DESC")))
        (is (= 45 (getf (first ordered-rows) :|age|)))
        (is (= 25 (getf (car (last ordered-rows)) :|age|))))
      ;; Test LIMIT and OFFSET
      (let ((limited-rows
             (sql-utils:rows-where table :order-by "age" :limit 2 :offset 1)))
        (is (= 2 (length limited-rows)))
        (is (string= "Bob" (getf (first limited-rows) :|name|)))
        (is (string= "Charlie" (getf (second limited-rows) :|name|))))
      ;; Test SELECT specific columns
      (let ((names-only (sql-utils:rows-where table :select "name")))
        (is (= 5 (length names-only)))
        (is (null (getf (first names-only) :|age|)))
        (is (string= "Alice" (getf (first names-only) :|name|)))))))

;; (squ:quote-default-value *last-connection* "(CURRENT_TIMESTAMP)")

(test primary-key-basics
  (with-test-connection (conn)
    ;; Test table with explicit primary key
    (let ((table
           (sql-utils:create-table conn "test_pk_table"
                                   '(("id" . "INTEGER") ("name" . "TEXT"))
                                   :pk "id")))
      (is (equal '("id") (sql-utils:pks table)))
      (is-false (sql-utils:uses-rowid-p table)))
    ;; Test table with compound primary key
    (let ((table
           (sql-utils:create-table conn "test_compound_pk"
                                   '(("year" . "INTEGER") ("month" . "INTEGER")
                                     ("name" . "TEXT"))
                                   :pk '("year" "month"))))
      (is (equal '("year" "month") (sql-utils:pks table)))
      (is-false (sql-utils:uses-rowid-p table)))
    ;; Test table with no explicit primary key (should use rowid)
    (let ((table
           (sql-utils:create-table conn "test_no_pk"
                                   '(("name" . "TEXT") ("age" . "INTEGER")))))
      (is (equal '("rowid") (sql-utils:pks table)))
      (is-true (sql-utils:uses-rowid-p table)))))

;; Test insert and insert-all methods
(test insert-basics
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_insert_table"
                                   '(("id" . "INTEGER") ("name" . "TEXT")
                                     ("age" . "INTEGER"))
                                   :pk "id")))
      ;; Insert a single record
      (let ((record '(:name "Alice" :age 30)))
        (sql-utils:insert table record)
        (is (= 1 (sql-utils:row-count table)))
        (let ((row (first (squ:rows table))))
          (is (string= "Alice" (getf row :|name|)))
          (is (= 30 (getf row :|age|))))))))

(test insert-all-basics
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_insert_all_table"
                                   '(("id" . "INTEGER") ("name" . "TEXT")
                                     ("age" . "INTEGER"))
                                   :pk "id")))
      ;; Insert multiple records
      (let ((records
             (list '(:name "Bob" :age 25) '(:name "Charlie" :age 35)
                   '(:name "David" :age 40))))
        (sql-utils:insert-all table records)
        (is (= 3 (sql-utils:row-count table)))
        (let ((rows (squ:rows table :order-by "age")))
          (is (string= "Bob" (getf (first rows) :|name|)))
          (is (string= "Charlie" (getf (second rows) :|name|)))
          (is (string= "David" (getf (third rows) :|name|))))))))

(test lookup-basics
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_lookup_table"
                                   '(("id" . "INTEGER") ("name" . "TEXT")
                                     ("age" . "INTEGER"))
                                   :pk "id")))
      ;; Insert records
      (sql-utils:insert-all table
                            (list '(:id 1 :name "Alice" :age 30)
                                  '(:id 2 :name "Bob" :age 25)
                                  '(:id 3 :name "Charlie" :age 35)))
      ;; Lookup existing record
      (let ((record (sql-utils:lookup table 2)))
        (is (= (getf record :|id|) 2))
        (is (string= (getf record :|name|) "Bob"))
        (is (= (getf record :|age|) 25)))
      ;; Test non-existing record
      (signals sql-utils:not-found-error
        (sql-utils:lookup table 999))
      ;; Test value-or-default for non-existing record
      (is
       (equal :not-found (sql-utils:value-or-default table 999 :not-found))))))

(test delete-record-basics
  (with-test-connection (conn)
    (let ((table (sql-utils:create-table conn "test_delete"
                                        '(("id" . "INTEGER")
                                          ("name" . "TEXT"))
                                        :pk "id")))
      ;; Insert test data
      (sql-utils:insert table '(:id 1 :name "Alice"))
      (sql-utils:insert table '(:id 2 :name "Bob"))

      ;; Test deleting existing record
      (is (= 2 (sql-utils:row-count table)))
      (sql-utils:delete-record table 1)
      (is (= 1 (sql-utils:row-count table)))
      (let ((remaining (first (sql-utils:rows table))))
        (is (= 2 (getf remaining :|id|)))
        (is (string= "Bob" (getf remaining :|name|))))

      ;; Test error on non-existent record
      (signals sql-utils:not-found-error
        (sql-utils:delete-record table 999))

      ;; Test compound primary key
      (let ((table2 (sql-utils:create-table conn "test_delete_compound"
                                           '(("year" . "INTEGER")
                                             ("month" . "INTEGER")
                                             ("data" . "TEXT"))
                                           :pk '("year" "month"))))
        (sql-utils:insert table2 '(:year 2024 :month 1 :data "Jan"))
        (sql-utils:insert table2 '(:year 2024 :month 2 :data "Feb"))

        (is (= 2 (sql-utils:row-count table2)))
        (sql-utils:delete-record table2 '(2024 1))
        (is (= 1 (sql-utils:row-count table2)))
        (let ((remaining (first (sql-utils:rows table2))))
          (is (= 2024 (getf remaining :|year|)))
          (is (= 2 (getf remaining :|month|))))))))

(test delete-where-basics
  (with-test-connection (conn)
    (let ((table (sql-utils:create-table conn "test_delete_where"
                                        '(("id" . "INTEGER")
                                          ("name" . "TEXT")
                                          ("age" . "INTEGER"))
                                        :pk "id")))
      ;; Insert test data
      (sql-utils:insert-all table
                           (list '(:id 1 :name "Alice" :age 25)
                                 '(:id 2 :name "Bob" :age 30)
                                 '(:id 3 :name "Charlie" :age 35)
                                 '(:id 4 :name "David" :age 40)))

      ;; Test basic where clause
      (is (= 4 (sql-utils:row-count table)))
      (sql-utils:delete-where table :where "age > ?" :where-args '(35))
      (is (= 3 (sql-utils:row-count table)))

      ;; Test multiple conditions
      (sql-utils:delete-where table
                             :where "age >= ? AND name LIKE ?"
                             :where-args '(30 "B%"))
      (is (= 2 (sql-utils:row-count table)))

      ;; Test delete all
      (sql-utils:delete-where table)
      (is (= 0 (sql-utils:row-count table)))

      ;; Test with analyze flag
      (sql-utils:insert table '(:id 1 :name "Test" :age 25))
      (sql-utils:delete-where table :analyze t)
      (is (= 0 (sql-utils:row-count table))))))

(test update-basics
  (with-test-connection (conn)
    (let ((table (sql-utils:create-table conn "test_update"
                                        '(("id" . "INTEGER")
                                          ("name" . "TEXT")
                                          ("age" . "INTEGER"))
                                        :pk "id")))
      ;; Insert test data
      (sql-utils:insert table '(:id 1 :name "Alice" :age 25))

      ;; Test basic update
      (sql-utils:update table 1 :updates '(:name "Alicia" :age 26))
      (let ((row (sql-utils:lookup table 1)))
        (is (string= "Alicia" (getf row :|name|)))
        (is (= 26 (getf row :|age|))))

      ;; Test error on non-existent record
      (signals sql-utils:not-found-error
        (sql-utils:update table 999 :updates '(:name "Nobody")))

      ;; Test with conversions
      (sql-utils:update table 1
                        :updates '(:name "alice")
                        :conversions '(:name "upper(?)")
                        :alter t)
      (let ((row (sql-utils:lookup table 1)))
        (is (string= "ALICE" (getf row :|name|))))

      ;; Test compound primary key
      (let ((table2 (sql-utils:create-table conn "test_update_compound"
                                           '(("year" . "INTEGER")
                                             ("month" . "INTEGER")
                                             ("data" . "TEXT"))
                                           :pk '("year" "month"))))
        (sql-utils:insert table2 '(:year 2024 :month 1 :data "January"))
        (sql-utils:update table2 '(2024 1) :updates '(:data "Jan"))
        (let ((row (sql-utils:lookup table2 '(2024 1))))
          (is (string= "Jan" (getf row :|data|))))))))

(test transform-basics
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_transform"
                                   '(("id" . "INTEGER") ("name" . "TEXT")
                                     ("age" . "INTEGER"))
                                   :pk "id")))
      ;; Insert some test data
      (sql-utils:insert-all table
                            (list '(:id 1 :name "Alice" :age 30)
                                  '(:id 2 :name "Bob" :age 25)))
      ;; Assert table has the correct column names
      (is
       (not
         (set-difference (sql-utils:column-names table)
                         (list "id" "name" "age")
                         :test #'equal)))
      ;; Test renaming columns
      (sql-utils:transform table :rename '(("name" . "full_name")))
      (let ((cols (mapcar #'sql-utils:name (sql-utils:columns table))))
        (is (member "full_name" cols :test #'string=))
        (is (not (member "name" cols :test #'string=))))
      ;; Test changing column types
      (sql-utils:transform table :types '(("age" . "TEXT")))
      (let ((age-col
             (find "age" (sql-utils:columns table)
                   :key #'sql-utils:name
                   :test #'string=)))
        (is (string= "TEXT" (sql-utils:column-type age-col))))
      ;; Test dropping columns
      (sql-utils:transform table :drop '("age"))
      (let ((cols (mapcar #'sql-utils:name (sql-utils:columns table))))
        (is (not (member "age" cols :test #'string=))))
      ;; Verify data was preserved
      (let ((rows (squ:rows table)))
        (is (= 2 (length rows)))
        (is (string= "Alice" (getf (first rows) :|full_name|)))))))

(test add-column-basics
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_add_column"
                                   '(("id" . "INTEGER") ("name" . "TEXT"))
                                   :pk "id")))
      ;; Test adding a simple column
      (sql-utils:add-column table "age" :col-type "INTEGER")
      (let ((cols (mapcar #'sql-utils:name (sql-utils:columns table))))
        (is (member "age" cols :test #'string=)))
      ;; Test adding column with NOT NULL and default
      (sql-utils:add-column table "status"
                            :col-type "TEXT"
                            :not-null-default "active")
      (let ((status-col
             (find "status" (sql-utils:columns table)
                   :key #'sql-utils:name
                   :test #'string=)))
        (is (not (null status-col)))
        (is (string= "'active'" (sql-utils:default-value status-col))))
      ;; Test adding foreign key column
      (sql-utils:create-table conn "other_table"
                              '(("id" . "INTEGER") ("data" . "TEXT"))
                              :pk "id")
      (sql-utils:add-column table "other_id"
                            :col-type "INTEGER"
                            :fk "other_table")
      (let ((fks (sql-utils:foreign-keys table)))
        (is (= 1 (length fks)))
        (let ((fk (first fks)))
          (is (string= "other_id" (sql-utils:column fk)))
          (is (string= "other_table" (sql-utils:other-table fk)))
          (is (string= "id" (sql-utils:other-column fk)))))
      ;; Test adding foreign key with custom column
      (sql-utils:add-column table "parent_id"
                            :col-type "INTEGER"
                            :fk "test_add_column"
                            :fk-col "id")
      (let ((fk
             (find "parent_id" (sql-utils:foreign-keys table)
                   :key #'sql-utils:column
                   :test #'string=)))
        (is (string= "test_add_column" (sql-utils:other-table fk)))
        (is (string= "id" (sql-utils:other-column fk)))))))

(test add-foreign-key-basics
  (with-test-connection (conn)
    ;; Create the tables
    (let ((parent
           (sql-utils:create-table conn "parent"
                                   '(("id" . "INTEGER") ("name" . "TEXT"))
                                   :pk "id"))
          (child
           (sql-utils:create-table conn "child"
                                   '(("id" . "INTEGER")
                                     ("parent_id" . "INTEGER")
                                     ("name" . "TEXT"))
                                   :pk "id")))
      ;; Test adding a simple foreign key
      (sql-utils:add-foreign-key child "parent_id" :other-table "parent")
      (let ((fks (sql-utils:foreign-keys child)))
        (is (= 1 (length fks)))
        (let ((fk (first fks)))
          (is (string= "parent_id" (sql-utils:column fk)))
          (is (string= "parent" (sql-utils:other-table fk)))
          (is (string= "id" (sql-utils:other-column fk)))))
      ;; Test adding with custom referenced column
      (sql-utils:add-foreign-key child "other_parent_id"
                                 :other-table "parent"
                                 :other-column "id"
                                 :ignore t)
      ;; This actually creates the bad_column column, so no error raised.
      ;; XXX TODO evaluate if this is what we want to do.
      ;; Test error on non-existent column
      ;; (signals sql-utils:alter-error
      ;;   (sql-utils:add-foreign-key child "bad_column" :other-table "parent"))
      ;; Test error on non-existent other table
      (signals sql-utils:missing-db-type-error
        (sql-utils:add-foreign-key child "parent_id"
                                   :other-table "table_nonexistent"))
      ;; Test error on non-existent other column
      (signals sql-utils:alter-error
        (sql-utils:add-foreign-key child "parent_id"
                                   :other-table "parent"
                                   :other-column "col_nonexistent"))
      ;; Test ignore flag with existing foreign key
      (sql-utils:add-foreign-key child "parent_id"
                                 :other-table "parent"
                                 :ignore t)
      ;; Test error when adding duplicate without ignore
      (signals sql-utils:alter-error
        (sql-utils:add-foreign-key child "parent_id" :other-table "parent")))))

(test add-foreign-keys-multi
  (with-test-connection (conn)
    ;; Create the tables
    (let ((parent
           (sql-utils:create-table conn "parent"
                                   '(("id" . "INTEGER") ("name" . "TEXT"))
                                   :pk "id"))
          (other
           (sql-utils:create-table conn "other"
                                   '(("id" . "INTEGER") ("data" . "TEXT"))
                                   :pk "id"))
          (child
           (sql-utils:create-table conn "child"
                                   '(("id" . "INTEGER")
                                     ("parent_id" . "INTEGER")
                                     ("other_id" . "INTEGER")
                                     ("name" . "TEXT"))
                                   :pk "id")))
      (test drop-table-basics
        (with-test-connection (conn)
          ;; Create a test table
          (let ((table
                 (sql-utils:create-table conn "test_drop"
                                         '(("id" . "INTEGER")
                                           ("name" . "TEXT"))
                                         :pk "id")))
            ;; Verify table exists
            (is (sql-utils:exists-p table))
            (is
             (member "test_drop" (sql-utils:table-names conn) :test #'string=))
            ;; Drop the table
            (sql-utils:drop table)
            ;; Verify table no longer exists
            (is (not (sql-utils:exists-p table)))
            (is
             (not
               (member "test_drop" (sql-utils:table-names conn)
                       :test #'string=)))
            ;; Test dropping non-existent table raises error
            (signals dbi:dbi-error
              (sql-utils:drop table))
            ;; Test ignore flag prevents error
            (finishes
              (sql-utils:drop table :ignore t)))))
      ;; Test adding multiple foreign keys
      (sql-utils:add-foreign-keys child
                                  '(("parent_id" "parent" "id")
                                    ("other_id" "other" "id")))
      (let ((fks (sql-utils:foreign-keys child)))
        (is (= 2 (length fks)))
        ;; Verify first foreign key
        (let ((fk
               (find "parent_id" fks :key #'sql-utils:column :test #'string=)))
          (is (string= "parent" (sql-utils:other-table fk)))
          (is (string= "id" (sql-utils:other-column fk))))
        ;; Verify second foreign key
        (let ((fk
               (find "other_id" fks :key #'sql-utils:column :test #'string=)))
          (is (string= "other" (sql-utils:other-table fk)))
          (is (string= "id" (sql-utils:other-column fk))))))))

(test transform-with-foreign-keys
  (with-test-connection (conn)
    ;; Create parent and other tables
    (sql-utils:create-table conn "parent"
                            '(("id" . "INTEGER") ("name" . "TEXT"))
                            :pk "id")
    (sql-utils:create-table conn "other"
                            '(("id" . "INTEGER") ("data" . "TEXT"))
                            :pk "id")
    ;; Create child table with foreign key
    (let ((table
           (sql-utils:create-table conn "child"
                                   '(("id" . "INTEGER")
                                     ("parent_id" . "INTEGER")
                                     ("data" . "TEXT"))
                                   :pk "id"
                                   :foreign-keys '(("parent_id" "parent"
                                                    "id")))))
      ;; Test dropping foreign key
      (sql-utils:transform table :drop-foreign-keys '("parent_id"))
      ;; Verify foreign key was dropped
      (is (= 0 (length (sql-utils:foreign-keys table))))
      ;; Test adding foreign key back
      (sql-utils:transform table
                           :add-foreign-keys '(("parent_id" "parent" "id")))
      ;; Verify foreign key was added
      (is (= 1 (length (sql-utils:foreign-keys table))))
      (is
       (not
         (set-difference (list "parent_id") (sql-utils:fk-names table)
                         :test #'equal)))
      (sql-utils:transform table :drop-foreign-keys '("parent_id"))
      ;; Verify foreign key was dropped
      (is (= 0 (length (sql-utils:foreign-keys table))))
      ;; Test replacing all foreign keys
      (sql-utils:transform table
                           :foreign-keys '(("parent_id" "parent" "id")
                                           ("other_id" "other" "id")))
      ;; Verify foreign keys were replaced
      (is (= 2 (length (sql-utils:foreign-keys table))))
      (is
       (not
         (set-difference (list "parent_id" "other_id")
                         (sql-utils:fk-names table)
                         :test #'equal)))
      ;; Test error when mixing foreign-keys with add/drop
      (signals error
        (sql-utils:transform table
                             :foreign-keys '(("parent_id" "parent" "id"))
                             :add-foreign-keys '(("other_id" "other" "id")))))))

(defun test-create-table-execution ()
  (with-test-connection (conn)
    (let ((table
           (sql-utils:create-table conn "test_table"
                                   '(("id" . "INTEGER") ("name" . "TEXT"))
                                   :pk "id")))
      (setf *last-table* table)
      (assert (sql-utils:exists-p table))
      (assert (= 2 (length (sql-utils:columns table)))))))

;; (test-create-table-execution)
;; Predicates: is is-every is-false is-true signals finishes
;; (run! 'sql-utils-test.sql-utils-tests:sql-utils-suite-exists)
;; (run! 'sql-utils-test.sql-utils-tests::create-table-sql-if-not-exists)
;; (run! 'sql-utils-test.sql-utils-tests::create-table-with-foreign-key)
;; (run! 'sql-utils-test.sql-utils-tests::create-table-execution)
;; (run! 'sql-utils-test.sql-utils-tests::drop-table-basics)
;; (run! 'sql-utils-test.sql-utils-tests::row-count-basics)
;; (run! 'sql-utils-test.sql-utils-tests::rows-basics)
;; (run! 'sql-utils-test.sql-utils-tests::primary-key-basics)
;; (run! 'sql-utils-test.sql-utils-tests::insert-basics)
;; (run! 'sql-utils-test.sql-utils-tests::insert-all-basics)
;; (run! 'sql-utils-test.sql-utils-tests::lookup-basics)
;; (run! 'sql-utils-test.sql-utils-tests::update-basics)
;; (run! 'sql-utils-test.sql-utils-tests::add-foreign-key-basics)
;; (run! 'sql-utils-test.sql-utils-tests::add-column-basics)
;; (run! 'sql-utils-test.sql-utils-tests::transform-basics)
;; (run! 'sql-utils-test.sql-utils-tests::transform-with-foreign-keys)
;; (run! 'sql-utils-test.sql-utils-tests::default-value-in-table)
;; (ql:quickload '(:sql-utils :sql-utils/sqlite-utils :sql-utils/tests))
;; (run! 'sql-utils-test.sql-utils-tests:sql-utils-suite)
;; (sql-utils/sqlite-cli::run "create-table" "examples/test.db" "people" "id" "integer" "name" "text" "email" "text" "age" "integer" "--pk" "id" "--not-null" "name,email" "--default" "age=0,name=NONAME")
