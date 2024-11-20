(in-package :sql-utils/sqlite-cli)

(defun tables/options ()
  "Returns options for the tables command"
  (list
    (clingon:make-option :flag
                         :description "Just show FTS4 enabled tables"
                         :long-name "fts4"
                         :key :fts4)
    (clingon:make-option :flag
                         :description "Just show FTS5 enabled tables"
                         :long-name "fts5"
                         :key :fts5)
    (clingon:make-option :flag
                         :description "Include row counts per table"
                         :long-name "counts"
                         :key :counts)
    (clingon:make-option :flag
                         :description "Include list of columns for each table"
                         :long-name "columns"
                         :key :columns)
    (clingon:make-option :flag
                         :description "Include schema for each table"
                         :long-name "schema"
                         :key :schema)))

(defun tables/handler (cmd)
  "Handler for the tables command"
  (let* ((args (clingon:command-arguments cmd))
         (path (first args))
         (db (squ:make-db-connection :sqlite :filename path))
         (fts4 (clingon:getopt cmd :fts4))
         (fts5 (clingon:getopt cmd :fts5))
         (counts (clingon:getopt cmd :counts))
         (columns (clingon:getopt cmd :columns))
         (schema (clingon:getopt cmd :schema)))
    ;; Get list of tables
    (let ((tables (squ:table-names db :fts4 fts4 :fts5 fts5)))
      (dolist (table tables)
        (format t "~A" table)
        (when counts
          (format t " (~A rows)" (squ:row-count (squ:make-table db table))))
        (when columns
          (let ((cols
                 (mapcar #'squ:name (squ:columns (squ:make-table db table)))))
            (format t "~%  Columns: ~{~A~^, ~}" cols)))
        (when schema
          (format t "~%  Schema: ~A" (squ:schema (squ:make-table db table))))
        (format t "~%")))))

(defun tables/command ()
  "Creates the tables command"
  (clingon:make-command :name "tables"
                        :description "List the tables in the database"
                        :usage "DATABASE"
                        :options (tables/options)
                        :handler #'tables/handler
                        :examples '(("List tables:" .
                                     "sql-utils tables data.db"))))

(defun rows/options ()
  "Returns options for the rows command"
  (list
    (clingon:make-option :list
                         :description "Columns to return"
                         :short-name #\c
                         :long-name "column"
                         :key :columns)
    (clingon:make-option :string
                         :description "Optional where clause"
                         :long-name "where"
                         :key :where)
    (clingon:make-option :string
                         :description "Order by ('column' or 'column desc')"
                         :short-name #\o
                         :long-name "order"
                         :key :order)
    (clingon:make-option :integer
                         :description "Number of rows to return"
                         :long-name "limit"
                         :key :limit)
    (clingon:make-option :integer
                         :description "SQL offset to use"
                         :long-name "offset"
                         :key :offset)))

(defun rows/handler (cmd)
  "Handler for the rows command"
  (let* ((args (clingon:command-arguments cmd))
         (path (first args))
         (table-name (second args))
         (db (squ:make-db-connection :sqlite :filename path))
         (table (squ:make-table db table-name))
         (columns (clingon:getopt cmd :columns))
         (where (clingon:getopt cmd :where))
         (order (clingon:getopt cmd :order))
         (limit (clingon:getopt cmd :limit))
         (offset (clingon:getopt cmd :offset)))
    (let ((rows
           (squ:rows-where table
                           :where where
                           :order-by order
                           :limit limit
                           :offset offset
                           :select (when columns
                                     (format nil "~{~A~^, ~}" columns)))))
      (dolist (row rows) (format t "~A~%" row)))))

(defun rows/command ()
  "Creates the rows command"
  (clingon:make-command :name "rows"
                        :description "Output all rows in the specified table"
                        :usage "DATABASE TABLE"
                        :options (rows/options)
                        :handler #'rows/handler
                        :examples '(("Show all rows:" .
                                     "sql-utils rows data.db mytable")
                                    ("Filter rows:" .
                                     "sql-utils rows data.db mytable --where \"id > 5\""))))

(defun top-level/options () "Returns options for the top-level command" (list))

(defun insert/options ()
  "Returns options for the insert command"
  (list
    (clingon:make-option :switch
                         :description "Read newline-delimited JSON"
                         :long-name "nl"
                         :key :nl)
    (clingon:make-option :list
                         :description "Column(s) to use as primary key"
                         :long-name "pk"
                         :key :pk)))

(defun %maybe-convert-to-keyword (js-name)
           (or (find-symbol (string-upcase js-name) :keyword)
               js-name))

(defun insert/handler (cmd)
  "Handler for the insert command"
  (let* ((args (clingon:command-arguments cmd))
         (path (first args))
         (table-name (second args))
         (record-arg (third args))
         (db (squ:make-db-connection :sqlite :filename path))
         (table (squ:make-table db table-name))
         (pk (clingon:getopt cmd :pk))
         (nl (clingon:getopt cmd :nl))
         records)
    ;; (format t "~&INSERT ARGS: ~A~%" args)
    ;; (format t "~&RECORD ARG: ~A~%" record-arg)
    ;; First try parsing from command line argument
    (when record-arg
      (handler-case
          (setf records
                (let* ((yason:*parse-object-key-fn* #'%maybe-convert-to-keyword)
                       (json-data (yason:parse record-arg)))
                  ;; (format t "~&JSON DATA: ~A" json-data)
                  (if (hash-table-p json-data)
                      (alexandria:hash-table-plist json-data)
                      (mapcar #'alexandria:hash-table-plist json-data))))
        (error ()
          (setf records (read-from-string record-arg)))))

    ;; (format t "~&Records: ~A~%" records)
    
    ;; If no records yet, try reading from stdin
    (when (and (not records) (not (eq *standard-input* *terminal-io*)))
      (let ((input (read-line *standard-input* nil nil)))
        (when input
          (handler-case
              (let ((json-data (yason:parse input)))
                (setf records
                      (if (hash-table-p json-data)
                          (alexandria:hash-table-plist json-data)
                          (mapcar #'alexandria:hash-table-plist json-data))))
            (error ()
              (setf records (read-from-string input)))))))
    
    ;; Error if we couldn't get any records
    (unless records
      (error "No valid records provided. Supply records as argument or via stdin."))
    
    (format t "~&DEBUG: Parsed records: ~A~%" records)
    
    ;; Handle both single record and list of records
    (if (and (listp records)
             (every #'listp records)
             (not (keywordp (first records))))
        ;; Insert multiple records
        (squ:insert-all table records :pk pk)
        ;; Insert single record
        (squ:insert table records :pk pk))))

(defun insert/command ()
  "Creates the insert command"
  (clingon:make-command :name "insert"
                        :description "Insert records from standard input into a table"
                        :usage "DATABASE TABLE"
                        :options (insert/options)
                        :handler #'insert/handler
                        :examples '(("Insert a record:" .
                                   "echo '(:name \"test\")' | sql-utils insert data.db mytable"))))

(defun create-database/options ()
  "Returns options for the create-database command"
  (list
    (clingon:make-option :flag
                         :description "Enable WAL mode on the created database"
                         :long-name "enable-wal"
                         :key :enable-wal)))

(defun create-database/handler (cmd)
  "Handler for the create-database command"
  (let* ((args (clingon:command-arguments cmd))
         (path (first args))
         (enable-wal (clingon:getopt cmd :enable-wal))
         (db (squ:make-db-connection :sqlite :filename path)))
    (when enable-wal
      (squ:execute db "PRAGMA journal_mode=WAL"))
    (squ:execute db "VACUUM")))

(defun create-database/command ()
  "Creates the create-database command"
  (clingon:make-command :name "create-database"
                        :description "Create a new empty database file"
                        :usage "DATABASE"
                        :options (create-database/options)
                        :handler #'create-database/handler
                        :examples '(("Create new database:" .
                                   "sql-utils create-database data.db"))))

(defun create-table/options ()
  "Returns options for the create-table command"
  (list
    (clingon:make-option :list
                         :description "Column(s) to use as primary key"
                         :long-name "pk"
                         :key :pk)
    (clingon:make-option :list
                         :description "Columns that should be created as NOT NULL"
                         :long-name "not-null"
                         :key :not-null)
    (clingon:make-option :list
                         :description "Default value that should be set for a column"
                         :long-name "default"
                         :key :defaults)
    (clingon:make-option :list
                         :description "Column, other table, other column to set as a foreign key"
                         :long-name "fk"
                         :key :foreign-keys)
    (clingon:make-option :flag
                         :description "If table already exists, do nothing"
                         :long-name "ignore"
                         :key :ignore)
    (clingon:make-option :flag
                         :description "If table already exists, replace it"
                         :long-name "replace"
                         :key :replace)
    (clingon:make-option :flag
                         :description "If table already exists, try to transform the schema"
                         :long-name "transform"
                         :key :transform)
    (clingon:make-option :flag
                         :description "Apply STRICT mode to created table"
                         :long-name "strict"
                         :key :strict)))

(defun create-table/handler (cmd)
  "Handler for the create-table command"
  (let* ((args (clingon:command-arguments cmd))
         (path (first args))
         (table-name (second args))
         (column-defs (cddr args))
         (db (squ:make-db-connection :sqlite :filename path)))
    
    ;; Validate we have an even number of column name/type pairs
    (when (oddp (length column-defs))
      (error "Columns must be specified as name type pairs"))
    
    ;; Convert column definitions to alist
    (let* ((columns (loop for (name type) on column-defs by #'cddr
                         collect (cons name (string-upcase type))))
           ;; Get options
           (pk (clingon:getopt cmd :pk))
           (not-null (clingon:getopt cmd :not-null))
           (defaults (clingon:getopt cmd :defaults))
           (foreign-keys (clingon:getopt cmd :foreign-keys))
           (ignore (clingon:getopt cmd :ignore))
           (replace (clingon:getopt cmd :replace))
           (transform (clingon:getopt cmd :transform))
           (strict (clingon:getopt cmd :strict)))

      ;; Validate column types
      (dolist (col columns)
        (unless (member (cdr col) '("INTEGER" "TEXT" "FLOAT" "BLOB") :test #'string=)
          (error "Column types must be one of: INTEGER, TEXT, FLOAT, BLOB")))

      ;; Create the table
      (squ:create-table db table-name columns
                        :pk pk
                        :not-null not-null
                        :defaults defaults
                        :foreign-keys foreign-keys
                        :if-not-exists ignore
                        :replace replace
                        :transform transform
                        :strict strict))))

(defun create-table/command ()
  "Creates the create-table command"
  (clingon:make-command :name "create-table"
                        :description "Add a table with the specified columns"
                        :usage "DATABASE TABLE [column type]..."
                        :options (create-table/options)
                        :handler #'create-table/handler
                        :examples '(("Create table:" .
                                   "sql-utils create-table data.db people id integer name text --pk id"))))

(defun top-level/sub-commands ()
  "Returns sub-commands for the top-level command"
  (list (tables/command) (rows/command) (insert/command)
        (create-database/command) (create-table/command)))

(defun top-level/handler (cmd)
  "Handler for the top-level command"
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  "Returns the top-level command"
  (clingon:make-command :name "sql-utils"
                        :version "0.1.0"
                        :description "SQL database utility tool"
                        :long-description "A Common Lisp tool for working with SQL databases"
                        :authors '("Joel Boehland")
                        :license "MIT"
                        :handler #'top-level/handler
                        :options (top-level/options)
                        :sub-commands (top-level/sub-commands)))
(defun run (&rest args)
  "Main entry point for the CLI"
  (let ((app (top-level/command)))
    (clingon:run app args)))
;; (sql-utils/sqlite-cli::run "tables" "examples/test.db" "--schema=1 --columns=1")

(defun main ()
  "Main entry point for the CLI"
  (let ((app (top-level/command)))
    (clingon:run app)
    (clingon:exit)))
