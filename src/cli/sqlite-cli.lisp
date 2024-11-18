(in-package :sql-utils/sqlite-cli)

(defun tables/options ()
  "Returns options for the tables command"
  (list
    (clingon:make-option :switch
                         :description "Just show FTS4 enabled tables"
                         :long-name "fts4"
                         :key :fts4)
    (clingon:make-option :switch
                         :description "Just show FTS5 enabled tables"
                         :long-name "fts5"
                         :key :fts5)
    (clingon:make-option :switch
                         :description "Include row counts per table"
                         :long-name "counts"
                         :key :counts)
    (clingon:make-option :switch
                         :description "Include list of columns for each table"
                         :long-name "columns"
                         :key :columns)
    (clingon:make-option :switch
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
         (squ:columns (clingon:getopt cmd :columns))
         (squ:schema (clingon:getopt cmd :schema)))
    ;; Get list of tables
    (let ((tables (squ:table-names db :fts4 fts4 :fts5 fts5)))
      (dolist (table tables)
        (format t "~A" table)
        (when counts
          (format t " (~A rows)" (squ:row-count (squ:make-table db table))))
        (when squ:columns
          (let ((cols
                 (mapcar #'squ:name (squ:columns (squ:make-table db table)))))
            (format t "~%  Columns: ~{~A~^, ~}" cols)))
        (when squ:schema
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

(defun top-level/sub-commands ()
  "Returns sub-commands for the top-level command"
  (list (tables/command) (rows/command)))

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

(defun main ()
  "Main entry point for the CLI"
  (let ((app (top-level/command)))
    (clingon:run app)))