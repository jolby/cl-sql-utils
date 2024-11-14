(in-package :sql-utils/sqlite)

(defun %sqlite-quote (db value)
  (let* ((query "SELECT quote(?)")
         (prepared (dbi:prepare (connection db) query))
         (result (dbi:execute prepared (list value)))
         (row (dbi:fetch result :format :values)))
    (first row)))

(defun %sqlite-table-exists-p (db table-name)
  (let* ((sql (format nil
                      "SELECT 1 FROM sqlite_master WHERE type='table' AND name='~A'"
                      table-name)))
    (%exists-query db sql)))

(defun %sqlite-view-exists-p (db view-name)
  (let* ((sql (format nil
                      "SELECT 1 FROM sqlite_master WHERE type='view' AND name='~A'"
                      view-name)))
    (%exists-query db sql)))

(defun %table-info (table)
  (if (not (exists-p table))
      (error "Table ~A does not exist in the database" (name table))
      (let* ((sql (format nil "PRAGMA table_info('~A')" (name table)))
             (prepared (dbi:prepare (connection (db table)) sql))
             (results (dbi:execute prepared))
             (rows (dbi:fetch-all results)))
        rows)))

(defun %get-columns (table)
  "Returns a list of Column objects representing the columns in this table"
  (if (not (exists-p table))
      (error 'missing-db-type-error
             :type "TABLE"
             :name (name table)
             :context (format nil "%get-columns. DB: ~A. Table: ~A" (db table) table))
      (let* ((rows (%table-info table)))
        (mapcar (lambda (row)
                  (make-column
                   (getf row :|cid|)
                   (getf row :|name|)
                   (getf row :|type|)
                   (= 1 (getf row :|notnull|))
                   (getf row :|dflt_value|)
                   (<= 1 (getf row :|pk|))))
                rows))))

(defun %get-foreign-keys (table)
  "Get foreign key definitions from the database for this table"
  (let* ((sql (format nil "PRAGMA foreign_key_list('~A')" (name table)))
         (prepared (dbi:prepare (connection (db table)) sql))
         (results (dbi:execute prepared))
         (rows (dbi:fetch-all results)))
    (mapcar (lambda (row)
              (make-foreign-key
               (name table)
               (getf row :|from|)
               (getf row :|table|)
               (getf row :|to|)))
            rows)))
