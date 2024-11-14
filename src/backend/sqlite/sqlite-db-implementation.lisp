(in-package :sql-utils/sqlite)

;; Sqlite database specialization marker class
(defclass sqlite-database (database) ())

(defmethod initialize-instance :after ((db sqlite-database) &key filename memory-name memory recreate)
  (let ((connection-spec `(:sqlite3 :database-name
                           ,(cond
                              (memory-name ":memory:")
                              (memory ":memory:")
                              (filename filename)
                              (t (error "Either specify a filename or :memory t or :memory-name NAME"))))))
    (when (and filename recreate (probe-file filename))
      (delete-file filename))
    (setf (connection db) (apply #'dbi:connect connection-spec)))
  (execute db "PRAGMA recursive_triggers=on;"))

(defmethod make-db-connection ((db-designator (eql :sqlite)) &rest args &key filename memory memory-name recreate)
  (declare (ignore args))
  (make-instance 'sqlite-database :filename filename :memory memory :memory-name memory-name :recreate recreate))

;; (defmethod register-function ((db database) fn &key deterministic replace name)
;;   "Register a function with the SQLite database.
;;    FN: The Common Lisp function to register
;;    DETERMINISTIC: If T, function always returns same result for same input
;;    REPLACE: If T, replace existing function
;;    NAME: Optional name for the function (defaults to function name)"
;;   (let ((fn-name (or name (symbol-name (function-name fn))))
;;         (det (if deterministic "DETERMINISTIC" "")))
;;     (execute db
;;             (format nil "CREATE ~A FUNCTION ~A ~A"
;;                     (if replace "OR REPLACE" "")
;;                     fn-name
;;                     det))
;;     fn-name))

(defmethod sql-quote ((db sqlite-database) value)
  "Quote a value for use in SQL queries"
  (typecase value
    (null "NULL")
    (string (format nil "[~A]" (cl-ppcre:regex-replace-all "'" value "''")))
    (number (princ-to-string value))
    (boolean (if value "1" "0"))
    (t (error "Cannot quote value of type ~A" (type-of value)))))

(defmethod quote-default-value ((db sqlite-database) value)
  "Quote a default value for use in SQL column definitions"
  (let ((str-value (princ-to-string value)))
    (cond
      ;; Already quoted string
      ((and (> (length str-value) 1)
            (or (and (char= (char str-value 0) #\')
                     (char= (char str-value (1- (length str-value))) #\'))
                (and (char= (char str-value 0) #\")
                     (char= (char str-value (1- (length str-value))) #\"))))
       str-value)
      ;; Special SQLite keywords
      ((member (string-upcase str-value)
               '("CURRENT_TIME" "CURRENT_DATE" "CURRENT_TIMESTAMP")
               :test #'string=)
       str-value)
      ;; Expression ending with )
      ((and (> (length str-value) 0)
            (char= (char str-value (1- (length str-value))) #\)))
       (format nil "(~A)" str-value))
      ;; Everything else gets quoted
      (t (%sqlite-quote db value)))))

(defmethod table-names ((db sqlite-database) &key fts4 fts5)
  (let (psql
        (sql "SELECT name FROM sqlite_master WHERE type='table'"))
    (when fts4
      (setf sql (concatenate 'string sql " AND sql like '%USING FTS4%'")))
    (when fts5
      (setf sql (concatenate 'string sql " AND sql like '%USING FTS5%'")))
    (setf psql (dbi:prepare (sql-utils:connection db) sql))
    (mapcar (lambda (row) (getf row :|name|)) (dbi:fetch-all (dbi:execute psql)))))

(defmethod view-names ((db sqlite-database))
  (let* ((sql "SELECT name FROM sqlite_master WHERE type='view'")
        (psql (dbi:prepare (sql-utils:connection db) sql)))
    (mapcar (lambda (row) (getf row :|name|)) (dbi:fetch-all (dbi:execute psql)))))

(defmethod make-table ((db sqlite-database) table-name &rest kwargs)
  "Get or create a Table object for the given table name.
   Returns a Table instance that can be used to interact with the table.
   If the table doesn't exist, it will be created when needed."
  (if (and (not (getf kwargs :force-new t))
           (%sqlite-table-exists-p (connection db) table-name))
      ;; Table exists - load from DB
      (make-table-from-db db table-name)
      ;; New table - create from kwargs
      (let ((table (make-instance 'sqlite-table
                                 :db db
                                 :name table-name
                                 :pk (getf kwargs :pk)
                                 :foreign-keys (convert-foreign-keys table-name (getf kwargs :foreign-keys))
                                 :column-order (getf kwargs :column-order)
                                 :not-null (getf kwargs :not-null)
                                 :defaults (getf kwargs :defaults)
                                 :hash-id (getf kwargs :hash-id)
                                 :hash-id-columns (getf kwargs :hash-id-columns)
                                 :strict (getf kwargs :strict t))))
        table)))

(defmethod make-table-from-db ((db sqlite-database) table-name &rest kwargs)
  "Create a Table object by reading schema info from the database"
  (let* ((table (make-instance 'sqlite-table
                              :db db
                              :name table-name))
         (columns (%get-columns table))
         (foreign-keys (%get-foreign-keys table)))
    (setf (slot-value table 'columns) columns)
    (setf (slot-value table 'foreign-keys) foreign-keys)
    table))
