(in-package :sql-utils)

(defmethod execute ((db database) sql &optional parameters)
  (when (tracer db)
    (funcall (tracer db) sql parameters))
  (dbi:with-transaction (connection db)
    (let ((query (dbi:prepare (connection db) sql)))
      (if parameters
          (dbi:execute query parameters)
          (dbi:execute query)))))

(defmethod executescript ((db database) sql)
  (when (tracer db)
    (funcall (tracer db) sql nil))
  (dbi:with-transaction (connection db)
    (dbi:do-sql (connection db) sql)))

(defmethod analyze ((db database) &optional name)
  "Run ANALYZE on the database or a specific table.
   NAME: Optional table name to analyze"
  (if name
      (execute db (format nil "ANALYZE ~A;" name))
      (execute db "ANALYZE;")))

;; Constructor function for Column

(defun make-column (cid name type notnull default-value is-pk)
  (make-instance 'column
                 :cid cid
                 :name name
                 :column-type type
                 :notnull notnull
                 :default-value default-value
                 :is-pk is-pk))

;; Constructor function for ForeignKey

(defun make-foreign-key (this-table column other-table other-column)
  (make-instance 'foreign-key
                 :this-table this-table
                 :column column
                 :other-table other-table
                 :other-column other-column))

(defun convert-foreign-keys (table-name foreign-keys)
  "Convert list-style foreign keys to ForeignKey instances"
  (when foreign-keys
    (mapcar
      (lambda (fk)
        (destructuring-bind
            (column other-table &optional (other-column "id"))
            fk
          (make-foreign-key table-name column other-table other-column)))
      foreign-keys)))

(defmethod create-table-sql
           ((db database) table-name columns
            &key pk foreign-keys column-order not-null defaults hash-id
            hash-id-columns extracts if-not-exists replace ignore transform
            strict)
  "Generate SQL to create a table with the specified schema.
   Returns the SQL string needed to create the table."
  (declare
   (ignorable column-order hash-id hash-id-columns extracts ignore transform
    strict))
  (let* ((if-not-exists-sql
          (when if-not-exists
            " IF NOT EXISTS"))
         (replace-sql
          (when replace
            " OR REPLACE "))
         (column-defs 'nil)
         (pk-constraint nil)
         (fk-constraints 'nil))
    ;; Process columns and build column definitions
    (loop for (col-name . col-type) in columns
          for col-sql
              = (format nil "~A ~A~A~A" (sql-quote db col-name)
                        (if (stringp col-type)
                            col-type
                            "TEXT") ; Default to TEXT if no type specified
                        (if (and not-null
                                 (member col-name not-null :test #'string=))
                            " NOT NULL"
                            "")
                        (if (and defaults
                                 (assoc col-name defaults :test #'string=))
                            (format nil " DEFAULT ~A"
                                    (quote-default-value db
                                                         (cdr
                                                           (assoc col-name
                                                                  defaults
                                                                  :test #'string=))))
                            ""))
          do (push col-sql column-defs))
    ;; Handle primary key
    (when pk
      (setf pk-constraint
              (if (listp pk)
                  (format nil "PRIMARY KEY(~{~A~^, ~})"
                          (mapcar (lambda (col) (sql-quote db col)) pk))
                  (format nil "PRIMARY KEY(~A)" (sql-quote db pk)))))
    ;; Handle foreign keys
    ;; foreign keys are lists of (COLUMN OTHER-TABLE [OTHER-COLUMN])
    ;; if OTHER-COLUMN isn't provided, the other-column will be "id"
    (when foreign-keys
      (loop for fk in foreign-keys
            for (col other-table other-col) = fk
            for constraint
                = (format nil "FOREIGN KEY(~a) REFERENCES ~a(~a)"
                          (sql-quote db (car fk)) (sql-quote db other-table)
                          (sql-quote db (or other-col "id")))
            do (push constraint fk-constraints)))
    ;; Combine all parts into final SQL
    (format nil
            "CREATE~A TABLE~A ~A (~{~A~^, ~}~:[~;, ~]~A~:[~;, ~]~{~A~^, ~})"
            (or replace-sql "") (or if-not-exists-sql "")
            (sql-quote db table-name) (nreverse column-defs) pk-constraint
            pk-constraint (and fk-constraints t) (nreverse fk-constraints))))

(defmethod create-table
           ((db database) table-name columns
            &key pk foreign-keys column-order not-null defaults hash-id
            hash-id-columns extracts if-not-exists replace ignore transform
            strict)
  "Create a table in the database with the specified schema.
   Returns a Table instance for the newly created table."
  (let ((existing-table (make-table db table-name)))
    (cond ;; Table exists - handle according to flags
          ((exists-p existing-table)
           (cond (ignore existing-table)
                 (replace
                  (drop existing-table)
                  (create-table db table-name columns
                                :pk pk
                                :foreign-keys foreign-keys
                                :column-order column-order
                                :not-null not-null
                                :defaults defaults
                                :hash-id hash-id
                                :hash-id-columns hash-id-columns))
                 (transform
                  (when (should-transform-table-p existing-table
                                                  :types columns
                                                  :column-order column-order
                                                  :pk pk
                                                  :not-null not-null
                                                  :defaults defaults)
                    (transform existing-table
                               :types columns
                               :column-order column-order
                               :pk pk
                               :not-null not-null
                               :defaults defaults))
                  existing-table)
                 (t
                  ;; Create new table
                  (let ((sql
                         (create-table-sql db table-name columns
                                           :pk pk
                                           :foreign-keys foreign-keys
                                           :column-order column-order
                                           :not-null not-null
                                           :defaults defaults
                                           :hash-id hash-id
                                           :hash-id-columns hash-id-columns
                                           :extracts extracts
                                           :if-not-exists if-not-exists
                                           :replace replace
                                           :ignore ignore
                                           :transform transform
                                           :strict strict)))
                    (execute db sql)
                    (make-table db table-name
                                :pk pk
                                :foreign-keys foreign-keys
                                :column-order column-order
                                :not-null not-null
                                :defaults defaults
                                :hash-id hash-id
                                :hash-id-columns hash-id-columns)))))
          ;; Table doesn't exist - create it
          (t
           (let ((sql
                  (create-table-sql db table-name columns
                                    :pk pk
                                    :foreign-keys foreign-keys
                                    :column-order column-order
                                    :not-null not-null
                                    :defaults defaults
                                    :hash-id hash-id
                                    :hash-id-columns hash-id-columns
                                    :extracts extracts
                                    :if-not-exists if-not-exists
                                    :replace replace
                                    :ignore ignore
                                    :transform transform
                                    :strict strict)))
             (execute db sql)
             (make-table db table-name
                         :pk pk
                         :foreign-keys foreign-keys
                         :column-order column-order
                         :not-null not-null
                         :defaults defaults
                         :hash-id hash-id
                         :hash-id-columns hash-id-columns))))))