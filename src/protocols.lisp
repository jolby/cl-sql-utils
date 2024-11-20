(in-package :sql-utils)

;;;; Database Protocol

(defgeneric database-p (database)
  (:method ((db database)) t)
  (:documentation "Returns T if OBJECT is a database object, NIL otherwise."))

(defgeneric make-db-connection (db-designator &rest args &key
                                &allow-other-keys))

(defgeneric open-p (database)
  (:method ((db database)) (dbi:ping (connection db)))
  (:documentation "Returns T if the database is open, NIL otherwise."))

(defgeneric register-function (database fn &key deterministic replace name))

(defgeneric execute (database sql &optional parameters))

(defgeneric executescript (database sql))

(defgeneric schema (database-object))

(defgeneric tables (database))

(defgeneric views (database))

(defgeneric triggers (database))

(defgeneric make-table (database table-name &rest kwargs))

(defgeneric make-table-from-db (db table-name &rest kwargs)
  (:documentation
   "Create a Table object by reading schema info from the database"))

(defgeneric sql-quote (database value))

(defgeneric quote-default-value (database value))

(defgeneric table-names (database &key fts4 fts5))

(defgeneric view-names (database))

(defgeneric create-table-sql (database table-name columns &key pk foreign-keys
                              column-order not-null defaults hash-id
                              hash-id-columns extracts if-not-exists replace
                              ignore transform strict))

(defgeneric create-table (database table-name columns &key pk foreign-keys
                          column-order not-null defaults hash-id
                          hash-id-columns extracts if-not-exists replace ignore
                          transform strict))

(defgeneric rename-table (database table-name new-name))

(defgeneric create-view (database view-name sql &key ignore replace))

(defgeneric analyze (database &optional name))

;;;; Queryable Protocol

(defgeneric exists-p (queryable))

(defgeneric row-count (queryable)
  (:documentation "Returns the total number of rows in this queryable object.")
  (:method ((q queryable)) (row-count-where q)))

(defgeneric row-count-where (queryable &rest args &key where where-args)
  (:documentation
   "Returns count of rows matching the where clause. If no where clause provided, counts all rows.")
  (:method ((q queryable) &rest args &key where where-args)
    (let* ((sql
            (format nil "SELECT COUNT(*) FROM [~A]~@[ WHERE ~A~]" (name q)
                    where))
           (prepared (dbi:prepare (connection (db q)) sql))
           (result (dbi:execute prepared where-args))
           (row (dbi:fetch result)))
      (getf row :|COUNT(*)| 0))))

(defgeneric rows (queryable &rest args &key order-by select limit offset)
  (:documentation
   "Returns all rows from this queryable object as a list of property lists.")
  (:method ((q queryable) &rest args &key order-by select limit offset)
    (apply #'rows-where (append (list q) args))))

(defgeneric rows-where (queryable &key where where-args order-by select limit
                        offset)
  (:documentation
   "Returns rows matching the where clause. Returns property lists for each row.")
  (:method ((q queryable) &rest args &key where where-args order-by select
            limit offset)
    (let* ((sql
            (with-output-to-string (s)
              (format s "SELECT ~A FROM [~A]" (or select "*") (name q))
              (when where
                (format s " WHERE ~A" where))
              (when order-by
                (format s " ORDER BY ~A" order-by))
              (when limit
                (format s " LIMIT ~A" limit))
              (when offset
                (format s " OFFSET ~A" offset))))
           (prepared (dbi:prepare (connection (db q)) sql))
           (result (dbi:execute prepared where-args)))
      (dbi:fetch-all result))))

(defgeneric columns (queryable))

(defgeneric column-names (queryable))

;; (defgeneric schema (queryable))

(defgeneric pks (queryable)
  (:documentation
   "Returns a list of column objects that make up the primary key"))

(defgeneric pk-names (queryable)
  (:documentation
   "Returns a list of column names that make up the primary key"))

(defgeneric fk-names (queryable)
  (:documentation "Returns a list of foreign key names"))

(defgeneric uses-rowid-p (queryable)
  (:documentation "Returns T if this table uses rowid as its primary key"))

(defgeneric lookup (table pk-values))

(defgeneric value-or-default (table key value))

(defgeneric delete-record (table pk-values)
  (:documentation "Delete row matching the specified primary key"))

(defgeneric delete-where (table &key where where-args analyze)
  (:documentation
   "Delete rows matching the where clause, or all rows if no where clause"))

(defgeneric update (table pk-values &key updates alter conversions)
  (:documentation
   "Update a row identified by pk-values with the specified updates"))

(defgeneric create (table columns &key pk foreign-keys column-order not-null
                          defaults hash-id hash-id-columns extracts
                          if-not-exists replace ignore transform strict))

(defgeneric sync-from-db (table)
  (:documentation "Synchronize the table object with the database schema"))

(defgeneric sync-to-db (table)
  (:documentation "Synchronize the database schema with the table object"))

(defgeneric transform (table &key types rename drop pk not-null defaults
                             drop-foreign-keys))

(defgeneric create-index (table columns &key index-name unique if-not-exists
                                find-unique-name analyze))

(defgeneric add-column (table col-name &key col-type fk fk-col
                              not-null-default))

(defgeneric add-missing-columns (table records))

(defgeneric add-foreign-key (table column &key other-table other-column ignore))

(defgeneric add-foreign-keys (table foreign-keys &rest kwargs))

(defgeneric drop (table &key ignore))

(defgeneric guess-foreign-column (table other-table))

(defgeneric detect-fts (table))

(defgeneric update (table pk-values &key updates alter conversions))

(defgeneric insert-chunk (table alter extracts chunk all-columns hash-id
                                hash-id-columns upsert pk))

(defgeneric insert (table record &key pk foreign-keys column-order not-null
                          defaults hash-id hash-id-columns))

(defgeneric insert-all (table records &key pk foreign-keys column-order
                              not-null defaults batch-size hash-id
                              hash-id-columns upsert))

(defgeneric analyze-column (table column &key common-limit value-truncate
                                  total-rows most-common least-common))