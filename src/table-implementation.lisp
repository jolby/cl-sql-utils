(in-package :sql-utils)

(defmethod uses-rowid-p ((table table))
  "Returns T if this table uses rowid as its primary key (no other primary keys specified)"
  (not (some #'is-pk (columns table))))

(defmethod pks ((table table))
  "Get the primary key column names for this table"
  (let ((pk-columns (remove-if-not #'is-pk (columns table))))
    (if pk-columns
        (mapcar #'name pk-columns)
        '("rowid"))))

(defmethod initialize-instance :after ((table table) &key)
  ;; nothing yet...
  )

(defmethod default-values ((table table))
  "Returns a map of column names to their default values"
  (let ((defaults (make-hash-table :test 'equal)))
    (dolist (col (columns table))
      (when (default-value col)
        (setf (gethash (name col) defaults)
              (decode-default-value (default-value col)))))
    defaults))

(defmethod lookup ((table table) pk-values)
  "Return row (as property list) for the specified primary key.
   Raises 'not-found-error' if a matching row cannot be found.
   pk-values: A single value, or a list of values for tables that have a compound primary key."
  (unless (listp pk-values)
    (setf pk-values (list pk-values)))
  (let ((pks (pks table)))
    (when (/= (length pks) (length pk-values))
      (error "Need ~A primary key value~:P" (length pks)))
    (let* ((wheres (mapcar (lambda (pk) (format nil "[~A] = ?" pk)) pks))
           (sql
            (format nil "SELECT * FROM [~A] WHERE ~A" (name table)
                    (str:join " AND " wheres)))
           (prepared (dbi:prepare (connection (db table)) sql))
           (result (dbi:execute prepared pk-values))
           (row (dbi:fetch result)))
      (if row
          row
          (error 'not-found-error :message "Record not found")))))

(defmethod value-or-default ((table table) key default-value)
  (handler-bind ((not-found-error
                  (lambda (c)
                    (declare (ignore c))
                    (return-from value-or-default default-value))))
    (lookup table key)))

(defmethod create
           ((table table) columns
            &key pk foreign-keys column-order not-null defaults hash-id
            hash-id-columns extracts if-not-exists replace ignore transform
            strict)
  (declare
   (ignore columns pk foreign-keys column-order not-null defaults hash-id
    hash-id-columns extracts if-not-exists replace ignore transform strict))
  ;; Implementation needed
  )

(defun %build-column-mapping (table rename)
  "Build mapping of old column names to new names based on rename parameter"
  (let ((mapping (make-hash-table :test 'equal)))
    (dolist (col (columns table))
      (let* ((old-name (name col))
             (new-name
              (or (cdr (assoc old-name rename :test #'string=)) old-name)))
        (setf (gethash old-name mapping) new-name)))
    mapping))

(defun %build-column-types (table types)
  "Build mapping of column names to their new types"
  (let ((type-map (make-hash-table :test 'equal)))
    (dolist (col (columns table))
      (let* ((col-name (name col))
             (new-type
              (or (cdr (assoc col-name types :test #'string=))
                  (column-type col))))
        (setf (gethash col-name type-map) new-type)))
    type-map))

(defun %generate-transform-sql
       (table new-table-name types rename drop pk not-null defaults
        foreign-keys new-columns)
  "Generate SQL for creating the new transformed table"
  (let* ((col-mapping (%build-column-mapping table rename))
         (type-mapping (%build-column-types table types))
         (columns nil))
    ;; (format t "COLS BEFORE: ~A ~%" new-columns)
    ;; Build column definitions
    (dolist (col (columns table))
      (let* ((old-name (name col)) (new-name (gethash old-name col-mapping)))
        (unless (member old-name drop :test #'string=)
          (push (cons new-name (gethash old-name type-mapping)) columns))))
    ;; Add new columns
    (when new-columns
      (dolist (col new-columns) (push col columns)))
    ;; (format t "COLS AFTER: ~A ~%" columns)
    ;; Create the new table SQL
    (create-table-sql (db table) new-table-name (nreverse columns)
                      :pk (or pk (pks table))
                      :not-null not-null
                      :defaults defaults
                      :foreign-keys foreign-keys)))

(defun %generate-copy-sql (table new-table-name rename drop)
  "Generate SQL for copying data to the new table"
  (let* ((col-mapping (%build-column-mapping table rename))
         (cols
          (remove-if (lambda (name) (member name drop :test #'string=))
                     (mapcar #'name (columns table))))
         (new-cols (mapcar (lambda (c) (gethash c col-mapping)) cols)))
    (format nil "INSERT INTO [~A] (~{[~A]~^, ~})
                SELECT ~{[~A]~^, ~} FROM [~A]"
            new-table-name new-cols cols (name table))))

(defmethod transform
           ((table table)
            &key types rename drop pk not-null defaults drop-foreign-keys
            add-foreign-keys foreign-keys ignore column-order keep-table)
  "Transform the table schema by:
   - Renaming columns (:rename '((old-name . new-name)))
   - Changing column types (:types '((column-name . new-type)))
   - Dropping columns (:drop '(column-names))
   - Setting new primary key (:pk new-pk)
   - Setting NOT NULL constraints (:not-null '(column-names))
   - Setting column defaults (:defaults '((column-name . default-value)))
   - Dropping foreign keys (:drop-foreign-keys '(column-names))
   - Adding foreign keys (:add-foreign-keys '((column other-table other-column)))
   - Setting all foreign keys (:foreign-keys '((column other-table other-column)))"
  ;; (format t "~&TRANSFORM: ~A ~%" table)
  ;; Validate foreign key parameters
  (when foreign-keys
    (when (or add-foreign-keys drop-foreign-keys)
      (error "Cannot specify both foreign-keys and add/drop-foreign-keys")))
  ;; Build list of foreign keys for new table
  (let* ((new-fks 'nil) (new-columns 'nil))
    ;; Copy existing foreign keys unless being dropped
    (dolist (fk (foreign-keys table))
      (unless (and drop-foreign-keys
                   (member (column fk) drop-foreign-keys :test #'string=))
        (push
         (list
           (if rename
               (cdr (assoc (column fk) rename :test #'string=))
               (column fk))
           (other-table fk) (other-column fk))
         new-fks)))
    ;; Helper function to add missing columns
    (flet ((ensure-fk-columns (fks)
             (dolist (fk fks)
               (let ((col-name (first fk)))
                 (unless (find col-name (mapcar #'name (columns table))
                               :test #'string=)
                   (push (cons col-name "INTEGER") new-columns))))))
      ;; Add new foreign keys if specified
      (when add-foreign-keys
        (ensure-fk-columns add-foreign-keys)
        (dolist (fk add-foreign-keys) (push fk new-fks)))
      ;; Or use complete replacement set
      (when foreign-keys
        (ensure-fk-columns foreign-keys)
        (setf new-fks foreign-keys)))
    ;; (format t "NEW FKs: ~A ~%" new-fks)
    ;; (format t "NEW COLS: ~A ~%" new-columns)
    (let* ((new-table-name
            (format nil "~A_new_~A" (name table) (random 1000000)))
           (transform-sql
            (%generate-transform-sql table new-table-name types rename drop pk
                                     not-null defaults new-fks new-columns))
           (copy-sql (%generate-copy-sql table new-table-name rename drop))
           (drop-table-sql (format nil "DROP TABLE [~A]" (name table)))
           (rename-table-sql
            (format nil "ALTER TABLE [~A] RENAME TO [~A]" new-table-name
                    (name table))))
      ;; Generate and execute CREATE TABLE
      ;; (format t "TRANSFORM SQL: ~A ~%" transform-sql)
      (execute (db table) transform-sql)
      ;; Copy data
      ;; (format t "COPY SQL: ~A ~%" copy-sql)
      (execute (db table) copy-sql)
      ;; Drop old table and rename new one
      ;; (format t "DROP TABLE: ~A ~%" drop-table-sql)
      (execute (db table) drop-table-sql)
      ;; (format t "RENAME TABLE: ~A ~%" rename-table-sql)
      (execute (db table) rename-table-sql)
      ;; Refresh table object from DB
      (sync-from-db table))))

(defmethod create-index
           ((table table) columns
            &key index-name unique if-not-exists find-unique-name analyze)
  (declare
   (ignore columns index-name unique if-not-exists find-unique-name analyze))
  ;; Implementation needed
  )

(defmethod add-column
           ((table table) col-name
            &key col-type fk (fk-col "id") not-null-default)
  "Add a column to this table.
   col-name: Name of the new column
   col-type: Column type (defaults to TEXT)
   fk: Name of table this column should foreign key to
   fk-col: Column in foreign key table to reference (defaults to id)
   not-null-default: Set column as NOT NULL with this default value"
  ;; (format t "~&add-column: ~A ~%" col-name)
  (when fk
    ;; Validate foreign key table exists
    (unless (member fk (table-names (db table)) :test #'string=)
      (error 'alter-error
             :message (format nil "Table '~A' does not exist" fk)))
    ;; If fk-col specified, validate it exists
    (when fk-col
      (let ((other-table (make-table (db table) fk)))
        (unless (find fk-col (mapcar #'name (columns other-table))
                      :test #'string=)
          (error 'alter-error
                 :message (format nil "Table '~A' has no column ~A" fk
                                  fk-col))))))
  ;; Build the ALTER TABLE SQL
  (let* ((col-type (or col-type "TEXT"))
         (not-null-sql
          (when not-null-default
            (format nil " NOT NULL DEFAULT ~A"
                    (quote-default-value (db table) not-null-default))))
         (sql
          (format nil "ALTER TABLE [~A] ADD COLUMN [~A] ~A~A" (name table)
                  col-name col-type (or not-null-sql ""))))
    ;; Execute the ALTER TABLE to add column
    (execute (db table) sql)
    (sync-from-db table)
    ;; Add foreign key if specified - must be done in a separate transaction
    (when fk
      (let ((fk-col (or fk-col "id")))
        ;; Add the foreign key constraint using transform
        (add-foreign-key table col-name :other-table fk :other-column fk-col)
        (sync-from-db table))))
  table)

(defmethod delete-record ((table table) pk-values)
  "Delete row matching the specified primary key.
   pk-values: A single value, or a list of values for tables with compound primary key"
  (unless (listp pk-values)
    (setf pk-values (list pk-values)))
  ;; Verify the record exists first
  (lookup table pk-values)
  (let* ((wheres (mapcar (lambda (pk) (format nil "[~A] = ?" pk)) (pks table)))
         (sql
          (format nil "DELETE FROM [~A] WHERE ~A" (name table)
                  (str:join " AND " wheres))))
    ;; (format t "~&DELETE: ~A, WHERE: ~A~%" sql pk-values)
    (execute (db table) sql pk-values))
  table)

(defmethod delete-where ((table table) &key where where-args analyze)
  "Delete rows matching the where clause, or all rows if no where clause specified.
   where: SQL where clause fragment (e.g. \"id > ?\")
   where-args: Parameters for the where clause
   analyze: If true, run ANALYZE after deleting"
  (unless (exists-p table)
    (return-from delete-where table))
  (let ((sql (format nil "DELETE FROM [~A]~@[ WHERE ~A~]" (name table) where)))
    (execute (db table) sql where-args)
    (when analyze
      (analyze (db table))))
  table)

(defmethod update ((table table) pk-values &key updates alter conversions)
  "Update a row identified by pk-values with the specified updates.
   pk-values: Primary key value(s) identifying the row
   updates: Property list of column names and new values
   alter: If true, add any missing columns
   conversions: Property list of column names and SQL functions to apply"
  (unless (listp pk-values)
    (setf pk-values (list pk-values)))
  ;; Verify record exists
  (lookup table pk-values)
  (when updates
    (let (sets args)
      ;; Build SET clause and args
      (loop for (key value) on updates by #'cddr
            do (let ((col-name (string-trim ":" (string key))))
                 (push
                  (format nil "[~A] = ~A" col-name
                          (or (getf conversions key) "?"))
                  sets)
                 (push value args)))
      ;; Add WHERE clause conditions
      (let* ((wheres
              (mapcar (lambda (pk) (format nil "[~A] = ?" pk)) (pks table)))
             (sql
              (format nil "UPDATE [~A] SET ~A WHERE ~A" (name table)
                      (str:join ", " (nreverse sets))
                      (str:join " AND " wheres))))
        ;; Execute update
        (handler-case
            (dbi:with-transaction (connection (db table))
              (let* ((update-result
                      (execute (db table) sql
                               (append (nreverse args) pk-values)))
                     ;; (row-count (dbi:row-count update-result))
                     (row-count (dbi:row-count (connection (db table))))
                     ;; (first-row (dbi:fetch update-result))
                     ;; (rowcount (getf first-row :|rows_affected|))
                     )
                (format t "~&Update result: ~A, row-count: ~A~%" update-result
                        row-count)
                (assert (= row-count 1))))
          ;; Handle missing column error
          (dbi:dbi-error (e)
            (when (and alter (search "no such column" (princ-to-string e)))
              ;; Add missing columns and retry
              (add-missing-columns table (list updates))
              (update table pk-values :updates updates)))))))
  ;; Store last updated pk
  (setf (last-pk table)
          (if (= (length (pks table)) 1)
              (first pk-values)
              pk-values))
  table)

(defmethod drop ((table table) &key ignore)
  "Drop this table.
   ignore: Set to T to ignore the error if the table does not exist"
  (handler-case
      (execute (db table) (format nil "DROP TABLE [~A]" (name table)))
    (dbi:dbi-error (e)
      (unless ignore
        (error e)))))

(defmethod guess-foreign-column ((table table) other-table)
  (declare (ignore other-table))
  ;; Implementation needed
  )

(defmethod detect-fts ((table table))
  ;; Implementation needed
  )

(defmethod insert-chunk
           ((table table) alter extracts chunk all-columns hash-id
            hash-id-columns upsert pk)
  (declare
   (ignore alter extracts chunk all-columns hash-id hash-id-columns upsert pk))
  ;; Implementation needed
  )

(defmethod insert
           ((table table) record
            &key pk foreign-keys column-order not-null defaults hash-id
            hash-id-columns)
  "Insert a single record into the table."
  (insert-all table (list record)
              :pk pk
              :foreign-keys foreign-keys
              :column-order column-order
              :not-null not-null
              :defaults defaults
              :hash-id hash-id
              :hash-id-columns hash-id-columns
              :batch-size 1))

(defmethod insert-all
           ((table table) records
            &key pk foreign-keys column-order not-null defaults batch-size
            hash-id hash-id-columns upsert)
  "Insert multiple records into the table."
  (declare (ignorable upsert))
  (let ((batch-size (or batch-size (batch-size table) 100)))
    ;; Ensure the table exists
    (when (not (exists-p table))
      ;; Create the table if it does not exist
      (let ((columns (extract-column-types records)))
        (create table columns
                :pk pk
                :foreign-keys foreign-keys
                :column-order column-order
                :not-null not-null
                :defaults defaults
                :hash-id hash-id
                :hash-id-columns hash-id-columns)))
    ;; Get the column names from the records
    (let* ((all-columns (or column-order (get-all-columns records)))
           (placeholders (make-list (length all-columns) :initial-element "?"))
           (sql
            (format nil "INSERT INTO [~A] (~{[~A]~^, ~}) VALUES (~{~A~^, ~})"
                    (name table) all-columns placeholders))
           (stmt (dbi:prepare (connection (db table)) sql)))
      ;; (format t "ALL COLS: ~A ~%" all-columns)
      ;; (format t "INSERT: ~A ~%" sql)
      ;; Insert records in batches
      (dbi:with-transaction (connection (db table))
        (loop for batch in (split-into-batches records batch-size)
              do (dolist (record batch)
                   ;; (format t "REC: ~A ~%" record)
                   (let ((values
                          (mapcar
                            (lambda (col)
                              (let ((val (getf record col)))
                                ;; (getf record (intern (string-upcase col))))
                                ;; (format t "~&GETF: ~A, FROM: ~A. Result: ~A~%" col record val)
                                val))
                            all-columns)))
                     ;; (format t "VALS: ~A ~%" values)
                     (dbi:execute stmt values))))))))

(defmethod add-missing-columns ((table table) records)
  (declare (ignore records))
  ;; Implementation needed
  )

(defmethod analyze-column
           ((table table) column
            &key common-limit value-truncate total-rows most-common
            least-common)
  (declare
   (ignore column common-limit value-truncate total-rows most-common
    least-common))
  ;; Implementation needed
  )

(defmethod add-foreign-key
           ((table table) column &key other-table (other-column "id") ignore)
  "Add a foreign key constraint to the specified column.
Creates the column if it doesn't exist.
Raises alter-error if the referenced table/column don't exist.

column: The column to mark as a foreign key
other-table: The table it refers to (required)
other-column: The column on the other table (defaults to primary key of other-table)
ignore: Set to T to ignore if foreign key already exists"
  ;; (format t "~&add-foreign-key: COL: ~A OTHER-TABLE: ~A, OTHER-COL: ~A~%"
  ;; column other-table other-column)
  ;; If other-table not specified, it's an error
  (unless (and other-table (exists-p (make-table (db table) other-table)))
    (error 'missing-db-type-error
           :type "TABLE"
           :name other-table
           :context (format nil "other-table must be specified and exist: ~A"
                            other-table)))
  ;; If other-column not specified, detect primary key on other-table
  (unless other-column
    (let ((other-table-obj (make-table (db table) other-table)))
      (setf other-column (guess-foreign-column table other-table))))
  ;; Ensure column exists, create if necessary
  (unless (find column (mapcar #'name (columns table)) :test #'string=)
    (add-column table column :col-type "INTEGER"))
  ;; Check the other table and column exist
  (let ((other-table-obj (make-table (db table) other-table)))
    (unless (or (string= other-column "rowid")
                (find other-column (mapcar #'name (columns other-table-obj))
                      :test #'string=))
      (error 'alter-error
             :message (format nil "No such column: ~A.~A" other-table
                              other-column))))
  ;; Check we don't already have this foreign key. If we do and ignore is true, return.
  ;; Otherwise, signal an error.
  (dolist (fk (foreign-keys table))
    (when (and (string= (column fk) column)
               (string= (other-table fk) other-table)
               (string= (other-column fk) other-column))
      (if ignore
          (return-from add-foreign-key table)
          (error 'alter-error
                 :message (format nil
                                  "Foreign key already exists for ~A => ~A.~A"
                                  column other-table other-column)))))
  ;; Add the foreign key by transforming the table
  (transform table :add-foreign-keys `((,column ,other-table ,other-column)))
  table)

(defmethod add-foreign-keys
           ((table table) foreign-keys &rest kwargs &key ignore)
  "Add multiple foreign key constraints.
   foreign-keys should be a list of (column other-table other-column) lists.
   ignore: Set to T to ignore if any foreign key already exists"
  (transform table :add-foreign-keys foreign-keys :ignore ignore)
  table)
