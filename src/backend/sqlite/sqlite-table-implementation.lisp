(in-package :sql-utils/sqlite)

;; Sqlite table specialization marker class

(defclass sqlite-table (table) ())

(defmethod initialize-instance :after ((table sqlite-table) &key)
  ;; nothing yet...
  )

(defmethod exists-p ((table sqlite-table))
  ;; (let ((sql (format nil "SELECT 1 FROM sqlite_master WHERE type='table' AND name='~A'" (name table))))
  ;;   (%exists-query (connection (db table)) sql))
  (%sqlite-table-exists-p (connection (db table)) (name table)))

(defmethod columns :before ((table sqlite-table))
  ;; Check if the slot is already bound and populated.
  ;; If not call %get-columns to populate it.
  ;; (format t "~&XXX columns :before!~%")
  (unless (slot-boundp table 'columns)
    (setf (slot-value table 'columns) (%get-columns table))))

(defmethod column-names ((table sqlite-table))
  "Get list of column names for this table"
  (mapcar #'name (columns table)))

(defmethod schema ((table sqlite-table))
  "Get the SQL schema for this table"
  (let* ((sql "SELECT sql FROM sqlite_master WHERE type='table' AND name=?")
         (prepared (dbi:prepare (connection (db table)) sql))
         (result (dbi:execute prepared (list (name table))))
         (row (dbi:fetch result)))
    (getf row :|sql|)))

(defmethod fk-names ((table sqlite-table))
  "Get list of column names that are foreign keys"
  (mapcar #'column (foreign-keys table)))

(defmethod sync-from-db ((table sqlite-table))
  ;; Refresh table object from DB
  (let ((refreshed-table (make-table-from-db (db table) (name table))))
    (setf (slot-value table 'columns) (slot-value refreshed-table 'columns)
          (slot-value table 'foreign-keys)
            (slot-value refreshed-table 'foreign-keys))
    table))