(in-package :sql-utils)

(defun check-table-columns (table desired-columns existing-columns)
  "Check if table needs transformation due to column differences.
   Returns T if transformation needed."
  (let ((missing-columns
         (set-difference desired-columns existing-columns :test #'equal))
        (columns-to-drop
         (set-difference existing-columns desired-columns :test #'equal)))
    (or missing-columns columns-to-drop)))

(defun check-column-order (table column-order existing-columns)
  "Check if table needs transformation due to column order differences.
   Returns T if transformation needed."
  (and column-order
       (not
         (equal (subseq existing-columns 0 (length column-order))
                column-order))))

(defun check-primary-keys (table current-pks desired-pk)
  "Check if table needs transformation due to primary key differences.
   Returns T if transformation needed."
  (let ((desired-pks
         (if (stringp desired-pk)
             (list desired-pk)
             desired-pk)))
    (and desired-pks (not (equal current-pks desired-pks)))))

(defun check-not-null-constraints (table current-not-null desired-not-null)
  "Check if table needs transformation due to NOT NULL constraint differences.
   Returns T if transformation needed."
  (let ((current (remove-duplicates current-not-null :test #'equal))
        (desired (remove-duplicates (or desired-not-null nil) :test #'equal)))
    (not
      (and (subsetp current desired :test #'equal)
           (subsetp desired current :test #'equal)))))

(defun check-default-values (table defaults current-defaults)
  "Check if table needs transformation due to default value differences.
   Returns T if transformation needed."
  (and defaults (not (equal defaults current-defaults))))

(defun should-transform-table-p
       (table &key types column-order pk not-null defaults)
  "Check if table needs to be transformed based on various criteria.
   Returns T if any transformation is needed."
  (let* ((existing-columns (mapcar #'name (columns table)))
         (current-pks (pks table))
         (current-not-null
          (mapcar #'name (remove-if-not #'notnull (columns table))))
         (current-defaults (default-values table)))
    (or (check-table-columns table types existing-columns)
        (check-column-order table column-order existing-columns)
        (check-primary-keys table current-pks pk)
        (check-not-null-constraints table current-not-null not-null)
        (check-default-values table defaults current-defaults))))