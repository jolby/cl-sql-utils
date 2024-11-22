(in-package :sql-utils)

(defun parse-list-of-pairs-to-alist (lop-string
                                     &key
                                       (outer-delim #\,)
                                       (inner-delim #\=)
                                       (accumulator nil accumulator-p))
  "Parse a list of pairs string to an alist."
  (format t "LOP-STRING: ~a, accum-p: ~A~%" lop-string accumulator-p)
  (let ((pairs (str:split outer-delim lop-string)))
    (setf pairs (mapcar #'str:trim pairs))
    (flet ((parse-pair (pair)
             (let ((pair (str:split inner-delim pair)))
               (cons (str:trim (first pair)) (str:trim (second pair))))))
      (if accumulator-p
          ;; loop over pairs and accumulate in accumulator
            (loop for pair in pairs
                    do (push (parse-pair pair) accumulator)
                    finally (return accumulator))
            ;; Otherwise mapcar over pairs
            (mapcar #'parse-pair pairs)))))
;; (parse-list-of-pairs-to-alist "a=1,b = 2, c=3")
#+(or)
(let* ((accum (list))
       (defaults (list "age=0, name='John'")))
  (loop for default in defaults
        do (setf accum (parse-list-of-pairs-to-alist default
                                                      :accumulator accum))
        finally (return))
  accum)

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

(defun decode-default-value (value)
  "Decode a default value from its SQL representation"
  (cond
    ;; Handle NULL
    ((null value) nil)
    ;; Handle quoted strings
    ((and (stringp value)
          (char= (char value 0) #\')
          (char= (char value (1- (length value))) #\'))
     (subseq value 1 (1- (length value))))
    ;; Handle special SQLite keywords
    ((member value '("CURRENT_TIME" "CURRENT_DATE" "CURRENT_TIMESTAMP")
             :test #'string=)
     value)
    ;; Handle expressions
    ((and (stringp value)
          (char= (char value 0) #\())
     value)
    ;; Otherwise return as-is
    (t value)))

(defun check-default-values (table defaults current-defaults)
  "Check if table needs transformation due to default value differences.
   Returns T if transformation needed."
  (and defaults
       (let ((current (default-values table)))
         (not (every (lambda (def)
                      (string= (cdr def)
                             (gethash (car def) current)))
                    defaults)))))

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
