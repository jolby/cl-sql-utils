(in-package :sql-utils)

;;;; Input record helpers
;;;; Input records are lists of key-value pairs (plists).

#+(or)
;; Example:

(let ((records
       (list '(:name "Bob" :age 25) '(:name "Charlie" :age 35)
             '(:name "David" :age 40)))))

(defun get-all-columns (input-records)
  "Return a list of all column names from the input-records."
  (let ((columns 'nil))
    (dolist (record input-records)
      ;; (dolist (key (loop for (k v) on record by #'cddr collect (string-downcase (symbol-name k))))
      (dolist
          (key
           (loop for (k v) on record by #'cddr
                 collect k))
        (unless (member key columns :test #'equal)
          (push key columns))))
    (nreverse columns)))

(defun extract-column-types (input-records)
  "Suggest column types based on the values in input-records."
  (let ((column-types (make-hash-table :test 'equal)))
    (dolist (record input-records)
      (loop for (key value) on record by #'cddr
            do (let* ((col (string-downcase (symbol-name key)))
                      (current-type (gethash col column-types))
                      (new-type (type-of value)))
                 (when (or (null current-type)
                           (subtypep new-type current-type))
                   (setf (gethash col column-types) new-type)))))
    (let ((columns 'nil))
      (maphash
        (lambda (k v)
          (push
           (cons k
                 (if (subtypep v 'integer)
                     "INTEGER"
                     "TEXT"))
           columns))
        column-types)
      (nreverse columns))))

(defun split-into-batches (list size)
  "Split LIST into batches of size SIZE."
  (let ((batches 'nil))
    (loop for i from 0 below (length list) by size
          do (push (subseq list i (min (length list) (+ i size))) batches))
    (nreverse batches)))