(in-package :sql-utils)

(defun %fetch-one-row (db query)
  (let* ((pquery (dbi:prepare db query))
         (result (dbi:execute pquery)))
    (when result (dbi:fetch result :format :values))))

(defun %fetch-one-value (db query)
  (let ((row (%fetch-one-row db query)))
    (when row (car row))))

(defun %execute-and-fetch-all (conn sql &rest params)
  (dbi:fetch-all
   (dbi:execute (dbi:prepare conn sql) params)))

(defun %exists-query (db query)
  "Returns T if the query returns a result of 1, NIL otherwise. Assumes query is of
the form 'SELECT 1 WHERE...'"
  (let ((result (%fetch-one-value db query)))
    (when result
      (= result 1))))
