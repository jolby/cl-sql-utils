(in-package :sql-utils)

;; Condition/Errors

(define-condition alter-error (error)
  ((message :initarg :message :reader error-message))
  (:report
   (lambda (condition stream)
     (format stream "Alter error: ~A" (error-message condition)))))

(define-condition not-found-error (error)
  ((message :initarg :message :reader error-message))
  (:report
   (lambda (condition stream)
     (format stream "Not found: ~A" (error-message condition)))))

(define-condition missing-db-type-error (error)
  ((type :initarg :type :reader error-type)
   (name :initarg :name :reader error-name)
   (context :initarg :context :reader error-context))
  (:report
   (lambda (condition stream)
     (format stream "Missing ~A '~A' in ~A" (error-type condition)
             (error-name condition) (error-context condition)))))

;; Column type definition

(defclass column ()
  ((cid :initarg :cid :accessor cid)
   (name :initarg :name :accessor name)
   (column-type :initarg :column-type :accessor column-type)
   (notnull :initarg :notnull :accessor notnull)
   (default-value :initarg :default-value :accessor default-value)
   (is-pk :initarg :is-pk :accessor is-pk)))

(defmethod print-object ((obj column) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A~A~A" (name obj) (column-type obj)
            (if (notnull obj)
                " NOT NULL"
                "")
            (if (is-pk obj)
                " PK"
                ""))))

;; ForeignKey type definition

(defclass foreign-key ()
  ((this-table :initarg :this-table :accessor this-table)
   (column :initarg :column :accessor column)
   (other-table :initarg :other-table :accessor other-table)
   (other-column :initarg :other-column :accessor other-column)))

(defmethod print-object ((obj foreign-key) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A.~A -> ~A.~A" (this-table obj) (column obj)
            (other-table obj) (other-column obj))))

;; Main Database class

(defclass database ()
  ((connection :initarg :connection
               :accessor connection
               :documentation "The underlying DBI database connection")
   (tracer :initform nil
           :accessor tracer
           :documentation "Optional function for tracing SQL execution")
   (use-counts-table :initform nil
                     :accessor use-counts-table
                     :documentation "Whether to use a _counts table for row counts"))
  (:documentation "Main database connection class that wraps a DBI connection.
Provides high-level methods for working with SQLite databases."))

(defmethod print-object ((obj database) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (connection obj))))

;; Queryable class (parent of Table and View)

(defclass queryable ()
  ((db :initarg :db :accessor db) (name :initarg :name :accessor name)))

(defmethod print-object ((obj queryable) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (name obj))))

;; Table class

(defclass table (queryable)
  ((pk :initarg :pk
       :accessor pk
       :documentation "Primary key column name(s) - can be a single name or list for compound keys")
   (foreign-keys :initarg :foreign-keys
                 :accessor foreign-keys
                 :documentation "List of foreign key constraint definitions")
   (columns :initarg :columns
            :accessor columns
            :documentation "List of column objects defining the table schema")
   (column-order :initarg :column-order
                 :accessor column-order
                 :documentation "Optional ordered list of column names")
   (not-null :initarg :not-null
             :accessor not-null
             :documentation "List of column names that cannot be NULL")
   (defaults :initarg :defaults
             :accessor defaults
             :documentation "Map of column names to their default values")
   (batch-size :initarg :batch-size
               :initform 100
               :accessor batch-size
               :documentation "Number of rows to insert in each batch operation")
   (hash-id :initarg :hash-id
            :accessor hash-id
            :documentation "If true, use a hash of row values as the primary key")
   (hash-id-columns :initarg :hash-id-columns
                    :accessor hash-id-columns
                    :documentation "List of columns to use for hash-id generation")
   (strict :initarg :strict
           :accessor strict
           :documentation "If true, apply STRICT mode constraints to the table")
   (last-pk :initarg :last-pk
            :accessor last-pk
            :documentation "The last primary key value inserted or updated in the table"))
  (:documentation "Represents a SQLite table and its schema.
Provides methods for querying and modifying both the table's structure and its data.
Tables should usually be created via db.table() rather than directly instantiated."))

(defmethod print-object ((obj table) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A (~D cols~@[, PK: ~A~])" (name obj)
            (length (columns obj)) (pks obj))))

;; Index type definition

(defclass index ()
  ((seq :initarg :seq :accessor seq)
   (name :initarg :name :accessor name)
   (unique :initarg :unique :accessor unique)
   (origin :initarg :origin :accessor origin)
   (partial :initarg :partial :accessor partial)
   (columns :initarg :columns :accessor columns)))

(defmethod print-object ((obj index) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A~A" (name obj) (columns obj)
            (if (unique obj)
                " UNIQUE"
                ""))))

;; XIndexColumn type definition

(defclass xindex-column ()
  ((seqno :initarg :seqno :accessor seqno)
   (cid :initarg :cid :accessor cid)
   (name :initarg :name :accessor name)
   (desc :initarg :desc :accessor desc)
   (coll :initarg :coll :accessor coll)
   (key :initarg :key :accessor key)))

(defmethod print-object ((obj xindex-column) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A~A" (name obj)
            (if (desc obj)
                " DESC"
                ""))))

;; XIndex type definition

(defclass xindex ()
  ((name :initarg :name :accessor name)
   (columns :initarg :columns :accessor columns)))

(defmethod print-object ((obj xindex) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A" (name obj) (columns obj))))

;; Trigger type definition

(defclass trigger ()
  ((name :initarg :name :accessor name)
   (table :initarg :table :accessor table)
   (sql :initarg :sql :accessor sql)))

(defmethod print-object ((obj trigger) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ON ~A" (name obj) (table obj))))

;; Constructor functions

(defun make-index (seq name unique origin partial columns)
  (make-instance 'index
                 :seq seq
                 :name name
                 :unique unique
                 :origin origin
                 :partial partial
                 :columns columns))

(defun make-xindex-column (seqno cid name desc coll key)
  (make-instance 'xindex-column
                 :seqno seqno
                 :cid cid
                 :name name
                 :desc desc
                 :coll coll
                 :key key))

(defun make-xindex (name columns)
  (make-instance 'xindex :name name :columns columns))

(defun make-trigger (name table sql)
  (make-instance 'trigger :name name :table table :sql sql))