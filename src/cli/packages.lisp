(defpackage :sql-utils/sqlite-cli
  (:use :cl)
  (:local-nicknames
   (:squ :sql-utils))
  (:import-from :clingon)
  (:import-from :sql-utils
                ;; Conditions
                :alter-error
                :not-found-error
                :missing-db-type-error
                ;; Classes
                :database
                :table
                :column
                :foreign-key
                ;; DB Accessors
                :connection
                :db
                :name
                ;; Table Accessors
                :columns
                :column-names
                :foreign-keys
                :pks
                :pk-names
                :fks
                :fk-names
                ;; Functions
                :schema
                :make-db-connection
                :execute
                :execute-script
                :sql-quote
                :quote-default-value
                :convert-foreign-keys
                :table-names
                :view-names
                :make-table
                :make-table-from-db
                :make-column
                :make-foreign-key
                :%exists-query
                :sync-from-db
                :exists-p
                :create
                :drop)
  (:export
   :main))
