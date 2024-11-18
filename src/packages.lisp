(defpackage :sql-utils
  (:use :cl)
  (:export ;; Conditions / Errors
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
           :tables
           ;; Table Accessors
           :pk
           :foreign-keys
           ;; Functions
           :database-p
           :open-p
           :make-db-connection
           :schema
           :table-names
           :view-names
           :make-table
           :make-table-from-db
           :execute
           :execute-script
           :sql-quote
           :quote-default-value
           :%exists-query
           :convert-foreign-keys
           ;; Tables
           :create-table-sql
           :create-table
           :sync-from-db
           :exists-p
           :row-count
           :row-count-where
           :rows
           :rows-where
           :lookup
           :value-or-default
           :create
           :drop
           :column
           :columns
           :column-names
           :pks
           :pk-names
           :fks
           :fk-names
           :use-rowid
           :insert
           :insert-all
           :add-column
           :transform
           :foreign-keys
           :other-table
           :other-column
           :add-foreign-key
           :add-foreign-keys
           :no-obvious-table
           ;; Views
           ;; Columns
           :cid
           :name
           :column-type
           :notnull
           :default-value
           :is-pk))