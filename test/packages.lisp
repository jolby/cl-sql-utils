(defpackage :sql-utils-test.sql-utils-tests
  (:use :cl :fiveam)
  (:local-nicknames (:squ :sql-utils))
  (:export :sql-utils-suite :sql-utils-suite-exists :test-connection-1))

(defpackage :sql-utils-test.cli-tests
  (:use :cl :fiveam :alexandria)
  (:export #:cli-suite))

