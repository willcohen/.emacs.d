;;; Set up sql-postgres mode to by default ask for ports as well

(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

;;; On Windows, use the Cygwin psql client instead (and be sure it is
;;; installed).

(when *is-my-desktop*
  (setq sql-postgres-program "C:/cygwin64/bin/psql.exe"))


(provide 'init-postgres)
