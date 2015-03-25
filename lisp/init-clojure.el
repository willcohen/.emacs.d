(require-package 'cider)

;;; Clojure
;;; Be sure that Leiningen is installed.
;;; Add these lines to ~/.lein/profiles.clj
;;; {:user {:plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]}}

;;; Until leiningen is updated, also force nrepl 0.2.7
;;; {:user {:plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]
;;; :dependencies [[org.clojure/tools.nrepl "0.2.7"]]}}

(provide 'init-clojure)
