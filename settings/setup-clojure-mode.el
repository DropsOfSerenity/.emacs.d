(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(provide 'setup-clojure-mode)
