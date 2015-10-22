;; turn off emacs interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; no startup message pls
(setq inhibit-startup-message t)

;; lets try using submodules + site-lisp for some things
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
;; and a settings directory
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; get those paths on the load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; keep emacs customizations seperated
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(setq ring-bell-function 'ignore)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(setq is-mac (equal system-type 'darwin))

(require 'setup-package)

(defun init--install-packages ()
  (packages-install
   '(ac-anaconda
     ag
     anaconda-mode
     cider
     clj-refactor
     clojure-mode
     clojure-mode-extra-font-locking
     css-eldoc
     dash
     dired-details
     dockerfile-mode
     elisp-slime-nav
     emmet-mode
     eproject
     expand-region
     f
     fill-column-indicator
     flx
     flx-ido
     flycheck
     flycheck-clojure
     flycheck-pos-tip
     groovy-mode
     guide-key
     handlebars-mode
     highlight-escape-sequences
     htmlize
     ido-at-point
     ido-vertical-mode
     js2-mode
     js2-refactor
     jump-char
     magit
     markdown-mode
     move-text
     multiple-cursors
     nodejs-repl
     paredit
     pony-mode
     prodigy
     projectile
     projectile-rails
     pyvenv
     rbenv
     restclient
     rhtml-mode
     rspec-mode
     scss-mode
     simple-httpd
     smart-forward
     smartparens
     smex
     spacegray-theme
     string-edit
     visual-regexp
     web-mode
     whitespace-cleanup-mode
     yaml-mode
     yasnippet
     yesql-ghosts
     )))


(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'appearance)
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Global whitespace cleanup mode please.
(global-whitespace-cleanup-mode)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
;; (eval-after-load 'org '(require 'setup-org))
;; (eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
;; (eval-after-load 'grep '(require 'setup-rgrep))
;; (eval-after-load 'shell '(require 'setup-shell))
(require 'setup-hippie)
(require 'setup-scss-mode)
(require 'setup-yasnippet)
(require 'setup-perspective)
(require 'setup-ffip)
(require 'setup-html-mode)
(require 'setup-paredit)
(require 'setup-web-mode)
(require 'setup-python-mode)
(require 'setup-pony-mode)
(require 'rbenv)
(global-rbenv-mode)
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'prodigy)
(global-set-key (kbd "C-x M-m") 'prodigy)

;; Font lock dash.el
                                        ; (eval-after-load "dash" '(dash-enable-font-lock))

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode
          ruby-mode
          markdown-mode
          groovy-mode
          scala-mode)
  (add-hook it 'turn-on-smartparens-mode))

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)


(autoload 'auto-complete-mode "auto-complete" nil t)
(eval-after-load 'flycheck '(require 'setup-flycheck))

;; Map files to modes
(require 'mode-mappings)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'multiple-cursors)
(require 'delsel)
(require 'jump-char)
(require 'eproject)
(require 'smart-forward)

;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

(require 'setup-projects)


(when is-mac (require 'mac))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
