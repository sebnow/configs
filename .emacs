(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))

;; Configuration
(setq inhibit-splash-screen t)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Start emacs server for emacsclient
(server-start)

;; Backup in a central directory
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      auto-save-default nil)

(setq enable-local-variables t)

;; Global indentation preferences
(setq fill-column 80)
(setq indent-tabs-mode 1)
(setq tab-width 2)
(setq tab-stop-list (let ((stops '(2)))
	(while (< (car stops) fill-column)
		(setq stops (cons (+ tab-width (car stops)) stops)))
	(nreverse stops)))


;; Load configuration "modules"
(require 'sn-bindings)
(require 'sn-lang-modes)
(require 'sn-org)
; Local overrides if exist
(if (file-exists-p (expand-file-name "~/.emacs.d/sn-local.el"))
    (require 'sn-local))
