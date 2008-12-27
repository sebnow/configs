(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

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

;; Load configuration "modules"
(load "bindings")
(load "modes")
