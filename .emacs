(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; Configuration
(setq inhibit-splash-screen t)
(show-paren-mode 1)
(scroll-bar-mode -1)

;; Load configuration "modules"
(load "bindings.el")
