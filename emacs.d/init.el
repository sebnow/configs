(setq load-path (cons (concat user-emacs-directory "site-lisp") load-path))

;;; Inhibit startup messages, etc
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message (user-login-name))

;;; Some helpful visual cues
(show-paren-mode 1)
(column-number-mode 1)

;;; Backup in a central directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat user-emacs-directory "backups")))))

;;; Load all custom-* files.
(let ((custom-files (directory-files user-emacs-directory t "^custom-.*.elc?$" t)))
  (mapc (lambda (filename)
	  ; Remove extension to load byte-compiled files.
	  (load (file-name-sans-extension filename))) custom-files))
