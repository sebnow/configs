;;; Inhibit startup messages, etc
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message (user-login-name))

;;; Some helpful visual cues
(show-paren-mode 1)
(column-number-mode 1)

(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(unless (package-installed-p 'ayu-theme)
  (package-install 'ayu-theme))

(use-package ayu-theme
  :config (load-theme 'ayu-dark t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ayu-theme evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
