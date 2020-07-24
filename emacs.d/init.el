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

(setq viper-mode t)
(require 'viper)

(use-package ayu-theme
  :config (load-theme 'ayu-dark t))
