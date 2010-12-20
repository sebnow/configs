(autoload 'paredit-mode "paredit"
  "Minor mode for peusdo-structurally editing Lisp code." t)

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook
	  (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook
	  (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook
	  (lambda () (paredit-mode +1)))
(add-hook 'inferior-scheme-mode-hook
	  (lambda () (paredit-mode +1)))
