;; C
(add-hook 'c-mode-hook
	'(lambda ()
		(setq c-set-style "linux")
		(setq tab-width 4)
		(setq indent-tabs-mode 1)
		(setq fill-column 80)
		(setq c-basic-offset tab-width)
		(setq tab-stop-list (let ((stops '(4)))
			(while (< (car stops) fill-column)
				(setq stops (cons (+ tab-width (car stops)) stops)))
			(nreverse stops)))
		(setq c-tab-always-indent t)
		(setq comment-multi-line t)))

;; reStructuredText
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
(add-to-list 'auto-mode-alist '("\\.rest$" . rst-mode))
; Update inline TOC when section titles are adjusted
(add-hook 'rst-adjust-hook 'rst-toc-update)
; Set section decorations to use Python Documentation convention
(setq rst-preferred-decorations '((?# over-and-under 0)
	(?* over-and-under 0)
	(?= simple 0)
	(?- simple 0)
	(?^ simple 0)
	(?\" simple 0)))

(provide 'sn-lang-modes)
