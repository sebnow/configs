(defun end-of-line-then-newline-and-indent ()
	"Move to the end of the line and insert a new line and indent"
	(interactive "*")
	(end-of-line)
	(newline-and-indent))

(define-key global-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key global-map (kbd "M-RET") 'end-of-line-then-newline-and-indent)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(provide 'sn-bindings)
