;; Org-mode
; org-agenda-files should be set in ~/emacs.d/sn-local.el
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Set default mode to org mode
(setq default-major-mode 'org-mode)
(setq org-log-done 'time) ; Timestamp on DONE

(provide 'sn-org)
