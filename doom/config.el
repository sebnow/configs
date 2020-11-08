;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-theme 'doom-nord)

(setq org-agenda-files '("~/Documents/Org")
      org-agenda-start-day "-1d"
      org-agenda-start-on-weekday nil
      org-directory "~/Documents/Org"
      org-image-actual-width nil
      org-log-into-drawer "LOGBOOK"
      org-startup-with-inline-images t)

(add-to-list 'org-modules 'org-habit t)
(eval-after-load 'org
  '(org-load-modules-maybe t))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq display-line-numbers-type nil)
