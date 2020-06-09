;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq display-line-numbers-type 'relative)

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      )

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list (quote (4 8))
              )

(setq vc-follow-symlinks t)

(map! :n "C-e" #'end-of-line
      :n "C-f" #'forward-char
      :n "C-b" #'backward-char
      :i "C-n" #'next-line
      :i "C-p" #'previous-line
      )

(map! :map lean-mode-map
      :n "C-c C-d" #'lean-find-definition
      )

(after! lean-mode
      (set-company-backend! 'lean-mode nil)
      )
