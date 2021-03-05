;;; ...doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq display-line-numbers-type 'relative)

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      )

(setq vc-follow-symlinks t)

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list (quote (4 8))
              )

(map! :n "C-e" #'end-of-line
      :n "C-f" #'forward-char
      :n "C-b" #'backward-char
      :n "C-k" #'kill-line
      :gi "C-S-v" #'yank
      :n ";"   #'evil-ex
      :i "C-n" #'next-line
      :i "C-p" #'previous-line
      )

(setq +notmuch-mail-folder "~/mail/account.lionmail")
(setq sendmail-program "gmi")
(setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/mail/account.lionmail"))
(setq notmuch-fcc-dirs nil) ;; Don't save outgoing mail locally; let gmi fetch it back for me
(setq notmuch-archive-tags '("-inbox" "-unread"))

(set-popup-rule! "^\\*notmuch update" :side 'bottom :size 30 :ttl 0) ;; Update in pop-up window

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

(map! :map lean-mode-map
      :n "C-c C-d" #'lean-find-definition
      )


;; (setq doom-theme 'doom-molokai)

;; (after! lean-mode
;;       (set-company-backend! 'lean-mode nil)
;;       )
