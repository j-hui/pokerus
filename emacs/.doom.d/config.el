;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      )

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list (quote (4 8))
              )
(setq vc-follow-symlinks t)

(setq evil-escape-key-sequence "kj")
(setq display-line-numbers-type 'relative)


(defun aza-delete-line ()
  "Delete from current position to end of line without pushing to `kill-ring'."
  (interactive)
  (delete-region (point) (line-end-position)))

(defun aza-delete-whole-line ()
  "Delete whole line without pushing to kill-ring."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(evilem-default-keybindings "C-f")

(map! ; examples at https://github.com/hlissner/doom-emacs/blob/11b514c8a13a7f327683a05c8cb70e1bf7bb2dc5/private/my-bindings.el
      "C-j" #'evil-scroll-down
      "C-k" #'evil-scroll-up
      "C-h" #'evil-beginning-of-line
      "C-l" #'evil-end-of-line

      :i "C-s" #'evil-escape
      :n "C-s" #'evil-write

      :n "x"   #'delete-forward-char
      :n "X"   #'delete-backward-char
      :v "x"   #'delete-forward-char
      :v "X"   #'delete-backward-char

      :n "D"   #'aza-delete-line
      :v "D"   #'delete-region
)
