;; echo '(load "~/.pokerus.el")' >> ~/.emacs

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; M-x package-install RET proof-general

;; M-x package-install RET paradox
(require 'paradox)
(paradox-enable)

(paradox-require 'color-theme)
(color-theme-initialize)
(color-theme-hober)

(paradox-require 'evil)
(evil-mode 1)

(paradox-require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(paradox-require 'evil-leader)
(global-evil-leader-mode)
;; (evil-leader/set-leader ",")

;;(paradox-require 'powerline-evil)
;;(powerline-evil-vim-color-theme)
;;(display-time-mode t)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      )

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list (quote (4 8))
              )

(paradox-require 'linum-relative)
(linum-relative-global-mode)
(setq linum-relative-backend 'display-line-numbers-mode
      linum-relative-current-symbol ""
      )
