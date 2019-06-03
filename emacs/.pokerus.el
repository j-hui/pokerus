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

;; M-x package-install RET paradox
(require 'paradox)
(paradox-enable)

(paradox-require 'use-package)

;; (paradox-require 'nyx-theme)
(paradox-require 'color-theme-sanityinc-tomorrow)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reclaiming some editor sanity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      )

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list (quote (4 8))
              )
(setq vc-follow-symlinks t)

(define-key global-map (kbd "RET") 'newline-and-indent)

(paradox-require 'linum-relative)
(linum-relative-global-mode)
;;(setq linum-relative-backend 'display-line-numbers-mode)
(setq linum-relative-current-symbol "")

;;;;;;;;;;;;
;; Evil mode
;;;;;;;;;;;;
(paradox-require 'evil)
(evil-mode 1)

(defvar w 'evil-save)

(paradox-require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(paradox-require 'evil-leader)
(global-evil-leader-mode)
;; (evil-leader/set-leader ",")

(paradox-require 'evil-escape)
(evil-escape-mode)
(setq-default evil-escape-key-sequence "kj")
(define-key evil-insert-state-map "\C-c" 'evil-force-normal-state)
(define-key evil-insert-state-map "\C-d" 'evil-force-normal-state)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;;;;;;;;;;;;;;
;; Evil plugins
;;;;;;;;;;;;;;;

(paradox-require 'powerline)
(powerline-default-theme)

;;(powerline-evil-vim-color-theme)
;;(display-time-mode t)
(paradox-require 'airline-themes)
(load-theme 'airline-badwolf t)

(paradox-require 'evil-commentary)
(evil-commentary-mode)

(paradox-require 'evil-surround)
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(paradox-require 'evil-easymotion)
(evilem-default-keybindings "\C-f")

(paradox-require 'neotree)
(use-package neotree
  :ensure t
  :config

	(evil-leader/set-key
	"m"  'neotree-toggle
	"n"  'neotree-project-dir)

  (setq projectile-switch-project-action 'neotree-projectile-action)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
      (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

      (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
      (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))
