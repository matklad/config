(setq custom-file (concat user-emacs-directory "/custom.el"))
(load custom-file 'noerror)
(setq inhibit-x-resources 't)
(setq inhibit-startup-screen t)
(setq frame-resize-pixelwise 't)
(setq default-frame-alist '((font . "JetBrains Mono-13") (vertical-scroll-bars . nil)))
(setq kill-whole-line 't)
(setq sentence-end-double-space nil)
(setq vc-follow-symlinks t)
(setq scroll-error-top-bottom t)
(setq markdown-fontify-code-blocks-natively t)
(setq backup-directory-alist `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
       `((".*" ,temporary-file-directory t)))

(fset 'yes-or-no-p 'y-or-n-p)
;; (set-frame-parameter nil 'fullscreen 'fullboth)
(set-face-attribute 'default nil :font "JetBrains Mono-13")
(set-fontset-font "fontset-default" nil
                   (font-spec :size 20 :name "Noto Emoji"))

;; (cua-mode t)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)
(show-paren-mode 1)
(auto-save-visited-mode t)
(fido-vertical-mode 1)
(add-hook 'imenu-after-jump-hook #'recenter-top-bottom)
(add-hook 'text-mode-hook '(lambda () (flyspell-mode t)))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(defun comment-or-uncomment-region-or-line ()
   "Comments or uncomments the region or the current line if there's no active region."
   (interactive)
   (let (beg end)
     (if (region-active-p)
 	 (setq beg (region-beginning) end (region-end))
       (setq beg (line-beginning-position) end (line-end-position)))
     (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "date --iso")))

(defun visit-init ()
  (interactive)
  (find-file "/home/matklad/config/home/.config/emacs/init.el"))


(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(custom-set-variables
 '(warning-suppress-log-types '((comp)))
 '(zoom-mode t)
 '(zoom-size '(0.618 . 0.618)))

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(setq package-user-dir "~/.cache/emacs/elpa")
(setq package-selected-packages
      '(zenburn-theme
	magit
	zoom
	rust-mode
	zig-mode
	multiple-cursors
	expand-region
	avy
	whitespace-cleanup-mode))

;; (package-refresh-contents)
(package-initialize)
(package-install-selected-packages)

(load-theme 'leuven t)

(global-whitespace-cleanup-mode t)

(define-key global-map (kbd "M-k") 'avy-goto-char-timer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

(require 'multiple-cursors)
(require 'expand-region)
(global-set-key (kbd "C-e") 'mc/mark-next-like-this-word)
(global-set-key (kbd "M-e") 'er/expand-region)

;; Blockers
;;   - 29 for treesit
;;   - treesit support in upstream expand-region
;;   - hierarchical imenu

(put 'dired-find-alternate-file 'disabled nil)
