(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq inhibit-x-resources 't)
(setq inhibit-startup-screen t)
(setq frame-resize-pixelwise 't)
(setq default-frame-alist '((font . "Fira Code-13") (vertical-scroll-bars . nil)))
(setq kill-whole-line 't)
(setq sentence-end-double-space nil)
(setq vc-follow-symlinks t)
(setq backup-directory-alist `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(fset 'yes-or-no-p 'y-or-n-p)
(set-frame-parameter nil 'fullscreen 'fullboth)
(set-face-attribute 'default nil :font "Fira Code 13")

(cua-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)
(show-paren-mode 1)
(auto-save-visited-mode t)

(global-set-key (kbd "C-k") ctl-x-map)
(global-set-key (kbd "C-x") nil)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-s") nil)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

(global-set-key (kbd "C-y") 'kill-whole-line)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(setq package-user-dir "~/.emacs.d/var/elpa")
(setq package-selected-packages
      '(zenburn-theme
	helm
	golden-ratio
	magit
	rust-mode
	ace-jump-mode))

;; (package-refresh-contents)
(package-initialize)
(package-install-selected-packages)

(load-theme 'zenburn t)
(golden-ratio-mode 1)

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-k C-f") #'helm-find-files)


(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "M-k") 'ace-jump-mode)
