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
(add-hook 'text-mode-hook '(lambda () (flyspell-mode t)))

(global-set-key (kbd "C-k") ctl-x-map)
(global-set-key (kbd "C-x") nil)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-s") nil)
(global-set-key (kbd "C-k k") 'kill-this-buffer)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(setq package-user-dir "~/.emacs.d/var/elpa")
(setq package-selected-packages
      '(zenburn-theme
	helm
	zoom
	magit
	forge
	rust-mode
	ace-jump-mode
	lsp-mode))

;; (package-refresh-contents)
(package-initialize)
(package-install-selected-packages)

(load-theme 'zenburn t)
(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(0.618 . 0.618)))

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-p") #'helm-find-files)


(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "M-k") 'ace-jump-mode)

(require 'lsp)
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-log-io 't)
