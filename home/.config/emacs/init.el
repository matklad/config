(setq custom-file (concat user-emacs-directory "/custom.el"))
(load custom-file 'noerror)
(setq inhibit-x-resources 't)
(setq inhibit-startup-screen t)
(setq frame-resize-pixelwise 't)
(setq default-frame-alist '((font . "JetBrains Mono-13") (vertical-scroll-bars . nil)))
(setq kill-whole-line 't)
(setq sentence-end-double-space nil)
(setq vc-follow-symlinks t)
(setq backup-directory-alist `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(fset 'yes-or-no-p 'y-or-n-p)
;; (set-frame-parameter nil 'fullscreen 'fullboth)
(set-face-attribute 'default nil :font "JetBrains Mono-13")
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Noto Emoji"))

;; (cua-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)
(show-paren-mode 1)
(auto-save-visited-mode t)
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


;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(setq package-user-dir "~/.cache/emacs/elpa")
(setq package-selected-packages
      '(zenburn-theme
	magit zoom
	counsel
	rust-mode
	zig-mode
	ace-jump-mode
	;; lsp-mode
	whitespace-cleanup-mode))

;; (package-refresh-contents)
(package-initialize)
(package-install-selected-packages)

(load-theme 'zenburn t)
(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(0.618 . 0.618)))


(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "M-k") 'ace-jump-mode)

;; (require 'lsp)
(add-to-list 'load-path "~/p/lsp-mode/")
(require 'lsp)
(require 'lsp-mode)
(load "~/p/lsp-mode/clients/lsp-rust.el")
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-log-io 't)
;; (global-set-key (kbd "C-w") 'lsp-extend-selection)
;; (global-set-key (kbd "<M-return>") 'lsp-execute-code-action)
;; (global-set-key (kbd "C-k r") 'lsp-rename)

(require 'git-commit)
(server-mode)

(global-whitespace-cleanup-mode)

(setq markdown-fontify-code-blocks-natively t)
(setq scroll-error-top-bottom t)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

