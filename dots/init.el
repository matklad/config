(setq inhibit-x-resources 't)
(setq frame-resize-pixelwise 't)

(set-frame-parameter nil 'fullscreen 'fullboth)

(require 'package)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(setq package-user-dir "~/.emacs.d/var/elpa")

(setq package-selected-packages
      '(zenburn-theme
	base16-theme
	helm
	magit
	paredit
	markdown-mode
	json-rpc
	haskell-mode
	smartparens
	flycheck
	clojure-mode
	inf-clojure
	projectile-ripgrep
	git-gutter
	crux
	cider
	rust-mode
	golden-ratio
	ace-jump-mode
	undo-tree
	back-button
        yaml-mode
        typescript-mode
        flycheck-rust
        emmet-mode
        multiple-cursors
        command-log-mode
        eglot
        ivy
        ivy-xref
        counsel
        adoc-mode
        fish-mode))

(package-initialize)
(package-refresh-contents)
(package-install-selected-packages)

(require 'eglot)
(add-to-list 'eglot-server-programs '(rust-mode . ("ra_lsp_server")))
(require 'ivy-xref)
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)



;; Basic
(cua-mode t)
(global-set-key (kbd "C-k") ctl-x-map)
(back-button-mode 1)
(global-undo-tree-mode)
(global-set-key (kbd "C-S-z")   'undo-tree-redo)
(global-set-key (kbd "M-<left>")    'back-button-local-backward)
(global-set-key (kbd "M-<right>")   'back-button-local-forward)
;; (global-set-key (kbd "C-k")   'magit-status)
(require 'paredit)
(define-key paredit-mode-map (kbd "C-k") nil)
;; (define-key paredit-mode-map (kbd "C-k") 'magit-status)

(defun finish-and-exit ()
  (interactive)
  (with-editor-finish 't)
  (save-buffers-kill-terminal))
(require 'magit)
(define-key git-commit-mode-map (kbd "C-x C-c") 'finish-and-exit)
(global-set-key (kbd "C-y")   'kill-whole-line)
(define-key paredit-mode-map (kbd "C-y") 'paredit-kill)
(require 'org)
(define-key org-mode-map (kbd "C-y") 'org-kill-line)
(global-set-key (kbd "C-S-j") 'crux-top-join-line)
(global-set-key (kbd "C-<prior>") 'beginning-of-buffer)
(global-set-key (kbd "C-<next>")  'end-of-buffer)
(setq kill-whole-line 't)
(setq sentence-end-double-space nil)

(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
(require 'helm-mode)
(setq helm-mode-fuzzy-match 't)

(setq backup-directory-alist `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq vc-follow-symlinks t)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "M-k") 'ace-jump-mode)

(setq initial-buffer-choice "~/work/work.adoc")
(setq-default indent-tabs-mode nil)
(setq scroll-margin 4)
(auto-save-visited-mode t)

;; Basic

;; UI
(defun switch-theme (name)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (customize-save-variable
   `custom-enabled-themes (list name)))
(golden-ratio-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)

(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :font "Fira Code 13")
(setq default-frame-alist '((font . "Fira Code-13") (vertical-scroll-bars . nil)))
;; UI

;; Non-essential keybindings
(require 'crux)
(require 'multiple-cursors)
(global-set-key (kbd "C-<f12>") 'helm-imenu)
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key (kbd "<home>") 'crux-move-beginning-of-line)
(global-set-key (kbd "M-j") 'mc/mark-next-like-this)
;; Non-essential keybindings

;; Modes
(show-paren-mode 1)
(smartparens-global-mode t)
(global-flycheck-mode)


(add-hook 'emacs-lisp-mode-hook  #'enable-paredit-mode)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'text-mode-hook '(lambda () (flyspell-mode t)))
(global-git-gutter-mode +1)
(setq python-shell-interpreter "python3")

;; Modes


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(ansi-term-color-vector
   [unspecified "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#d3d0c8"] t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages (quote (zenburn-theme base16-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values
   (quote
    ((bug-reference-bug-regexp . "\\(\\(?:[Ii]ssue \\|[Ff]ixe[ds] \\|[Rr]esolve[ds]? \\|[Cc]lose[ds]? \\|[Pp]\\(?:ull [Rr]equest\\|[Rr]\\) \\|(\\)#\\([0-9]+\\))?\\)")
     (eval add-hook
           (quote prog-mode-hook)
           (lambda nil
             (whitespace-mode 1))
           (not :APPEND)
           :BUFFER-LOCAL)
     (whitespace-style face lines indentation:space)
     (eval let*
           ((x
             (dir-locals-find-file default-directory))
            (this-directory
             (if
                 (listp x)
                 (car x)
               (file-name-directory x))))
           (unless
               (or
                (featurep
                 (quote swift-project-settings))
                (and
                 (fboundp
                  (quote tramp-tramp-file-p))
                 (tramp-tramp-file-p this-directory)))
             (add-to-list
              (quote load-path)
              (concat this-directory "utils")
              :append)
             (let
                 ((swift-project-directory this-directory))
               (require
                (quote swift-project-settings))))
           (set
            (make-local-variable
             (quote swift-project-directory))
            this-directory)))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
