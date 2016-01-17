(setq enable-local-eval t)

(setq-default indent-tabs-mode nil
              tab-width 4
              case-fold-search t
              default-directory "~"
              fill-column 80)

(delete-selection-mode t)

(require 'use-package)
(use-package diminish)
(use-package bind-key)

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face trailing lines-tail
                                space-before-tab
                                indentation space-after-tab)
        whitespace-line-column 80))

(setq-default next-line-add-newlines nil
              require-final-newline t
              kill-whole-line t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(setq default-input-method 'russian-computer)


(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))


(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (cache-file-name "saveplace")))


(use-package savehist
  :config
  (setq savehist-additional-variables
        '(search ring regexp-search-ring)
        savehist-autosave-interval 30
        savehist-file (cache-file-name "savehist"))
  (savehist-mode t))


(use-package recentf
  :init
  (setq recentf-save-file (cache-file-name "cache/recentf")
        recentf-max-saved-items 50
        recentf-max-menu-items 10)
  :config
  (recentf-mode t))


(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))


(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode 1))


(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)


(use-package tramp)


(add-hook 'text-mode-hook 'turn-on-auto-fill)


(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  (define-key company-mode-map (kbd "C-M-i") 'company-complete)
  (setq company-require-match "Off"
        company-clang-arguments '("-std=c++11")
        company-idle-delay 0
        company-show-numbers t))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line))
(global-set-key (kbd "M-/") 'hippie-expand)


(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode))


(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t
        helm-echo-input-in-header-line t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b")   'helm-for-files)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (helm-mode)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-use-git-grep t
        projectile-keymap-prefix (kbd "C-c p")
        projectile-cache-file (cache-file-name "projectile.cache")
        projectile-known-projects-file (cache-file-name "projectile-bookmarks.eld"))
  (projectile-global-mode))


(use-package helm-projectile
  :ensure t
  :config
  (setq helm-for-files-preferred-list
        '(helm-source-buffers-list
          helm-source-projectile-files-list
          helm-source-recentf
          helm-source-bookmarks
          helm-source-file-cache
          helm-source-files-in-current-dir
          helm-source-locate))
  (helm-projectile-on)
  (define-key projectile-command-map (kbd "s g") 'projectile-grep))


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/cache/undo/"))))
  (defalias 'redo 'undo-tree-redo)

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))


(use-package magit
  :ensure t
  :pin melpa-stable
  :bind ("M-k" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"
        magit-pull-arguments (quote ("--rebase"))))


(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config (global-anzu-mode t))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


(use-package move-text
  :ensure t
  :bind (("C-S-<up>" . move-text-up)
         ("C-S-<down>" . move-text-down)))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))


(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode t))

(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook '(lambda () (flyspell-mode t))))


(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-word-1))

(use-package buffer-move
  :ensure t)

(defun ace-window-golden-rato (arg)
  (interactive "p")
  (ace-window arg)
  (golden-ratio))

(use-package ace-window
  :ensure t
  :config
  :bind ("C-x o" . ace-window-golden-rato))

(winner-mode t)


(use-package god-mode
  :ensure t)


(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))


(use-package neotree
  :ensure t)


(use-package shrink-whitespace
  :ensure t
  :bind ("M-SPC" . shrink-whitespace))


(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]))
  (global-unset-key k))

(use-package langtool
  :ensure t
  :config
  (setq langtool-java-classpath   "/usr/share/languagetool:/usr/share/java/languagetool/*"
        langtool-mother-tongue "ru"
        langtool-disabled-rules '("WHITESPACE_RULE")))


(setq initial-major-mode 'text-mode)

(setq initial-scratch-message "\
Latency Comparison Numbers
--------------------------
L1 cache reference                            0.5 ns
Branch mispredict                             5   ns
L2 cache reference                            7   ns             14x L1 cache
Mutex lock/unlock                            25   ns
Syscall                                      50   ns
Main memory reference                       100   ns             20x L2 cache, 200x L1 cache
Compress 1K bytes with Zippy              3,000   ns
Send 1K bytes over 1 Gbps network        10,000   ns    0.01 ms
Context switch                           30,000   ns    0.03 ms  300x memory reference
Read 4K randomly from SSD*              150,000   ns    0.15 ms
Read 1 MB sequentially from memory      250,000   ns    0.25 ms
Round trip within same datacenter       500,000   ns    0.5  ms
Read 1 MB sequentially from SSD*      1,000,000   ns    1    ms  4X memory
Disk seek                            10,000,000   ns   10    ms  20x datacenter roundtrip
Work quant                           10,000,000   ns   10    ms
Read 1 MB sequentially from disk     20,000,000   ns   20    ms  80x memory, 20X SSD
Send packet CA->Netherlands->CA     150,000,000   ns  150    ms")
