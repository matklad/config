(require 'use-package)

(defun turn-on-whitespace ()
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'turn-on-whitespace)

(which-function-mode 't)


(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-clang-language-standard "c++11")
  (global-flycheck-mode))

(setq compilation-ask-about-save  nil)

;; Python


(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package f
  :ensure t)

(defun pyenv-from-file ()
  (let ((current-file (buffer-file-name))
        (file-name ".python-version"))
    (when current-file
      (let* ((conf-dir (locate-dominating-file current-file file-name))
             (conf-file (concat conf-dir file-name)))
        (pyenv-mode-set
         (string-trim (f-read conf-file)))))))

(use-package pyenv-mode
  :ensure t
  :config
  (pyenv-mode)
  (add-hook 'python-mode-hook 'pyenv-from-file))

(defun electric-python-hook ()
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local))

(add-hook 'python-mode-hook 'electric-python-hook)


;; Lisp



(use-package paredit
  :diminish paredit-mode
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  (add-hook 'racket-mode-hook           'enable-paredit-mode)
  (add-hook 'paredit-mode-hook '(lambda () (setq kill-whole-line nil))))

(use-package rainbow-delimiters
  :ensure t)

(use-package paren-face
  :ensure t
  :config
  (global-paren-face-mode)
  (push 'racket-mode paren-face-modes)
  (custom-set-faces
   '(parenthesis ((t (:foreground "#DCDCCC"))))))

(global-prettify-symbols-mode)

(defun prettify-lambda ()
  (push '("lambda"  . ?λ) prettify-symbols-alist))

(defun my-racket-hook ()
  (prettify-lambda))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook
            'my-racket-hook)
  (sp-local-pair 'racket-mode "'" nil :actions nil)
  (sp-local-pair 'racket-mode "`" nil :actions nil))

(defun my-clojure-hook ()
  (enable-paredit-mode)
  (setq company-idle-delay 0)
  (clj-refactor-mode t)
  (cljr-add-keybindings-with-prefix "C-c C-m"))


(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'my-clojure-hook))


(use-package clj-refactor
  :ensure t)


(defun align-schema ()
  "Nicely aligns :-"
  (interactive)
  (save-excursion
    (let ((beg (search-backward "["))
          (end (search-forward "]")))
      (align-regexp beg end "\\(\\s-*\\):"))))


(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (define-key cider-mode-map (kbd "C-M-i") 'company-complete)
  (define-key cider-mode-map (kbd "C-c :") 'align-schema)
  (setq cider-stacktrace-default-filters '(java repl tooling dup)
        cider-prompt-for-symbol nil)
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'forward-error 1)
  (put-clojure-indent 'try-> 1)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (font-lock-add-keywords
                nil
                '(("(\\(try->\\)\\s-+"
                   (1 font-lock-keyword-face))))
             (font-lock-add-keywords
                nil
                '(("(\\(catch->\\)\\s-+"
                   (1 font-lock-keyword-face))))))


(defun cljs-repl ()
  "activates clojurescript repl in CIDER"
  (interactive)
  (nrepl-sync-request:eval "(require '[cemerick.piggieback :as piggieback])
    (require '[cljs.repl.node :as node])
    (piggieback/cljs-repl (node/repl-env))"))


;; C/C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defconst my-cc-style
  '("bsd"
    (c-basic-offset . 4)
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-style" my-cc-style)

(setq-default c-default-style "my-cc-style")

;; OpenGL

(use-package glsl-mode
  :ensure t)

;; Ocaml

(use-package tuareg
  :ensure t
  :config
  (define-key tuareg-mode-map (kbd "C-c C-c") 'smart-compile)
  (sp-local-pair 'tuareg-mode "'" nil :actions nil)
  (setq compilation-auto-jump-to-first-error 't))

(use-package utop
  :ensure t)

(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
  (setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode)
                  ("\\.topml$" . tuareg-mode))
                auto-mode-alist))
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
  (add-hook 'tuareg-mode-hook 'merlin-mode))


;; (use-package flycheck-ocaml
;;   :ensure t
;;   :config
;;   (setq merlin-error-after-save nil)
;;   (flycheck-ocaml-setup))

;; Haskell

(use-package hi2
  :ensure t
  :config
  (setq hi2-show-indentations nil)
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (yas-minor-mode -1)
               (turn-on-hi2))))

(defun hi2-smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (hi2-newline-and-indent)
  (recenter))

(use-package haskell-mode
  :ensure t
  :config
  (define-key haskell-mode-map (kbd "M-o") 'hi2-smart-open-line)
  (setq flycheck-ghc-args '("-Wall" "-fno-warn-name-shadowing")))


;; Latex

(defun run-latex ()
  (interactive)
  (let ((TeX-save-query nil)) (TeX-save-document ""))
  (TeX-command-menu "LaTeX"))

(defun toggle-formula ()
  (interactive)
  (if (eq nil (search-forward "\\)" (+ (point) 4) 't 1))
      (progn
        (insert "\\(  \\)")
        (backward-char)
        (backward-char)
        (backward-char)
        (deactivate-input-method))
    (toggle-input-method)))


(defun latex-hook ()
  (local-set-key (kbd "C-x C-s") 'run-latex)
  (local-set-key (kbd "C-c C-f") 'toggle-formula)
  (flychek-mode nil)
  (ispell-change-dictionary "ru-yo"))

(use-package tex
  :ensure auctex
  :config
  (setq-default TeX-engine 'default
                TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'latex-hook))

;; Web

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package emmet-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (sp-local-pair 'web-mode "{{" "}}")
  (sp-local-pair 'web-mode "{%" "%}" :insert "C-c b"))

(use-package coffee-mode
  :ensure t
  :config
  (setq-default coffee-tab-width 2))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(use-package js2-mode
  :ensure t
  ;; Not working well with ES6
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  )

(font-lock-add-keywords 'js-mode
  '(("constructor" . font-lock-keyword-face)))

(add-hook 'js-mode-hook
          (lambda ()
            (subword-mode 1)))

(require 'ansi-color)
(defun mocha-colorize-compilation-buffer ()
  "Colorize compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'mocha-colorize-compilation-buffer)

;; Rust

(use-package rust-mode
  :ensure t
  :config
  (defun my-rust-mode-hook ()
    (whitespace-mode 0)
    (setq compilation-auto-jump-to-first-error 't
          whitespace-line-column 99
          fill-column 99)
    (whitespace-mode 1))
  (add-hook 'rust-mode-hook 'my-rust-mode-hook)
  (sp-local-pair 'rust-mode "'" nil :actions nil))

(use-package racer
  :ensure t)

(use-package company-racer
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Kotlin

(add-to-list 'auto-mode-alist '("\\.kt\\'" . java-mode))
