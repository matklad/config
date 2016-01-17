(use-package dash
  :ensure t)


(defun smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (recenter))


(defun smart-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


(defun isearch-delete-something ()
  "Delete non-matching text or the last character."
  ;; Mostly copied from `isearch-del-char' and Drew's answer on the page above
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))


(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
        (delete-file filename)
        (message "Deleted file %s" filename)
        (kill-buffer)))))


(defun rename-buffer-and-file ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (rename-file filename new-name t)
        (set-visited-file-name new-name t t)))))


(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
   (->> (buffer-list)
     (-filter #'buffer-file-name)
     (--remove (eql (current-buffer) it)))
   #'kill-buffer))


;;; from mastering emacs
(defun point-in-string-p (pt)
  "Returns t if PT is in a string"
  (eq 'string (syntax-ppss-context (syntax-ppss pt))))

(defun beginning-of-string ()
  "Moves to the beginning of a syntactic string"
  (interactive)
  (unless (point-in-string-p (point))
    (error "You must be in a string for this command to work"))
  (while (point-in-string-p (point))
    (forward-char -1))
  (point))

(defun swap-quotes ()
  "Swaps the quote symbols in a \\[python-mode] string"
  (interactive)
  (save-excursion
    (let ((bos (save-excursion
                 (beginning-of-string)))
          (eos (save-excursion
                 (beginning-of-string)
                 (forward-sexp)
                 (point)))
          (replacement-char ?\'))
      (goto-char bos)
      ;; if the following character is a single quote then the
      ;; `replacement-char' should be a double quote.
      (when (eq (following-char) ?\')
          (setq replacement-char ?\"))
      (delete-char 1)
      (insert replacement-char)
      (goto-char eos)
      (delete-char -1)
      (insert replacement-char))))



(put 'smart-compile-command 'safe-local-variable #'stringp)

(defun smart-compile ()
  "runs compile command based on current major mode."
  (interactive)
  (let* ((cmd
          (cond ((bound-and-true-p smart-compile-command) smart-compile-command)
                ((eq major-mode 'js-mode) "npm test")
                ((eq major-mode 'rust-mode) "cargo build")
                ((eq major-mode 'haskell-mode) "cabal run")))
         (default-directory (projectile-project-root)))
    (progn
      (save-some-buffers 1)
      (compile cmd))))


(global-set-key (kbd "<f11>")   'toggle-frame-maximized)
(global-set-key (kbd "C-a")     'smart-move-beginning-of-line)
(global-set-key (kbd "C-c '")   'swap-quotes)
(global-set-key (kbd "C-c D")   'delete-file-and-buffer)
(global-set-key (kbd "C-c C-c") 'smart-compile)
(global-set-key (kbd "C-c c")   'compile)
(global-set-key (kbd "C-c k")   'kill-other-buffers)
(global-set-key (kbd "C-c r")   'rename-buffer-and-file)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "M-o")     'smart-open-line)
(global-set-key (kbd "M-s")     'sp-splice-sexp)
(global-set-key (kbd "C-c s")   'helm-imenu)
(global-set-key (kbd "M-%")     'anzu-query-replace)
(global-set-key (kbd "C-M-%")   'anzu-query-replace-regexp)
(global-set-key (kbd "S-SPC")   'toggle-input-method)
(global-set-key (kbd "M-h")     'backward-kill-word)
(global-set-key (kbd "C-c n")   'helm-all-mark-rings)
(global-set-key (kbd "C-c b")   'pop-global-mark)
(define-key key-translation-map [?\C-h] [?\C-?])


(define-key isearch-mode-map (kbd "<backspace>")
  #'isearch-delete-something)
