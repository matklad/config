;;; ra-emacs-lsp.el --- Rust analyzer emacs bindings for emacs-lsp -*- lexical-binding: t; -*-
;;; Code:

(package-install 'lsp-mode)
(package-install 'dash)
(package-install 'ht)
(require 'lsp)
(require 'dash)
(require 'ht)

;; This currently
;;  - sets up rust-analyzer with emacs-lsp, giving
;;    - code actions
;;    - completion (use company-lsp for proper snippet support)
;;    - imenu support
;;    - on-type formatting
;;    - 'hover' type information & documentation (with lsp-ui)
;;  - implements source changes (for code actions etc.), except for file system changes
;;  - implements joinLines (you need to bind rust-analyzer-join-lines to a key)
;;  - implements extendSelection (either bind rust-analyzer-extend-selection to a key, or use expand-region)

;; What's missing:
;;  - file system changes in apply-source-change
;;  - semantic highlighting
;;  - onEnter, parentModule, findMatchingBrace
;;  - runnables
;;  - the debugging commands (syntaxTree and analyzerStatus)
;;  - lsp-ui doesn't interpret the markdown we return currently and instead displays it raw (https://github.com/emacs-lsp/lsp-ui/issues/220 )
;;  - more

;; Also, there's a problem with company-lsp's caching being too eager, sometimes
;; resulting in outdated completions.

(defcustom rust-analyzer-command '("ra_lsp_server")
  ""
  :type '(repeat (string)))

(defconst rust-analyzer--notification-handlers
  '(("rust-analyzer/publishDecorations" . (lambda (_w _p)))))

(defconst rust-analyzer--action-handlers
  '(("rust-analyzer.applySourceChange" .
     (lambda (p) (rust-analyzer--apply-source-change-command p)))))

(defun rust-analyzer--uri-filename (text-document)
  (lsp--uri-to-path (gethash "uri" text-document)))

(defun rust-analyzer--goto-lsp-loc (loc)
  (-let (((&hash "line" "character") loc))
    (goto-line (1+ line))
    (move-to-column character)))

(defun rust-analyzer--apply-text-document-edit (edit)
  "Like lsp--apply-text-document-edit, but it allows nil version."
  (let* ((ident (gethash "textDocument" edit))
         (filename (rust-analyzer--uri-filename ident))
         (version (gethash "version" ident)))
    (with-current-buffer (find-file-noselect filename)
      (when (or (not version) (= version (lsp--cur-file-version)))
        (lsp--apply-text-edits (gethash "edits" edit))))))

(defun rust-analyzer--apply-source-change (data)
  ;; TODO fileSystemEdits
  (--each (-> data (ht-get "workspaceEdit") (ht-get "documentChanges"))
    (rust-analyzer--apply-text-document-edit it))
  (-when-let (cursor-position (ht-get data "cursorPosition"))
    (let ((filename (rust-analyzer--uri-filename (ht-get cursor-position "textDocument")))
          (position (ht-get cursor-position "position")))
      (find-file filename)
      (rust-analyzer--goto-lsp-loc position)
      )))

(defun rust-analyzer--apply-source-change-command (p)
  (let ((data (-> p (ht-get "arguments") (car))))
    (rust-analyzer--apply-source-change data)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () rust-analyzer-command))
  :notification-handlers (ht<-alist rust-analyzer--notification-handlers)
  :action-handlers (ht<-alist rust-analyzer--action-handlers)
  :major-modes '(rust-mode)
  :ignore-messages nil
  :server-id 'rust-analyzer))

(defun rust-analyzer--join-lines-params ()
  "Join lines params."
  (list :textDocument (lsp--text-document-identifier)
        :range (if (use-region-p)
                   (lsp--region-to-range (region-beginning) (region-end))
                 (lsp--region-to-range (point) (point)))))

(defun rust-analyzer-join-lines ()
  (interactive)
  (->
   (lsp-send-request (lsp-make-request "rust-analyzer/joinLines"
                                       (rust-analyzer--join-lines-params)))
   (rust-analyzer--apply-source-change)))

(with-eval-after-load 'company-lsp
  ;; company-lsp provides a snippet handler for rust by default that adds () after function calls, which RA does better
  (setq company-lsp--snippet-functions (assq-delete-all "rust" company-lsp--snippet-functions)))

;; extend selection

(defun rust-analyzer-extend-selection ()
  (interactive)
  (-let (((&hash "start" "end") (rust-analyzer--extend-selection)))
    (rust-analyzer--goto-lsp-loc start)
    (set-mark (point))
    (rust-analyzer--goto-lsp-loc end)
    (exchange-point-and-mark)))

(defun rust-analyzer--extend-selection-params ()
  "Extend selection params."
  (list :textDocument (lsp--text-document-identifier)
        :selections
        (vector
         (if (use-region-p)
             (lsp--region-to-range (region-beginning) (region-end))
           (lsp--region-to-range (point) (point))))))

(defun rust-analyzer--extend-selection ()
  (->
   (lsp-send-request
    (lsp-make-request
     "rust-analyzer/extendSelection"
     (rust-analyzer--extend-selection-params)))
   (ht-get "selections")
   (car)))

(defun rust-analyzer--add-er-expansion ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(rust-analyzer-extend-selection))))

(with-eval-after-load 'expand-region
  (add-hook 'rust-mode-hook 'rust-analyzer--add-er-expansion))

(provide 'ra-emacs-lsp)
;;; ra-emacs-lsp.el ends here
