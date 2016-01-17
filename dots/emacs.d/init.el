;;; init.el ---

(setq inhibit-x-resources 't)

(defvar local-dir user-emacs-directory
  "The root dir of the Emacs configuration.")

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name local-dir))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(defun cache-file-name (file-name)
  (let* ((file-path (expand-file-name file-name "~/.cache/emacs"))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))


(defun load-local (file-name)
  (load (local-file-name file-name)))

(setq gc-cons-threshold 50000000)
(setq shell-file-name "bash")

(setq load-prefer-newer t)
(load-local "core/packages.el")
(load-local "rc/ui.el")
(load-local "rc/editor.el")
(load-local "rc/bindings.el")
(load-local "rc/coding.el")

(setq custom-file (cache-file-name "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))


;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
