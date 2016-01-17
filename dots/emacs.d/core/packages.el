;;; packages.el ---
(require 'cl)
(require 'package)

(setq package-user-dir "~/.local/share/emacs/elpa")
(defvar async-byte-compile-log-file  (cache-file-name "async-bytecomp.log"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))


(package-install 'use-package)
(ensure-package 'use-package)

(require 'use-package)

;;; packages.el ends here
