(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq
 straight-use-package-by-default t
 use-package-always-defer t
 custom-file (locate-user-emacs-file "custom-vars.el"))

(use-package gnu-elpa-keyring-update)
;
(when (or (eq system-type 'gnu/linux) (eq system-type 'darwin))
  (add-to-list 'load-path "~/.config/emacs/config"))

(when (eq system-type 'windows-nt)
  (add-to-list 'load-path (concat (getenv "APPDATA") "/.emacs.d/config")))

(require 'general)
(require 'workspace)
(require 'my-org)
(when (string= (system-name) "tower")
    (require 'unified-theme))

(load custom-file 'noerror 'nomessage)
