(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-mode which-key visual-fill-column use-package sudo-edit rg rainbow-mode rainbow-delimiters neotree lua-mode leuven-theme ivy-rich helpful gnu-elpa-keyring-update evil-collection eglot diminish counsel company clojure-ts-mode cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
