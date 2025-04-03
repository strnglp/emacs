;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ⚠️ BEHAVIOR CHANGES     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package icomplete
  :bind (:map icomplete-minibuffer-map
              ("TAB" . icomplete-force-complete)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("<return>" . icomplete-force-complete-and-exit)
              ("C-<return>" . exit-minibuffer))
  :hook
  (after-init . (lambda ()
                  (fido-mode -1)
                  (icomplete-vertical-mode 1)))
  :config
  (setq
   auth-sources '((:source "~/.config/emacs/.authinfo.gpg"))
   epg-pinentry-mode 'loopback
   icomplete-compute-delay 0
   icomplete-delay-completions-threshold 0
   icomplete-hide-common-prefix nil
   icomplete-in-buffer t
   icomplete-max-delay-chars 0
   icomplete-prospects-height 10
   icomplete-scroll t
   icomplete-separator "  "
   icomplete-show-matches-on-no-input t
   icomplete-with-completion-tables t)
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions))

(defun enable-prog-settings ()
  "Enable settings for programming modes."
  (display-line-numbers-mode 1)
  (setq-local truncate-lines t))

(defun prog-mode-hooks ()
  "Ensure prog-mode settings apply to modes that don't derive from prog-mode."
  (enable-prog-settings)
  (run-hooks 'prog-mode-hook))

(setq-default
 buffer-file-coding-system 'utf-8-unix
 indent-tabs-mode nil
 tab-width 4)

(setq
 Man-notify-method 'aggressive
 auto-save-visited-mode t
 backup-inhibited t
 browse-url-browser-function #'browse-url-firefox
 browse-url-secondary-browser-function #'browse-url-eww
 compile-command nil
 completion-auto-help nil
 completion-ignore-case t
 completion-show-help nil
 completion-styles '(partial-completion flex initials) ;; completion-styles '(basic partial-completion substring emacs22 flex)
 completions-detailed t
 completions-max-height 20
 confirm-kill-emacs #'y-or-n-p
 create-lockfiles nil
 delete-by-moving-to-trash t
 dired-kill-when-opening-new-dired-buffer t
 display-line-numbers-type 'relative
 enable-recursive-minibuffers t
 find-ls-option '("-exec ls -ldh {} +" . "-ldh")
 global-auto-revert-non-file-buffers t
 help-window-select t
 isearch-lazy-count t
 kill-do-not-save-duplicates t
 large-file-warning-threshold (* 1024 1024 1024 8)
 make-backup-files nil
 minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt)
 mouse-yank-at-point t
 pixel-scroll-precision-use-momentum nil
 proced-auto-update-flag 'visible
 proced-auto-update-interval 1
 proced-descent t
 proced-filter 'user
 proced-tree-flag t
 read-answer-short t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 remote-file-name-inhibit-auto-save-visited t
 remote-file-name-inhibit-locks t
 search-whitespace-regexp ".*?"
 select-enable-clipboard t
 select-enable-primary nil
 set-mark-command-repeat-pop t
 tab-always-indent 'complete
 use-dialog-box nil
 use-file-dialog nil
 use-short-answers t
 wdired-allow-to-change-permissions t
 wdired-create-parent-directories t)

(when (eq system-type 'darwin)
  (setq
   mac-command-modifier 'meta
   mac-option-modifier 'super))

(unless (string= (system-name) "tower")
  (setq dired-use-ls-dired nil))

(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(mouse-wheel-mode 1)
(pixel-scroll-precision-mode 1)
(prefer-coding-system 'utf-8)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(winner-mode)
(xterm-mouse-mode 1)

(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "C-x C-k RET"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))
(global-unset-key [mouse-3])

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [remap list-buffers] #'buffer-menu)

(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))
(add-hook 'proced-mode-hook (lambda () (proced-toggle-auto-update 1)))
(add-hook 'prog-mode-hook #'enable-prog-settings)
(add-hook 'emacs-lisp-mode-hook #'prog-mode-hooks)
(add-hook 'sh-mode-hook #'prog-mode-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ➕ BEHAVIOR ADDITIONS    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sudo-edit)

(use-package define-word)

(use-package lua-mode)

(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.php\\'" . web-mode))
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2))

(use-package tintin-mode
  :straight
  (tintin-mode :type git
               :host github
               :repo "matthewrsilver/tintin-mode")
  :mode ("\\.tin\\'" "\\.tt\\'" "\\.tintin\\'"))

(use-package geiser-guile
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-default-implementation 'guile
        geiser-guile-binary "guile3.0"))


(use-package move-lines
  :straight
  (move-lines :type git
              :host github
              :repo "targzeta/move-lines"))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("q" . nil)
              ("M-s o" . my-pdf-occur-and-switch)))

(use-package marginalia
  :init (marginalia-mode))

(use-package clipetty
  :bind ("M-w" . clipetty-kill-ring-save))

(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  :hook (prog-mode . rainbow-mode))

(use-package gptel
  :bind ("<f6>" . gptel)
  :config
  (global-set-key (kbd "C-c RET") 'gptel-send)
  (setq
   gptel-model 'gemini-2.5-pro-exp-03-25
   gptel-backend
   (gptel-make-gemini "Gemini"
     :key (funcall (plist-get
                    (car (auth-source-search :host "aistudio.google.com"))
                    :secret))
     :stream t)))

(defun format-elisp-buffer ()
  "Format the current Emacs Lisp buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(setq
 ispell-program-name "aspell"
 ispell-dictionary "en_US")

(with-eval-after-load 'icomplete
  (defun my-icomplete-scroll-size ()
    "Determine the number of visible candidates in the Icomplete minibuffer."
    (let* ((win (active-minibuffer-window))
           (lines (max 1 (1- (window-height win)))))
      (if win lines 6)))  ;; Fallback to 6 if window height isn't detected

  (define-key icomplete-minibuffer-map (kbd "C-v")
              (lambda ()
                (interactive)
                (dotimes (_ (my-icomplete-scroll-size))
                  (icomplete-forward-completions))))

  (define-key icomplete-minibuffer-map (kbd "M-v")
              (lambda ()
                (interactive)
                (dotimes (_ (my-icomplete-scroll-size))
                  (icomplete-backward-completions)))))


(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c f") 'format-elisp-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-j") 'duplicate-dwim)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-a") 'my/toggle-window-transparency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 🎨 APPEARANCE CUSTOMIZATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package leuven-theme)
(use-package tao-theme)
(use-package tango-plus-theme)
(use-package gruber-darker-theme)
(use-package greymatters-theme)
(use-package modus-themes
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-slanted-constructs t
        modus-themes-italic-constructs t
        modus-themes-common-palette-overrides
        `((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified))))

(use-package nerd-icons-dired
  :config (nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :demand t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config (nerd-icons-completion-mode))

(setq
 column-number-mode nil
 current-theme nil
 custom-safe-themes t
 dark-theme 'modus-vivendi
 display-line-numbers-widen t
 display-line-numbers-width 3
 light-theme 'modus-operandi
 line-number-mode nil
 proced-enable-color-flag t
 resize-mini-windows 'grow-only
 ring-bell-function 'ignore
 scroll-conservatively 8
 scroll-margin 5
 split-height-threshold nil
 split-width-threshold 170
 visible-bell nil
 window-combination-resize t
 window-resize-pixelwise t)

(defun disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(defun adapt-theme ()
  "Adapt theme to time of day."
  (interactive)
  (let* ((time (current-time))
         (decoded-time (decode-time time))
         (hour (nth 2 decoded-time)))
    (if (or (< hour 7) (>= hour 18))
        (unless (eq current-theme dark-theme)
          (setq current-theme dark-theme)
          (load-theme dark-theme t))
      (unless (eq current-theme light-theme)
        (setq current-theme light-theme)
        (load-theme light-theme t)))))

(defun clear-theme ()
  (disable-theme dark-theme)
  (disable-theme light-theme)
  (setq current-theme nil))

(defun reset-theme ()
  (clear-theme)
  (adapt-theme))

(defun my/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 75)
        (current-alpha (frame-parameter nil 'alpha-background)))
    (if (not current-alpha)  ;; If current-alpha is nil, set it to alpha-transparency
        (set-frame-parameter nil 'alpha-background alpha-transparency)
      (if (= current-alpha alpha-transparency)
          (set-frame-parameter nil 'alpha-background 100)
        (set-frame-parameter nil 'alpha-background alpha-transparency)))))

(unless desktop-save-mode
  (adapt-theme))

(run-with-timer 0 60 'adapt-theme)

(add-hook 'minibuffer-setup-hook (lambda () (setq truncate-lines t)))

(provide 'general)
