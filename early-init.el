;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ⚠️ BEHAVIOR CHANGES     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold (* 50 1000 1000))
(defun my/display-startup-time ()
  (setq gc-cons-threshold (* 2 1000 1000))
  (advice-remove 'message #'silent-message)
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'my/display-startup-time)


(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      ring-bell-function 'ignore
      warning-minimum-level :emergency)

(defvar startup-message-log nil)

(defun silent-message (format-string &rest args)
  (push (apply #'format format-string args) startup-message-log))
(advice-add 'message :override #'silent-message)

(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'default))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ➕ BEHAVIOR ADDITIONS    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 🎨 APPEARANCE CUSTOMIZATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
 inhibit-compacting-font-caches t
 my/mono-font "JetBrainsMono NF-16"
 my/mono-face "JetBrainsMono NF 16"
 my/variable-face "Linux Libertine 20"
 my/org-header-face "Linux Libertine Capitals 20"
 initial-frame-alist '((width . 120) (height . 48))
 default-frame-alist '((width . 120) (height . 48))
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 frame-title-format '("Emacs")
 )

(cond
 ((eq system-type 'gnu/linux)
  ())
 ((eq system-type 'darwin)
  (setq-local
   my/mono-font "JetBrainsMono Nerd Font-16"
   my/mono-face "JetBrainsMono Nerd Font 16"))
 (t ())
 )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun my/set-faces ()
  (set-face-attribute 'default nil :font my/mono-face)
  (set-face-attribute 'fixed-pitch nil :font my/mono-face)
  (set-face-attribute 'variable-pitch nil :font my/variable-face :weight 'book)
  (set-face-background 'vertical-border (face-background 'mode-line-inactive))
  (set-face-foreground 'vertical-border (face-background 'mode-line-inactive)))

(defun my/set-org-faces ()
  (with-eval-after-load 'org
    (when (facep 'org-meta-line)
      (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch))
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-end-line nil :inherit 'org-block-begin-line)
    (set-face-attribute 'org-document-title nil :font my/org-header-face :height 1.5)
    (set-face-attribute 'org-level-1 nil :font my/org-header-face :height 1.1)
    (set-face-attribute 'org-level-2 nil :font my/org-header-face :height 1.1)
    (set-face-attribute 'org-level-3 nil :font my/org-header-face :height 1.1)
    (set-face-attribute 'org-level-4 nil :font my/org-header-face :height 1.1))
  (with-eval-after-load 'org-superstar-mode
    (set-face-attribute 'org-superstar-item nil :inherit 'fixed-pitch :height 120)))

(add-hook 'org-mode #'my/set-org-faces)
(add-hook 'org-superstar-mode #'my/set-org-faces)
(add-hook 'after-load-theme-hook (lambda  ()
                                   (my/set-faces)
                                   (my/set-org-faces)))



(add-hook 'after-make-frame-functions
          (lambda (_)
            (set-frame-font my/mono-font t t)
            (my/set-faces)
            (my/set-org-faces)))
