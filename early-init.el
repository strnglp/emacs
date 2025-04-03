;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ⚠️ BEHAVIOR CHANGES     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold (* 50 1000 1000))
(defun display-startup-time ()
  (setq gc-cons-threshold (* 2 1000 1000))
  (advice-remove 'message #'silent-message)
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      ring-bell-function 'ignore
      startup-message-log nil
      warning-minimum-level :emergency)

(defun silent-message (format-string &rest args)
  (push (apply #'format format-string args) startup-message-log))
(advice-add 'message :override #'silent-message)

(tooltip-mode -1)
(blink-cursor-mode 0)
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
(defvar base-frame-parameters
  '((width . 120) (height . 48)
    (vertical-scroll-bars . nil)
    (tool-bar-lines . 0)
    (menu-bar-lines . 0)
    (left-fringe . 0)
    (right-fringe . 0))
  "Base parameters for initial and default Emacs frames.")

(setq
 inhibit-compacting-font-caches t
 my/mono-font "JetBrainsMono NF-16"
 my/mono-face "JetBrainsMono NF 16"
 my/variable-face "Linux Libertine 20"
 my/org-header-face "Linux Libertine Capitals 20"
 initial-frame-alist base-frame-parameters
 default-frame-alist base-frame-parameters
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 frame-title-format '("Emacs"))

(cond
 ((eq system-type 'gnu/linux)
  ())
 ((eq system-type 'darwin)
  (setq
   my/mono-font "JetBrainsMono Nerd Font-16"
   my/mono-face "JetBrainsMono Nerd Font 16"))
 (t ()))

(defun set-faces ()
  (set-face-attribute 'default nil :font my/mono-face)
  (set-face-attribute 'fixed-pitch nil :font my/mono-face)
  (set-face-attribute 'variable-pitch nil :font my/variable-face :weight 'book)
  (set-face-background 'vertical-border (face-background 'mode-line-inactive))
  (set-face-foreground 'vertical-border (face-background 'mode-line-inactive)))

(defun set-org-faces ()
  (when (featurep 'org)
    (when (facep 'org-meta-line)
      (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch))
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-end-line nil :inherit 'org-block-begin-line)
    (set-face-attribute 'org-document-title nil :font my/org-header-face :height 1.5)
    (set-face-attribute 'org-level-1 nil :font my/org-header-face)
    (set-face-attribute 'org-level-2 nil :font my/org-header-face)
    (set-face-attribute 'org-level-3 nil :font my/org-header-face)
    (set-face-attribute 'org-level-4 nil :font my/org-header-face))
  (when (featurep 'org-superstar)
    (set-face-attribute 'org-superstar-item nil :inherit 'fixed-pitch)))

(with-eval-after-load 'org
  (set-org-faces))


(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'run-after-load-theme-hook)

(add-hook 'org-mode #'set-org-faces)
(add-hook 'org-superstar-mode #'set-org-faces)
(add-hook 'after-load-theme-hook (lambda  ()
                                   (set-faces)
                                   (set-org-faces)))
(add-hook 'after-make-frame-functions
          (lambda (_)
            (set-frame-font my/mono-font t t)
            (set-faces)
            (set-org-faces)))
