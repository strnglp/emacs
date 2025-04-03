;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ⚠️ BEHAVIOR CHANGES     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
 ibuffer-saved-filter-groups
 '(("default"
    ("org" (or
            (mode . org-mode)
            (name . "^\\*Org Src")
            (name . "^\\*Org Agenda\\*$")))
    ("tramp" (name . "^\\*tramp.*"))
    ("emacs" (or
              (name . "^\\*scratch\\*$")
              (name . "^\\*Messages\\*$")
              (name . "^\\*Warnings\\*$")
              (name . "^\\*Shell Command Output\\*$")
              (name . "^\\*Async-native-compile-log\\*$")
              (name . "^\\*straight-")))
    ("ediff" (or
              (name . "^\\*ediff.*")
              (name . "^\\*Ediff.*")))
    ("dired" (mode . dired-mode))
    ("terminal" (or
                 (mode . term-mode)
                 (mode . shell-mode)
                 (mode . eshell-mode)))
    ("help" (or
             (name . "^\\*Help\\*$")
             (name . "^\\*info\\*$")
             (name . "^\\*helpful")))))

 ibuffer-show-empty-filter-groups nil)

(add-to-list
 'display-buffer-alist
 '(
   ;; ("\\*.*e?shell\\*"
   ;;  (display-buffer-in-side-window)
   ;;  (window-height . 0.25)
   ;;  (side . bottom)
   ;;  (slot . -1))
   ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
    (display-buffer-in-side-window)
    (window-height . 0.25)
    (side . bottom)
    (slot . 0))
   ("\\*\\([Hh]elp\\)\\*"
    (display-buffer-in-side-window)
    (window-width . 75)
    (side . right)
    (slot . 0))
   ("\\*\\(Ibuffer\\)\\*"
    (display-buffer-in-side-window)
    (window-width . 100)
    (side . right)
    (slot . 1))
   ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
    (display-buffer-in-side-window)
    (window-height . 0.25)
    (side . bottom)
    (slot . 1))
   ("\\*\\(grep\\|find\\)\\*"
    (display-buffer-in-side-window)
    (window-height . 0.25)
    (side . bottom)
    (slot . 2))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ➕ BEHAVIOR ADDITIONS    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/workspace ()
  (interactive)
  (delete-other-windows)
  (let* ((ww (window-width))
         (wh (window-height))
         (bot-row-height (/ wh -4))
         (left (frame-selected-window))
         (right-top (split-window-right (- ww 79)))
         (left-bot (split-window-below bot-row-height left))
         (right (split-window-below (/ wh 4) right-top))
         (right-bot (split-window-below bot-row-height right)))
    (select-window left-bot)
    (shell)
    (select-window right-bot)
    (if (string= (system-name) "tower")
        (progn
          (let ((buf (gptel "Gemini")))
            (switch-to-buffer buf)))
      (dired "~/"))
    (select-window right)
    (let ((org-agenda-window-setup 'current-window))
      (org-agenda nil "n"))
    (calendar)
    (select-window left)

    (set-window-dedicated-p left-bot t)
    (set-window-dedicated-p right-bot t)
    (set-window-dedicated-p right-top t)
    (set-window-dedicated-p right t)))

(provide 'workspace)
