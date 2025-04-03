(use-package mustache
  :demand t
  :straight
  (mustache
   :type git
   :host github
   :repo "Wilfred/mustache.el"))

(defun get-hex-color-for-face (face attribute)
  "Retrieve the hex color for the given FACE and ATTRIBUTE, with a fallback mechanism."
  (let* ((fallback (face-attribute 'default attribute nil t))
         (color (face-attribute face attribute nil t)))
    ;;(message "Attribute: %s" attribute)

    ;; Normalize color to a string if it's a symbol
    (when (symbolp color)
      (setq color (symbol-name color)))

    ;; Use fallback if color is unspecified
    (when (string-match-p "unspecified" color)
      ;;(message "UNSPECIFIED DETECTED: face: %s attribute: %s using: %s" face attribute fallback)
      (setq color fallback))

    ;; Convert named colors to hex if not already in hex format
    (unless (string-match-p "^#" color)
      (setq color (ignore-errors
                    (color-rgb-to-hex
                     (nth 0 (color-name-to-rgb color))
                     (nth 1 (color-name-to-rgb color))
                     (nth 2 (color-name-to-rgb color)) 2))))
    ;; Return the resulting color or nil if invalid
    color))



(defun get-terminal-colors-from-theme ()
  "Extract ansi colors from the active theme, including default foreground and background."
  (let* ((default-fg (get-hex-color-for-face 'default :foreground))
         (default-bg (get-hex-color-for-face 'default :background))
         (selection-fg (get-hex-color-for-face 'region :foreground))
         (selection-bg (get-hex-color-for-face 'region :background))
         (cursor-bg (get-hex-color-for-face 'cursor :background))
         (ansi-colors
          (delq nil
                (mapcar (lambda (ansi-face)
                          (let ((fg (get-hex-color-for-face ansi-face :foreground)))
                            (unless (eq fg 'unspecified)
                              (cons ansi-face fg))))
                        (cl-remove-if-not
                         (lambda (face)
                           (string-match-p "^ansi-.*" (symbol-name face)))
                         (face-list))))))
    (append ansi-colors
            (list (cons 'ansi-color-foreground default-fg)
                  (cons 'ansi-color-background default-bg)
                  (cons 'selection-foreground selection-fg)
                  (cons 'selection-background selection-bg)
                  (cons 'cursor-background cursor-bg)))))



(defun get-window-colors-from-theme ()
  "Extract window colors from the active theme"
  `((highlight-background . ,(get-hex-color-for-face 'highlight :background))
    (highlight-foreground . ,(get-hex-color-for-face 'highlight :foreground))
    (isearch-background . ,(get-hex-color-for-face 'isearch :background))
    (isearch-foreground . ,(get-hex-color-for-face 'isearch :foreground))
    (tty-menu-selected-face-foreground . ,(get-hex-color-for-face 'tty-menu-selected-face :foreground))
    (tty-menu-selected-face-background . ,(get-hex-color-for-face 'tty-menu-selected-face :background))
    (mode-line-active-foreground . ,(get-hex-color-for-face 'mode-line :foreground))
    (mode-line-active-background . ,(get-hex-color-for-face 'mode-line :background))
    (mode-line-inactive-foreground . ,(get-hex-color-for-face 'mode-line-inactive :foreground))
    (mode-line-inactive-background . ,(get-hex-color-for-face 'mode-line-inactive :background))
    (window-divider-fg . ,(get-hex-color-for-face 'window-divider :foreground))
    (inactive-window-divider-fg . ,(get-hex-color-for-face 'window-divider-last-pixel :foreground))))


(defun write-color-data (path)
  "Combine the ansi colors and window colors from the current theme and write them to ~/.emacs.d/color-data.json"
  (write-region
   (with-temp-buffer
     (insert (json-serialize
              (append
               `((theme . ,(symbol-name (car custom-enabled-themes))))
               (get-terminal-colors-from-theme)
               (get-window-colors-from-theme))))
     (json-pretty-print (point-min) (point-max))
     (buffer-string))
   nil path))


(defun render-template (template-dir out)
  (let ((mustache-partial-paths (list template-dir))
        (colors (append (get-terminal-colors-from-theme)
                        (get-window-colors-from-theme)))
        rendered)

    (setq rendered (mustache-render
                    "{{> template}}"
                    (cl-loop for (name . color) in colors
                             collect (cons (symbol-name name) (format "%s" color)))))

    (write-region
     rendered
     nil
     (concat (file-name-as-directory template-dir) out))))


(defun my/apply-unified-theme (&rest _args)
  "Call an external tool to utilize color-data.json"
  (interactive)
  ;;(message "Applying unified theme")
  (render-template "~/.config/unified-theme/wezterm/" "colors.toml")
  (render-template "~/.config/unified-theme/xterm/" "Xresources")
  (render-template "~/.config/unified-theme/tmux/" "colors")
  (render-template "~/.config/unified-theme/i3/" "colors")
  (render-template "~/.config/unified-theme/rofi/" "colors.rasi")
  (start-process "touch wezterm" nil "touch" (expand-file-name "~/.config/wezterm/wezterm.lua"))
  (start-process "source tmux" nil "tmux" "source-file" (expand-file-name "~/.config/tmux/tmux.conf"))
  (start-process "xrdb merge" nil "xrdb" (expand-file-name "~/.Xresources"))
  ;;(start-process "reload i3" nil "i3-msg" "reload")
  ;;(start-process "xsetroot" nil "xsetroot" "-solid" (get-hex-color-for-face 'mode-line-inactive :background))
  (message "Applied unified theme"))

;; We must wait until late in the init process before we attempt to derive any theme color values
;;(add-hook 'emacs-startup-hook #'my/apply-unified-theme)
(add-hook 'server-after-make-frame-hook #'my/apply-unified-theme)
(advice-add 'enable-theme :after #'my/apply-unified-theme)

(provide 'unified-theme)
