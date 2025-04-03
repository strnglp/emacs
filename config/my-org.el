;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ⚠️ BEHAVIOR CHANGES     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-org-paths ()
  (let* ((home (or (getenv "HOME") "~"))
         (base-dir (concat home "/Documents/Org/"))
         (agenda-dir (concat base-dir "Agenda/")))
    (setq
     org-directory base-dir
     org-default-notes-file (concat agenda-dir "TODO.org")
     org-roam-directory (concat base-dir "org-roam/")
     org-agenda-files
     (seq-filter #'file-regular-p (directory-files agenda-dir t "\\.org\\'")))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("C-c RET" . gptel-send)
              ("C-M-<right>" . org-indent-item)
              ("C-M-<left>" . org-outdent-item))

  :hook
  ((org-mode . set-org-cursor)
   (org-agenda-mode . set-agenda-cursor))

  :config
  (set-org-paths)
  (org-babel-do-load-languages 'org-babel-load-languages '((scheme . t)))

  (setq
   org-capture-templates
   `(("a" "Appointment" entry (file org-default-notes-file)
      "* %?\n  SCHEDULED: %^T\n")
     ("T" "Task with backlink" entry (file org-default-notes-file)
      "* TODO %?\n+ Source: %a\n\n"
      :prepare-finalize (org-append-backlink))
     ("t" "Task" entry (file org-default-notes-file)
      "* TODO %?" :empty-lines 1)
     ("z" "Zibaldone" plain (file ,(lambda () (get-next-zibaldone)))
      "" :no-save t :jump-to-captured t))

   org-agenda-use-tag-inheritance nil
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   org-cycle-separator-lines 1
   org-fontify-done-headline nil
   org-fontify-quote-and-verse-blocks t
   org-fontify-todo-headline nil
   org-hidden-keywords '(title author email date description tags filetags)
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-link-frame-setup '((file . find-file))
   org-return-follows-link t
   org-startup-indented t
   org-startup-with-inline-images 'inlineimages
   org-tags-column -118
   org-use-tag-inheritance nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ➕ BEHAVIOR ADDITIONS    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/org-insert-web-metadata (url)
  "Fetch Open Graph metadata from URL and insert it into the Org document."
  (interactive "sEnter URL: ")
  (let ((html (with-temp-buffer
                (url-insert-file-contents url)
                (buffer-string))))
    (when (string-match "<title>\\([^<]+\\)</title>" html)
      (setq title (match-string 1 html)))
    (insert (format ":LINK: [[%s][%s]]"
                    url title))))

(defun get-next-zibaldone ()
  "Return the next numbered .org filename in DIR, based on a fresh file count."
  (let* ((dir (expand-file-name "~/Sync/Documents/Zibaldone/")))
    (let* ((files (directory-files dir nil "^[0-9]+\\.org$"))
           (next-num (1+ (length files))))
      (expand-file-name (format "%d.org" next-num) dir))))

(defun org-append-backlink ()
  "Append a backlink to the 'Related Tasks' section at the bottom of the original file."
  (when (org-capture-get :original-file)
    (let* ((orig-file (org-capture-get :original-file))
           (note-id (org-id-get-create))
           (note-title (save-excursion
                         (goto-char (point-min))
                         (if (looking-at "^\\*+ \\(.*\\)")
                             (match-string 1)
                           (progn
                             (goto-char (point-max))
                             (if (re-search-backward "^\\*+ \\(.*\\)" nil t)
                                 (match-string 1)
                               "Untitled Task")))))
           (backlink (format "\n- [[id:%s][%s]]" note-id note-title)))
      (with-current-buffer (find-file-noselect orig-file)
        (goto-char (point-max))
        (unless (re-search-backward "^\\* Related Tasks" nil t)
          (goto-char (point-max))
          (insert "\n* Related Tasks"))
        (goto-char (point-max))
        (insert backlink)
        (save-buffer)))))

(defun get-major-org-title ()
  "Return the #+title of the current Org buffer, stripped of any trailing ' <.*'."
  (let ((title (or (org-element-map (org-element-parse-buffer 'greater-element) 'keyword
                     (lambda (el)
                       (when (string= (org-element-property :key el) "TITLE")
                         (org-element-property :value el)))
                     nil t)
                   (message "No #+title found"))))
    (if title
        (replace-regexp-in-string " <.*" "" title)
      (message "No #+title found"))))

(defun my/insert-previous-roam-note ()
  "Insert an Org-roam ID link to the most recent note with a similar title, excluding the current file."
  (interactive)
  (let* ((base-title (get-major-org-title))
         (current-file (buffer-file-name))
         (nodes (org-roam-db-query
                 [:select [id file title]
                          :from nodes
                          :where (like title $s1)]
                 (concat base-title "%")))
         (filtered-nodes (seq-remove (lambda (node)
                                       (string= (nth 1 node) current-file))
                                     nodes))
         (latest-node (car (sort filtered-nodes
                                 (lambda (a b)
                                   (string> (file-name-base (nth 1 a))
                                            (file-name-base (nth 1 b))))))))
    (if latest-node
        (let ((id (nth 0 latest-node))
              (title (nth 2 latest-node)))
          (message "latest-node structure: %s" latest-node)
          (message (format "inserting ID %s and TITLE %s" id title))
          (insert (format "[[id:%s][%s]]" id title)))
      (message "No previous note found with base title: %s" base-title))))

(use-package org-roam
  :config
  (setq
   org-roam-db-location (expand-file-name "org-roam.db" org-directory)
   org-roam-node-display-template
   (concat "${title:50} "
           (propertize "${tags:*}" 'face 'org-tag))
   org-roam-capture-templates
   '(("d" "Default"
      plain "%?"
      :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                 (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}")
                 :unnarrowed t)
      ("D" "Dated"
       plain "%?"
       :if-new
       (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title} %t")
       :unnarrowed t))))

  (org-roam-db-autosync-mode)

  :hook
  (org-roam-mode . variable-pitch-mode)
  (org-roam-mode . (lambda ()
                     (setq-local
                      cursor-type 'hollow
                      display-buffer--same-window-action
                      '(display-buffer-use-some-window
                        (main)))))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("<f1>" . org-roam-buffer-toggle)))))

(use-package org-roam-ui
  :after org-roam
  :hook
  (org-roam-ui-mode . org-roam-ui-sync-theme)
  :config
  (setq
   org-roam-ui-host "0.0.0.0"
   org-roam-ui-sync-theme t
   org-roam-ui-follow t
   org-roam-ui-update-on-save t
   org-roam-ui-open-on-start t))

(use-package org-pdftools
  :mode "\\.pdf\\'"
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter
  :init
  (setq org-noter-supported-modes
        '(doc-view-mode pdf-view-mode))
  :config
  (org-noter-enable-org-roam-integration)  )

(use-package org-noter-pdftools
  :after org-noter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 🎨 APPEARANCE CUSTOMIZATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-agenda-cursor ()
  (setq-local cursor-type nil)
  (hl-line-mode)
  (visual-line-mode -1))

(defun set-org-cursor ()
  (setq-local cursor-type 'bar)
  (variable-pitch-mode)
  (visual-line-mode))


(use-package visual-fill-column
  :init
  (setq
   visual-fill-column-width 80
   visual-fill-column-center-text t)
  :hook(org-mode . visual-fill-column-mode))

(use-package org-pretty-table
  :straight
  (org-pretty-table
   :type git
   :host github
   :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))

(use-package org-tidy
  :config
  (setq
   org-tidy-top-property-style 'invisible
   org-tidy-properties-style 'invisible)
  :hook (org-mode . org-tidy-mode))

(use-package org-superstar
  :config
  (setq
   org-superstar-leading-bullet "?\s"
   org-superstar-special-todo-items t
   org-superstar-todo-bullet-alist '(("DONE" . 9745)
                                     (default . 9744))
   org-superstar-headline-bullets-list '("●" "◎" "○" "◆" "◇" "◆")
   org-superstar-item-bullet-alist
   '((?* . ?•)
     (?+ . ?◦)
     (?- . ?‣))
   inhibit-compacting-font-caches t
   org-superstar-prettify-item-bullets t)
  :hook (org-mode . org-superstar-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(provide 'my-org)
