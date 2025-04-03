;; Provide mairix maildir search inside emacs
(require 'gnus)
(require 'nnmairix)

(global-set-key (kbd "<f5>") 'gnus)

;; Configure gnus for reading email from a local mbsync maildir
(setq
 gnus-home-directory "~/.cache/emacs/gnus"
 gnus-dribble-directory "~/.cache/emacs/gnus"
 gnus-startup-file "~/.cache/emacs/gnus/newsrc"
 gnus-always-read-dribble-file t
 gnus-read-newsrc-file t
 gnus-use-full-window nil
 gnus-select-method '(nnnil)
 gnus-secondary-select-methods
 '((nntp "feedbase"
         (nntp-open-connection-function nntp-open-tls-stream)
         (nntp-port-number 563)
         (nntp-address "feedbase.org"))
   (nnmaildir "proton"
              (directory "~/Mail/Proton")
              (directory-files nnheader-directory-files-safe)))
 ;; Don't hide groups with no new messages from group buffer
 gnus-permanently-visible-groups ".*"
 ;; Don't hide old emails from summary buffer
 gnus-parameters
 '(("proton.*"
    (gnus-large-newsgroup nil)
    (display . all)))
 ;; Custom friendly date format for recieved messages
 gnus-user-date-format-alist
 '(((gnus-seconds-today) . "%H:%M")
   ((+ 86400 (gnus-seconds-today)) . "Yesterday")
   (604800 . "%A")
   ((gnus-seconds-month) . "%b %d")
   ((gnus-seconds-year) . "%b %d")
   (t . "%b %d '%y"))
 gnus-fetch-old-headers t
 ;; Custom summary line fromat
 gnus-summary-line-format
 (concat "%U%R %* %~(max-right 35)~(pad-right 35)n "
         "%B %s %-160= %~(max-left 10)~(pad-left 10)&user-date;\n")
 gnus-sum-thread-tree-indent          "  "
 gnus-sum-thread-tree-root            "● "
 gnus-sum-thread-tree-false-root      "◯ "
 gnus-sum-thread-tree-single-indent   "◎ "
 gnus-sum-thread-tree-vertical        "│"
 gnus-sum-thread-tree-leaf-with-other "├─► "
 gnus-sum-thread-tree-single-leaf     "╰─► "
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-article-sort-functions
 '(gnus-article-sort-by-most-recent-date)
 gnus-thread-sort-functions
 '(gnus-thread-sort-by-most-recent-date)
 ;; Optimize for speed
 gnus-asynchronous t
 gnus-use-cache t
 gnus-cacheable-groups "proton.*")

;; Highlight selected line
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)'
(add-hook 'gnus-topic-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-server-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

(add-hook 'gnus-get-new-news-hook (lambda ()
                                    (message "Syncing email")
                                    (shell-command "mbsync -q -a")))

(setq smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 1025
      smtpmail-stream-type 'starttls
      smtpmail-smtp-user "jviolano@protonmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      user-mail-address "jviolano@protonmail.com"
      message-alternative-emails
      (concat
       (regexp-quote "jv.random@pm.me") "\\|"
       (regexp-quote "jv.shopping@pm.me") "\\|"
       (regexp-quote "jv.social@pm.me") "\\|"
       (regexp-quote "contact@strangeloop.blog"))
      user-full-name "John Violano")

(setq gnus-message-archive-group nil)

;; gnus and mbsync rely on protonmail-bridge providing email
(defun my/start-protonmail-bridge ()
  (interactive)
  "Start ProtonMail Bridge if it isn't running."
  (unless (get-process "protonmail-bridge")
    (let ((process (start-process "protonmail-bridge" nil "protonmail-bridge" "-n")))
      (set-process-query-on-exit-flag process nil))))

;; Desktop autostarts this now
;;(start-protonmail-bridge)
(provide 'email)
