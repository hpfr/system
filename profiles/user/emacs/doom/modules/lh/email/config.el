(load! "mu4e-load-path.el")             ; the nix store load path is evaluated on system rebuild

(set-email-account! "hpfr"
                    '((mu4e-sent-folder . "/hpfr/Sent")
                      (mu4e-drafts-folder . "/hpfr/Drafts")
                      (mu4e-trash-folder . "/hpfr/Trash")
                      (mu4e-refile-folder . "/hpfr/Archive")
                      (user-full-name . "Liam Hupfer")
                      (user-mail-address . "liam@hpfr.net")
                      (smtpmail-smtp-user . "liam@hpfr.net")
                      (mu4e-compose-signature . "—Liam")
                      (org-msg-signature . "\n\n#+begin_signature\n—Liam\n#+end_signature")))

;; for public-inbox maildirs
;; (setq mu4e-mu-home "~/.cache/mu-lists/"
;;       mu4e-get-mail-command "mbsync --config ~/repos/system/profiles/user/email/mbsync-lists.conf -a")

;; this must be available on startup for the desktop entry when emacs is not running
(defun mu4e-compose-from-mailto (mailto-string)
  (require 'mu4e)
  (unless mu4e~server-props (mu4e t) (sleep-for 0.1))
  (let* ((mailto (rfc2368-parse-mailto-url mailto-string))
         (to (cdr (assoc "To" mailto)))
         (subject (or (cdr (assoc "Subject" mailto)) ""))
         (body (cdr (assoc "Body" mailto)))
         (org-msg-greeting-fmt (if (assoc "Body" mailto)
                                   (replace-regexp-in-string "%" "%%"
                                                             (cdr (assoc "Body" mailto)))
                                 org-msg-greeting-fmt))
         (headers (-filter (lambda (spec) (not (-contains-p '("To" "Subject" "Body") (car spec)))) mailto)))
    (mu4e~compose-mail to subject headers)))

(after! mu4e
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (setq mu4e-context-policy 'ask
        mu4e-compose-signature nil
        sendmail-program "/etc/profiles/per-user/lh/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        mu4e-headers-fields
        '((:account-stripe . 1)
          (:maildir . 17)
          (:human-date . 12)
          (:flags . 10)
          (:from-or-to . 40)
          (:subject))
        mu4e-headers-date-format "%F"
        mu4e-headers-time-format "%Hh%Mm%S"
        mu4e-headers-long-date-format "%F %a %Hh%Mm%S %Z"
        mu4e-date-format-long "%F %a %Hh%Mm%S %Z"
        mu4e-get-mail-command "mbsync -a"
        ;; don't use teco's gmail optimizations
        mu4e-index-cleanup t
        mu4e-index-lazy-check nil
        mu4e-headers-full-search nil ;; toggle with Q
        gnus-treat-display-smileys nil
        +mu4e-personal-addresses '("liam@hpfr.net")
        mu4e-bookmarks '((:name "Unread messages"
                          :query "flag:unread and not flag:trashed" :key ?u)
                         (:name "Today's messages" :query "date:today..now" :key ?t)
                         (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
                         (:name "Messages with images" :query "mime:image/*" :key ?p)
                         (:name "Flagged messages" :query "flag:flagged" :key ?f)))

  (defvar mu4e-reindex-request-file "/tmp/mu_reindex_now"
    "Location of the reindex request, signaled by existence")
  (defvar mu4e-reindex-request-min-seperation 5.0
    "Don't refresh again until this many second have elapsed.
Prevents a series of redisplays from being called (when set to an appropriate value)")

  (defvar mu4e-reindex-request--file-watcher nil)
  (defvar mu4e-reindex-request--file-just-deleted nil)
  (defvar mu4e-reindex-request--last-time 0)

  (defun mu4e-reindex-request--add-watcher ()
    (setq mu4e-reindex-request--file-just-deleted nil)
    (setq mu4e-reindex-request--file-watcher
          (file-notify-add-watch mu4e-reindex-request-file
                                 '(change)
                                 #'mu4e-file-reindex-request)))

  (defadvice! mu4e-stop-watching-for-reindex-request ()
    :after #'mu4e~proc-kill
    (if mu4e-reindex-request--file-watcher
        (file-notify-rm-watch mu4e-reindex-request--file-watcher)))

  (defadvice! mu4e-watch-for-reindex-request ()
    :after #'mu4e~proc-start
    (mu4e-stop-watching-for-reindex-request)
    (when (file-exists-p mu4e-reindex-request-file)
      (delete-file mu4e-reindex-request-file))
    (mu4e-reindex-request--add-watcher))

  (defun mu4e-file-reindex-request (event)
    "Act based on the existance of `mu4e-reindex-request-file'"
    (if mu4e-reindex-request--file-just-deleted
        (mu4e-reindex-request--add-watcher)
      (when (equal (nth 1 event) 'created)
        (delete-file mu4e-reindex-request-file)
        (setq mu4e-reindex-request--file-just-deleted t)
        (mu4e-reindex-maybe t))))

  (defun mu4e-reindex-maybe (&optional new-request)
    "Run `mu4e~proc-index' if it's been more than
`mu4e-reindex-request-min-seperation'seconds since the last request,"
    (let ((time-since-last-request (- (float-time)
                                      mu4e-reindex-request--last-time)))
      (when new-request
        (setq mu4e-reindex-request--last-time (float-time)))
      (if (> time-since-last-request mu4e-reindex-request-min-seperation)
          (mu4e~proc-index nil t)
        (when new-request
          (run-at-time (* 1.1 mu4e-reindex-request-min-seperation) nil
                       #'mu4e-reindex-maybe))))))

;; can't use after! due to mu4e module using use-package block's :after
;; https://github.com/jwiegley/use-package/issues/829
(use-package-hook! org-msg
  :post-config
  ;; Use org-msg to reply even with plaintext emails, but only export to plaintext
  ;; in that case. that way we can take advantage of table syntax, etc
  (setq org-msg-default-alternatives '((new . (utf-8 html))
                                       (reply-to-html . (utf-8 html))
                                       (reply-to-text . (utf-8)))
        ;; no top-posting with HTML emails
        org-msg-posting-style nil))

(load! "private.el")
