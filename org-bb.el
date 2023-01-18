;;; org-bb.el --- Defining todo blockers in a block -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.3"))
;; URL: https://github.com/akirak/org-blockers-block

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library lets you define todo blockers for Org mode in "blockers" blocks.

;; Inside a "blockers" block, you can put ID links to Org entries, and they will
;; become the blockers of the entry. You can also put extra information in the
;; block to describe why they are blockers.

;; By setting :trigger property of the block to a todo keyword, you can also
;; update the todo state of the entry automatically when all of the dependencies
;; are satisfied. You can guard the state change by setting :from property to an
;; initial todo state.

;;; Code:

(require 'org)
(require 'ol)

(defgroup org-bb nil
  "Manage Org dependencies in blocks."
  :group 'org)

(declare-function thing-at-point-looking-at "thingatpt")

(defcustom org-bb-skeleton
  '(> "#+begin_blockers :from "
      (let ((kw (completing-read "Origin state: "
                                 (mapcar #'car org-todo-kwd-alist)
                                 nil t nil nil
                                 (org-get-todo-state))))
        (if (string-empty-p kw)
            nil
          (format "\"%s\"" kw)))
      " :trigger "
      (concat "\"" (completing-read "Set the state when it is ready: "
                                    (mapcar #'car org-todo-kwd-alist)
                                    nil t)
              "\"")
      "\n"
      _
      "\n#+end_blockers")
  "Skeleton used in `org-bb-insert-block' command."
  :group 'org-bb
  :type 'sexp)

;;;###autoload
(define-minor-mode org-bb-mode
  "Turn on blocker block features in `org-mode'."
  :global t
  (if org-bb-mode
      (progn
        (add-hook 'org-trigger-hook #'org-bb-trigger)
        (add-hook 'org-blocker-hook #'org-bb-blocker)
        (add-hook 'org-ctrl-c-ctrl-c-hook #'org-bb-maybe-update))
    (progn
      (remove-hook 'org-trigger-hook #'org-bb-trigger)
      (remove-hook 'org-blocker-hook #'org-bb-blocker)
      (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-bb-maybe-update))))

(defun org-bb-blocker (change-plist)
  "Return t if the change is not blocked."
  (when (eq 'todo-state-change (plist-get change-plist :type))
    (catch 'blockers-non-blocking
      (org-with-point-at (plist-get change-plist :position)
        (let ((bound (org-entry-end-position)))
          (save-excursion
            (while (re-search-forward org-block-regexp bound t)
              (pcase (org-bb--matched-block)
                ((and `((,begin . ,end) . ,plist)
                      (guard (not (and (plist-get plist :from)
                                       (equal (plist-get plist :from)
                                              (plist-get change-plist :to))))))
                 (goto-char begin)
                 (while (< (point) end)
                   (when (and (org-at-item-checkbox-p)
                              (not (equal (match-string 1)
                                          "[X]")))
                     (setq org-block-entry-blocking
                           (when (re-search-forward org-link-bracket-re (pos-eol) t)
                             (match-string 1)))
                     (throw 'blockers-non-blocking nil))
                   (forward-line))
                 (goto-char end)))))))
      t)))

(defun org-bb-trigger (change-plist)
  "Trigger changes according to the blocking property."
  (when (eq 'todo-state-change (plist-get change-plist :type))
    (let* ((pos (plist-get change-plist :position)))
      (dolist (target-id (org-entry-get-multivalued-property pos org-bb-blocker-property))
        (if-let (marker (org-id-find target-id 'markerp))
            (with-current-buffer (marker-buffer marker)
              (org-with-wide-buffer
               (goto-char marker)
               (let ((entry-end (org-entry-end-position))
                     (todo (org-get-todo-state)))
                 (when-let (new-todo
                            (catch 'new-todo
                              (while (re-search-forward org-block-regexp bound t)
                                (pcase (org-bb--matched-block entry-end)
                                  (`((,begin . ,block-end) . ,plist)
                                   (let ((trigger (plist-get plist :trigger)))
                                     (goto-char begin)
                                     (when (and trigger
                                                (equal (plist-get plist :from)
                                                       todo)
                                                (not (org-bb--maybe-blocker block-end)))
                                       (throw 'new-todo trigger))
                                     (goto-char block-end)))))))
                   (org-todo new-todo)
                   (message "Set the state of \"%s\" to %s"
                            (org-link-display-format (org-get-heading t t t t))
                            new-todo)))))
          (error "ID %s is not found" target-id))))))

(defun org-bb--matched-block ()
  "Return information on the blocked matched by the regexp."
  (when (equal (match-string 1) "blockers")
    (let ((match (match-data)))
      (throw 'found-bb (cons (cons (goto-char (nth 8 match))
                                   (nth 9 match))
                             (read (concat "(" (or (match-string 3) "") ")")))))))

(defun org-bb--maybe-blocker (end)
  "Return a link to a blocking entry, if any."
  (catch 'blockers-blocker-link
    (while (re-search-forward org-link-bracket-re end t)
      (let ((link (match-string 0))
            (href (match-string 1))
            (case-fold-search t))
        (when (string-match (rx bol "id:" (group (+ anything)))
                            href)
          (let ((id (match-string 1 href)))
            (if-let (marker (org-id-find id 'markerp))
                (with-current-buffer (marker-buffer marker)
                  (org-with-wide-buffer
                   (goto-char marker)
                   (unless (org-entry-is-done-p)
                     (throw 'blockers-blocker-link link))))
              (message (concat "Not found: " link))
              (throw 'blockers-blocker-link link))))))))

;;;###autoload
(defun org-bb-insert-block ()
  "Insert a blockers block.

To tweak the template to suit your preference, customize
`org-bb-skeleton' variable."
  (interactive)
  (require 'skeleton)
  (unless (derived-mode-p 'org-mode)
    (user-error "Please run this command in org-mode"))
  (org-bb--set-blockee-property)
  (skeleton-insert org-bb-skeleton))

;;;###autoload
(defun org-bb-update-block (&optional as-hook)
  "Update the blockers defined in the block at point."
  (interactive)
  (require 'thingatpt)
  (save-match-data
    (if (or (org-at-block-p)
            (and (not as-hook)
                 (org-in-block-p '("blockers"))
                 (thing-at-point-looking-at org-block-regexp)))
        (if (equal (match-string 1) "blockers")
            (let ((end (nth 9 (match-data)))
                  (target (org-id-get-create)))
              (save-excursion
                (while (re-search-forward org-link-bracket-re end t)
                  (let ((link (match-string 0))
                        (href (match-string 1))
                        (case-fold-search t))
                    (when (string-match (rx bol "id:" (group (+ anything)))
                                        href)
                      (let ((id (match-string 1 href)))
                        (if-let (marker (org-id-find id 'markerp))
                            (org-entry-add-to-multivalued-property
                             marker org-bb-blocker-property target)
                          (error "Not found: %s" link))))))))
          (unless as-hook
            (user-error "Not on a \"blockers\" block")))
      (unless as-hook
        (user-error "Not on a block")))))

(defun org-bb-maybe-update ()
  "Update the block at point.

This function is intended for addition to
`org-ctrl-c-ctrl-c-hook'. See `org-bb-mode'."
  (org-bb-update-block t))

(provide 'org-bb)
;;; org-bb.el ends here
