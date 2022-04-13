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

(defconst org-bb-blocker-property "BB_BLOCKING")
(defconst org-bb-blockee-property "BB_BLOCKERS")

;;;###autoload
(define-minor-mode org-bb-mode
  "Turn on blocker block features in `org-mode'."
  :global t
  (if org-bb-mode
      (progn
        (add-hook 'org-trigger-hook #'org-bb-trigger)
        (add-hook 'org-blocker-hook #'org-bb-blocker))
    (progn
      (remove-hook 'org-trigger-hook #'org-bb-trigger)
      (remove-hook 'org-blocker-hook #'org-bb-blocker))))

(defun org-bb-blocker (change-plist)
  "Return t if the change is not blocked."
  (when (eq 'todo-state-change (plist-get change-plist :type))
    (let ((pos (plist-get change-plist :position)))
      (pcase (org-entry-get pos org-bb-blockee-property)
        (`nil t)
        ("block"
         (org-with-point-at pos
           (let ((entry-end (org-entry-end-position)))
             (save-match-data
               (save-excursion
                 (catch 'blockers-non-blocking
                   (when (re-search-forward org-block-regexp entry-end t)
                     (pcase (org-bb--matched-block)
                       (`((,begin . ,end) . ,plist)
                        (when (equal (match-string 1) "blockers")
                          (let* ((from (plist-get plist :from)))
                            (unless (and from (equal from (plist-get change-plist :to)))
                              (goto-char begin)
                              (when-let (link (org-bb--maybe-blocker end))
                                (setq org-block-entry-blocking link)
                                (throw 'blockers-non-blocking nil)))
                            (goto-char end))))))
                   t))))))
        (s
         (message "Invalid BB_BLOCKERS value: %s" s)
         nil)))))

(defun org-bb-trigger (change-plist)
  "Trigger changes according to the blocking property."
  (when (eq 'todo-state-change (plist-get change-plist :type))
    (let* ((pos (plist-get change-plist :position))
           (origin-id (org-id-get pos)))
      (dolist (target-id (org-entry-get-multivalued-property pos org-bb-blocker-property))
        (if-let (marker (org-id-find target-id 'markerp))
            (org-with-point-at marker
              (let ((entry-end (org-entry-end-position))
                    (todo (org-get-todo-state)))
                (when-let (new-todo
                           (catch 'new-todo
                             (while (re-search-forward org-block-regexp entry-end t)
                               (pcase (org-bb--matched-block)
                                 (`((,begin . ,block-end) . ,plist)
                                  (when (equal (match-string 1) "blockers")
                                    (let ((trigger (plist-get plist :trigger)))
                                      (goto-char begin)
                                      (when (and trigger
                                                 (equal (plist-get plist :from)
                                                        todo)
                                                 (not (org-bb--maybe-blocker block-end)))
                                        (throw 'new-todo trigger))
                                      (goto-char block-end))))))))
                  (org-todo new-todo)
                  (message "Set the state of \"%s\" to %s"
                           (org-link-display-format (org-get-heading t t t t))
                           new-todo))))
          (error "ID %s is not found" target-id))))))

(defun org-bb--matched-block ()
  "Return information on the blocked matched by the regexp."
  (when (equal (match-string 1) "blockers")
    (cons (cons (goto-char (nth 8 match))
                (nth 9 match))
          (read (concat "(" (or (match-string 3) "") ")")))))

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
                (org-with-point-at marker
                  (unless (org-entry-is-done-p)
                    (throw 'blockers-blocker-link link)))
              (message (concat "Not found: " link))
              (throw 'blockers-blocker-link link))))))))

;;;###autoload
(defun org-bb-insert-block ()
  "Insert a blockers block."
  (interactive)
  (require 'skeleton)
  (unless (derived-mode-p 'org-mode)
    (user-error "Please run this command in org-mode"))
  (org-bb--set-blockee-property)
  (skeleton-insert '(> "#+begin_blockers :from "
                       (concat "\"" (completing-read "Origin state: "
                                                     (mapcar #'car org-todo-kwd-alist)
                                                     nil t nil nil
                                                     (org-get-todo-state))
                               "\"")
                       " :trigger "
                       (concat "\"" (completing-read "Set the state when it is ready: "
                                                     (mapcar #'car org-todo-kwd-alist)
                                                     nil t)
                               "\"")
                       "\n"
                       _
                       "\n#+end_blockers")))

;;;###autoload
(defun org-bb-update-block ()
  "Update the blockers defined in the block at point."
  (interactive)
  (save-match-data
    (if (or (org-at-block-p)
            (and (org-in-block-p '("blockers"))
                 (thing-at-point-looking-at org-block-regexp)))
        (if (equal (match-string 1) "blockers")
            (let ((end (nth 9 (match-data)))
                  (target (org-id-get-create)))
              (org-bb--set-blockee-property)
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
          (user-error "Not on a \"blockers\" block"))
      (user-error "Not on a block"))))

(defun org-bb--set-blockee-property ()
  (org-entry-put nil org-bb-blockee-property "block"))

(provide 'org-bb)
;;; org-bb.el ends here
