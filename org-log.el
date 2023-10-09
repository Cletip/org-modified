;;; org-log.el -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: PAYARD Clément <clement020302@gmail.com>
;; URL: https://github.com/Cletip/org-log
;; Keywords: org-mode, roam, convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org "9.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;
;;; Code:


(defconst org-log-string "MODIFIED:" "The word that is inserted before the template")

(defconst org-log-separator "--" "The separator inserted between the two template in `org-log-template'. Used to know if the line is closed")

(defvar org-log-template 
  (cons 
   (concat org-log-string " " "[" (cdr org-timestamp-formats) "]")
   (concat "[" (cdr org-timestamp-formats) "]"))
  "Cons, the first is inserted when it's start, the second is for when that stop")

(defun org-log-close-after-save ()
  "function to be called for the hook after save file"
  (org-log-close-timestamp-in-file (buffer-file-name))
  )

(defun org-log-close-timestamp-in-file (file)
  "Function call to close all the timestamp in a particular file"
  (save-window-excursion
    (find-file file)
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward org-log-string nil t)
	(end-of-line)
	(when (not (org-log-line-closed-p))
	  (org-log-close-timestamp))
	(re-search-forward org-property-end-re nil t)))))

(defun org-log-line-closed-p ()
  "Check if the line is closed"
  (string-match-p org-log-separator
		  ;; return the line as string
		  (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun org-log-close-timestamp ()
  ""
  ;; to avoid the infinite loop
  (let ((after-change-functions (remove 'org-log-open-timestamp after-change-functions)))
    (insert (concat org-log-separator (format-time-string (cdr org-log-template))))))

(defun org-log-find-modified-line ()
  "find and get at the end of the line with MODIFIED"
  )

(defvar org-log-back-to-heading #'org-back-to-heading
  "Name of the function to have the heading where to place the open timestamp at point. Good alternative is `org-log-back-to-heading-with-id'"
  )

(defun org-log-back-to-heading-with-id ()
  "Place the point to the heading with an id"
  (while (and (not id) (org-up-heading-safe))
    (setq id (org-entry-get (point) "ID"))))

(defun org-log-check-heading-open-timestamp-p ()
  "At point, return t if heading is open"
  (save-excursion
    (let ((end (org-log-end-of-meta-data-char)))
(when
    (search-forward org-log-string end t)
  (not (org-log-line-closed-p))))))

(defun org-log-open-timestamp (START END OLD-LEN)
  "Open timestamp for the current org-heading define by `org-log-back-to-heading'"
  (save-excursion
    ;; go to heading
    (funcall org-log-back-to-heading)

    ;; checked that a heading is not already open
    (when (not (org-log-check-heading-open-timestamp-p))
      ;; now, let's insert the timestamp to begin the tracking
      (let ((end (save-excursion (outline-next-heading) (point))))
	;; when we don't find the logbook drawer, create logbook
	(when (not (save-excursion (re-search-forward org-logbook-drawer-re end t)))
	  (save-excursion
	    (goto-char (org-log-end-of-meta-data-char))
	    (insert ":LOGBOOK:" "\n"
		    ":END:" "\n")
	    ))

	;; after that, go to logbook
	(re-search-forward (concat ":" (org-log-into-drawer) ":") end t)
	;; and insert a new line
	(insert "\n" (format-time-string (car org-log-template)))))))

(defun org-log-end-of-meta-data-char()
  "Return the char after the last drawer"
  (save-excursion
    (org-end-of-meta-data t)
    (re-search-backward "[^\n]" nil t)
    (next-line)
    (beginning-of-line)
    (point)))



(define-minor-mode org-log-mode
  "Minor mode to add in a logbook when a heading has change and for how many times"
  :global nil
  (if org-log-mode
      (progn
	(add-hook 'after-save-hook 'org-log-close-after-save nil t)
	(add-hook 'after-change-functions 'org-log-open-timestamp nil t)
	)
    (progn
      (remove-hook 'after-save-hook 'org-log-close-after-save t)
      (remove-hook 'after-change-functions 'org-log-open-timestamp t)
      )))

(defun org-log-mode-on ()
  "Function use to activate org-log-mode with the global mode"
  (when (eq major-mode 'org-mode)
    (when (or (eq t org-log-mode-include-files)
	      (memq (buffer-file-name) org-log-mode-include-files))
      (org-log-mode))))

(define-globalized-minor-mode org-log-global-mode org-log-mode org-log-mode-on)

(defvar org-log-mode-include-files t "If this variable is t, org-log-mode is for all files. If nil, desactivate. If list of file, org-log-mode-global is active only in this files")

(provide 'org-log)


;;; org-log.el ends here
