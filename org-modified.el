;;; org-modified.el -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2022 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: PAYARD Clément <clement020302@gmail.com>
;; URL: https://github.com/Cletip/org-modified
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

(defconst org-modified-string "MODIFIED:" "The word that is inserted before the template")

(defconst org-modified-separator "--" "The separator inserted between the two template in `org-modified-template'. Used to know if the line is closed")

(defvar org-modified-mode-include-files t "If this variable is t, org-modified-mode is for all files. If nil, desactivate. If list of file, org-modified-mode-global is active only in this files")

(defvar org-modified-template 
  (cons 
   (concat org-modified-string " " "[" (cdr org-timestamp-formats) "]")
   (concat "[" (cdr org-timestamp-formats) "]"))
  "Cons, the first is inserted when it's start, the second is for when that stop")

(defvar org-modified-back-to-heading #'org-back-to-heading
  "Name of the function to have the heading where to place the open timestamp at point. Good alternative is `org-modified-back-to-heading-with-id'"
  )

;;; Library
(defun org-modified-close-after-save ()
  "function to be called for the hook after save file"
  (org-modified-close-timestamp-in-file (buffer-file-name))
  )

(defun org-modified-close-timestamp-in-file (file)
  "Function call to close all the timestamp in a particular file"
  (save-window-excursion
    (find-file file)
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward org-modified-string nil t)
	(end-of-line)
	(when (not (org-modified-line-closed-p))
	  (org-modified-close-timestamp))
	(re-search-forward org-property-end-re nil t)))))

(defun org-modified-line-closed-p ()
  "Check if the line is closed"
  (string-match-p org-modified-separator
		  ;; return the line as string
		  (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun org-modified-close-timestamp ()
  ""
  ;; to avoid the infinite loop
  (let ((after-change-functions (remove 'org-modified-open-timestamp after-change-functions)))
    (insert (concat org-modified-separator (format-time-string (cdr org-modified-template))))))

(defun org-modified-back-to-heading-with-id ()
  "Place the point to the heading with an id"
  (while (and (not id) (org-up-heading-safe))
    (setq id (org-entry-get (point) "ID"))))

(defun org-modified-check-heading-open-timestamp-p ()
  "At point, return t if heading is open"
  (save-excursion
    (let ((end (org-modified-end-of-meta-data-char)))
(when
    (search-forward org-modified-string end t)
  (not (org-modified-line-closed-p))))))

(defun org-modified-open-timestamp (START END OLD-LEN)
  "Open timestamp for the current org-heading define by `org-modified-back-to-heading'"
  (save-excursion
    ;; go to heading
    (funcall org-modified-back-to-heading)

    ;; checked that a heading is not already open
    (when (not (org-modified-check-heading-open-timestamp-p))
      ;; now, let's insert the timestamp to begin the tracking
      (let ((end (save-excursion (outline-next-heading) (point))))
	;; when we don't find the logbook drawer, create logbook
	(when (not (save-excursion (re-search-forward org-modifiedbook-drawer-re end t)))
	  (save-excursion
	    (goto-char (org-modified-end-of-meta-data-char))
	    (insert ":LOGBOOK:" "\n"
		    ":END:" "\n")
	    ))

	;; after that, go to logbook
	(re-search-forward (concat ":" (org-modified-into-drawer) ":") end t)
	;; and insert a new line
	(insert "\n" (format-time-string (car org-modified-template)))))))

(defun org-modified-end-of-meta-data-char()
  "Return the char after the last drawer"
  (save-excursion
    (org-end-of-meta-data t)
    (re-search-backward "[^\n]" nil t)
    (next-line)
    (beginning-of-line)
    (point)))



;;;###autoload
(define-minor-mode org-modified-mode
  "Minor mode to add in a logbook when a heading has change and for how many times"
  :global nil
  (if org-modified-mode
      (progn
	(add-hook 'after-save-hook 'org-modified-close-after-save nil t)
	(add-hook 'after-change-functions 'org-modified-open-timestamp nil t)
	)
    (progn
      (remove-hook 'after-save-hook 'org-modified-close-after-save t)
      (remove-hook 'after-change-functions 'org-modified-open-timestamp t)
      )))

(defun org-modified-mode-on ()
  "Function use to activate org-modified-mode with the global mode"
  (when (eq major-mode 'org-mode)
    (when (or (eq t org-modified-mode-include-files)
	      (memq (buffer-file-name) org-modified-mode-include-files))
      (org-modified-mode))))

;;;###autoload
(define-globalized-minor-mode org-modified-global-mode org-modified-mode org-modified-mode-on)

(provide 'org-modified)

;;; org-modified.el ends here
