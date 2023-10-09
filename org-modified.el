;;; org-modified.el --- Track when org heading change  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Clément Payard

;; Author: Clément Payard <clement020302@gmail.com>
;; URL: https://github.com/Cletip/org-modified
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.4"))
;; Keywords: track timestamp org

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Readme.

;;; Code:

(require 'org)

(defgroup org-modified nil
  "Group for customizing org-modified, a package for tracking and logging modifications in Org mode files.
It provides facilities to insert and manage timestamps associated with modifications within Org mode documents."
  :prefix "org-modified-"
  :group 'org
  :link '(url-link :tag "GitHub Repository" "https://github.com/YourGitHubUsername/org-modified")
  :tag "Org Modified")

(defcustom org-modified-string "MODIFIED:"
  "The word that is inserted before the template in `org-modified-template'."
  :type 'string
  :group 'org-modified)

(defcustom org-modified-separator "--"
  "The separator inserted between the two templates in `org-modified-template'. Used to determine if the line is closed."
  :type 'string
  :group 'org-modified)

(defcustom org-modified-mode-include-files t
  "If this variable is t, org-modified-mode is for all files.
If nil, deactivate org-modified-mode.
If list of file, org-modified-mode-global is active only in these files."
  :type '(choice (const :tag "All Files" t)
                 (const :tag "No Files" nil)
                 (repeat :tag "Specific Files" file))
  :group 'org-modified)

(defcustom org-modified-template
  (cons 
   (concat org-modified-string " " "[" (cdr org-timestamp-formats) "]")
   (concat "[" (cdr org-timestamp-formats) "]"))
  "Cons of two strings: the first is inserted at the start, the second at the stop of tracking."
  :type '(cons string string)
  :group 'org-modified)

(defcustom org-modified-back-to-heading #'org-back-to-heading
  "Function to locate the heading where the open timestamp is placed.
`org-modified-back-to-heading-with-id' is a good alternative."
  :type 'function
  :group 'org-modified)

(defun org-modified-back-to-heading-with-id ()
  "Move to the first upper element/heading with an id."
  (while (and (not (org-entry-get (point) "ID")) (org-up-heading-safe))))

(defun org-modified-end-of-meta-data-char()
  "Return the position of after last drawer."
  (save-excursion
    (org-end-of-meta-data t)
    (re-search-backward "[^\n]" nil t)
    (next-line)
    (beginning-of-line)
    (point)))

(defun org-modified-line-closed-p ()
  "Check if the line is closed."
  (string-match-p org-modified-separator
		  ;; return the line as string
		  (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun org-modified-check-heading-open-timestamp-p ()
  "At point, return t if heading is open"
  (save-excursion
    (let ((end (org-modified-end-of-meta-data-char)))
      (when (search-forward org-modified-string end t)
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
	(when (not (save-excursion (re-search-forward org-logbook-drawer-re end t)))
	  (save-excursion
	    (goto-char (org-modified-end-of-meta-data-char))
	    (insert ":LOGBOOK:" "\n"
		    ":END:" "\n")))
	;; after that, go to logbook
	(re-search-forward (concat ":" (org-log-into-drawer) ":") end t)
	;; and insert a new line
	(insert "\n" (format-time-string (car org-modified-template)))))))

(defun org-modified-close-timestamp ()
  "Function that close the opent timestamp with the separator `org-modified-separator'"
  ;; to avoid an infinite loop where the is an insertion, then modification, so insertion, etc.
  (let ((after-change-functions (remove 'org-modified-open-timestamp after-change-functions)))
    (insert (concat org-modified-separator (format-time-string (cdr org-modified-template))))))

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

(defun org-modified-close-after-save ()
  "Function to be called for the hook after save file"
  (org-modified-close-timestamp-in-file (buffer-file-name)))

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
