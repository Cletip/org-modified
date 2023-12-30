;;; org-modified.el --- Track when org heading change  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Clément Payard

;; Author: Clément Payard <clement020302@gmail.com>
;; URL: https://github.com/Cletip/org-modified
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.6"))
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
   (concat org-modified-string " " "[" (cdr org-time-stamp-formats) "]")
   (concat "[" (cdr org-time-stamp-formats) "]"))
  "Cons of two strings: the first is inserted at the start, the second at the stop of tracking."
  :type '(cons string string)
  :group 'org-modified)

(defcustom org-modified-back-to-heading #'org-back-to-heading
  "Function to locate the heading where the open timestamp is placed. If the heading locating by this function has no id, create one to conserve the place where he is.
`org-modified-back-to-heading-with-id' is a good alternative. "
  :type 'function
  :group 'org-modified)

;; mettre un if org-roam present, alors org-roam-id-open
;; (org-id-open "9927c4c1-5bea-4251-a890-956ccce5ea3e" nil)
;; (org-roam-id-open '9927c4c1-5bea-4251-a890-956ccce5ea3e nil)
(defcustom org-modified-go-to-open-heading #'org-id-open
  "Function to locate the heading where the open timestamp is placed. If the heading locating by this function has no id, create one to conserve the place where he is.
`org-modified-back-to-heading-with-id' is a good alternative. "
  :type 'function
  :group 'org-modified)

(defcustom org-modified-fusion-time 1
  "Number of minutes specifying the interval for fusing timestamps of two events in Org mode.
Events with timestamps within this interval are considered for fusion."
  :type 'integer
  :group 'org-modified)

(defvar org-modified--open-heading nil "Id of the current open heading")

(defcustom org-modified-keep-id-p nil
  "If t, keep the id after the suppress of `org-modified--suppress-id-propertie'"
  :type 'boolean
  :group 'org-modified)

(defvar org-modified--suppress-id-propertie "ORG-MODIFIED-SUPPRESS" "Name of propertie to have know if we must suppress the id")

(defun org-modified-back-to-heading-with-id ()
  "Move to the first upper element/heading with an id."
  (while (and (not (org-entry-get (point) "ID")) (org-up-heading-safe))))

(defun org-modified-back-to-heading-with-id-roam ()
  "Move to the first upper element/heading with an id."
  (while (and (not (org-entry-get (point) "ID")) (org-up-heading-safe))))

(defun org-modified-beginning-of-metadatas-pos()
  "Return the position after last drawer."
  (save-excursion
    (funcall org-modified-back-to-heading)
    (outline-end-of-heading)
    (point)))

(defun org-modified-end-of-metadatas-pos()
  "Return the position after the heading after metadatas. If none, return the char where the drawer \"Logbook\" must me inserted"
  (save-excursion
    (funcall org-modified-back-to-heading)
    (org-end-of-meta-data t)
    (re-search-backward "[^\n]" nil t)
    (condition-case err
	(forward-line 1)
      (error
       nil))
    (beginning-of-line)
    (point)))

(defun org-modified--get-previous-date ()
  "Check if the timestamp from the current cursor position to end of line is newer than one day. Return nil if no previous date."
  (interactive)
  (save-excursion
    ;; move to the correct point
    (when (re-search-forward org-modified-string nil t)
      (re-search-forward org-modified-separator nil t)
      (re-search-forward org-element--timestamp-regexp nil t)
      ;; return the string
      (match-string-no-properties 1))))

(defun org-modified--fusion-p (date)
  "Return t if the current time is within `org-modified-fusion-time` minutes of DATE.
DATE is expected to be in a human-readable format."
  (let* ((date-time (date-to-time date)) ; Convert human-readable date to time format
         (current-time (current-time))    ; Get the current time
         (diff (time-subtract current-time date-time)) ; Calculate the difference
         (minutes-diff (/ (float-time diff) 60))) ; Convert the difference to minutes
    (<= minutes-diff (+ 1 org-modified-fusion-time))))

(defun org-modified--reopen-timestamp ()
  "At a logbook"
  (re-search-forward org-modified-string nil t)
  (re-search-forward org-modified-separator nil t)
  (goto-char (match-beginning 0))
  (delete-region (point) (line-end-position))
  )

(defun org-modified-open-timestamp (id)
  "Open timestamp for the current org-heading define by `org-modified-back-to-heading'"

  ;; check if there is a drawer. If none, open one
  (save-excursion
    (let ( (end (save-excursion (outline-next-heading) (- (point) 1))))
      (when (not (save-excursion (re-search-forward org-logbook-drawer-re end t)))
	(save-excursion
	  (goto-char (org-modified-end-of-metadatas-pos))
	  (insert ":LOGBOOK:" "\n"
		  ":END:" "\n")))))

  (save-excursion
    (let ((begin (org-modified-beginning-of-metadatas-pos))
	  (end (org-modified-end-of-metadatas-pos)))
      ;; go to place where insert
      (if org-log-into-drawer
	  (re-search-forward (concat ":" (org-log-into-drawer) ":") end t)
	(goto-char end))

      (let* ((previous-date (org-modified--get-previous-date)))
	;; several case here
	(cond
	 ;; TODO exeception, for example date of creation
	 ;; ((call each function in a list of exclude, with previous-date and id) nil)
	 ;; if previous date is org-modified-fusion-time before, open the previous timestamp
	 ((and previous-date (org-modified--fusion-p previous-date))

	  (org-modified--reopen-timestamp)

	  )
	 (t 
	  ;; insert new line with new open timestamp
	  (insert "\n" (format-time-string (car org-modified-template)))))
	))

    ;; save the id close the timestamp
    (setq org-modified--open-heading id)
    ;; hook to maintain consistent way even if the buffer is closed
    ;; replace by another function and a list ?
    (add-hook 'before-save-hook 'org-modified-close-timestamp nil t)))

(defun org-modified-close-timestamp ()
  "Function call to close the open timestamp"
  ;; for another file
  (save-window-excursion
    ;; same file
    (save-excursion
      ;; found id ?
      (if (org-id-find org-modified--open-heading 'marker)

	  (progn
	    ;; if yes, go to the id
	    (funcall org-modified-go-to-open-heading org-modified--open-heading nil)

	    ;; save excursion to clear the heading after
	    (save-excursion
	      ;; define where to start and end the research to close timestamp
	      (let ((end (org-modified-end-of-metadatas-pos)))

		;; go to the line where the timestamp is open
		(if (re-search-forward org-modified-string end t)
		    (progn
		      (end-of-line)
		      ;; close the timestamp
		      (insert (concat org-modified-separator (format-time-string (cdr org-modified-template))))

		      )
		  ;; case of not found org-modified-string
		  (message "Org-modified : Previous org-modified-string not found to close the timestamp, with ID: %s" org-modified--open-heading)
		  )

		)
	      )
	    
	    ;; after find the heading, we must clear that
	    ;; keep the id or not
	    (when (intern (org-entry-get (point) org-modified--suppress-id-propertie nil 'literal-nil))
	      (org-entry-delete nil "ID"))
	    ;; In all case, delete the property to know if we must delete ID
	    (org-entry-delete nil org-modified--suppress-id-propertie)
	    ;; remove local hook
	    (remove-hook 'before-save-hook 'org-modified-close-timestamp t)
	    

	    )

	;; case of not found the id
	(message "Org-modified : Previous heading not found to close timestamp. ID not found: %s" org-modified--open-heading)	
	)

      ;; suppress id of org-modified in all case, because closed
      (setq org-modified--open-heading nil)

      ;; TODO run some hook of org-modified here

      )))

(defun org-modified-update-timestamp()
  ""
  ;; because that can be call everywhere du to certain advices
  (when org-modified-mode
    (let ((current-heading-id nil))
      (save-excursion
	(save-restriction
	  (widen)
	  (when 
	      (and




	       ;; some problem occur when I do an undo or redo
	       (or
		(not (eq 'undo last-command)) (not (eq 'redo last-command))
		;; (not (eq 'undo this-command)) (not (eq 'redo this-command))

		)
	       ;; don't want if there is no back-heading
	       (condition-case err
		   (progn (funcall org-modified-back-to-heading)
			  
			  ;; if we don't have always seen this heading
			  (when (not (org-entry-get (point) org-modified--suppress-id-propertie))
			    ;; when a id before, don't suppress
			    (if (org-entry-get (point) "ID")
				(org-entry-put (point) org-modified--suppress-id-propertie "nil")
			      ;; if we want to keep the id, keep the id
			      (if org-modified-keep-id-p
				  (org-entry-put (point) org-modified--suppress-id-propertie "nil")
				;; else, remove the id when we finish
				(org-entry-put (point) org-modified--suppress-id-propertie "t"))))

			  ;; create an id if there is not : only way to have a unique place.
			  (setq current-heading-id (org-id-get-create)))
		 (error
		  nil))))

	  ;; case where open new one
	  ;; where (eq current-heading-id org-modified--open-heading), when need to do nothing
	  (when (not (equal current-heading-id org-modified--open-heading))
	    ;; close the old if there is one
	    (when org-modified--open-heading
	      (org-modified-close-timestamp))
	    ;; open the new
	    (org-modified-open-timestamp current-heading-id)
	    ))))))

;;;###autoload
(define-minor-mode org-modified-mode
  "Minor mode to add in a logbook when a heading has change and for how many times"
  :global nil
  (if org-modified-mode
      (progn

	(add-hook 'post-self-insert-hook 'org-modified-update-timestamp nil t)

	(advice-add 'yank :after #'org-modified-update-timestamp)
	(advice-add 'kill-region :after #'(lambda (_ _ &optional REGION) (org-modified-update-timestamp)))

	;; TODO if org-modified-log-states (log when todo, done, schedule, etc)

	;; if org-modified-org-schedule
	(advice-add 'org--deadline-or-schedule :after #'(lambda (_ _ _) (org-modified-update-timestamp)))
	

	)
    (progn
      ;; close open heading
      (org-modified-close-timestamp)

      (remove-hook 'post-self-insert-hook 'org-modified-update-timestamp t)

      (advice-remove 'yank #'org-modified-update-timestamp)
      (advice-remove 'kill-region  #'(lambda (_ _ &optional REGION) (org-modified-update-timestamp)))

      (advice-remove 'org--deadline-or-schedule #'(lambda (_ _ _) (org-modified-update-timestamp)))

      )))

(defun org-modified-mode-on ()
  "Function use to activate org-modified-mode with the global mode"
  (when (eq major-mode 'org-mode)
    (when (or (eq t org-modified-mode-include-files)
	      (member (buffer-file-name) org-modified-mode-include-files))
      (org-modified-mode))))

;;;###autoload
(define-globalized-minor-mode global-org-modified-mode org-modified-mode org-modified-mode-on)

(provide 'org-modified)

;;; org-modified.el ends here
