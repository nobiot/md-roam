;;; md-roam.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Noboru Ota
;;
;; Author: Noboru Ota <http://github/I040050>
;; Maintainer: Noboru Ota <me@nobiot.com>
;; Created: April 15, 2020
;; Modified: April 15, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/I040050/md-roam
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:
;;;

(require 's)
(require 'org-roam)

;;;; md-roam addtional variables
(defvar md-roam-title-regex
  (concat "\\(^title:[[:blank:]]*\\)"   ; The line needs to begin with 'title:',
                                        ; followed by 0-n spaces or tabs.
                                        ; YAML might insist on whitespace, but
                                        ; here we can be more lenient
          "\\(.+\n\\)" ; Actual title string (1-n characters)
                      ))

;;;; functions
(defun md-roam--extract-title-from-current-buffer ()
  "Extract title from the current buffer (markdown file with YAML frontmatter).

This function looks fo the YAML frontmatter deliniator '---' begining of
the buffer. No space is allowed before or after the deliniator.

It assumes:
 (1) Current buffer is a markdonw file (but does not check it)
 (2) It has title in the YAML frontmatter on top of the file
 (3) The format is 'title: The Document Title Value'
 (4) The title value is escaped by the double quotations

The extraction is done via regex expresion in defined in 'md-roam-title-regex.
It expects one or more space after the 'title:' key before the title value.
If the space is not there, the title extracted will be ':title value'.

At the moment, for some weird reason, the regex leaves one whitespace in front
of the title. 's-trim-left is used to remove it.


TODO At the moment, an empty title is fine, but if there is no space or one
     space, it returns ':' as the title; if more than one space, ''.
TODO Bind the regexp search for the tile. At the moment, the title key: value
     does not even have to be wihtin the YAML frontmatter, as long as there is
     '---' at the top of the file!
TODO Ideally, I do not want s-trim-left to be there, but I could not figure
     out how.
I will move these issues to GitHub/GitLab."

  (when
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^---\n" 5 t nil))
    (when (string-match md-roam-title-regex (buffer-string))
      (s-trim-left (match-string-no-properties 2)))))

(defun md-roam--extract-titles (alias-list)
  "Add markdown titles to the ALIAS-LIST returned form org-roam--extract-titles."
  (let ((md-title (md-roam--extract-title-from-current-buffer)))
    (if md-title (cons md-title alias-list)
      alias-list)))

(advice-add 'org-roam--extract-titles :filter-return #'md-roam--extract-titles)


(provide 'md-roam)
;;; md-roam.el ends here
