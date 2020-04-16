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

The extraction is done via regex expresion in the variable defined in
'md-roam-title-regex.
It expects one or more space after the 'title:' key before the title value.
If the space is not there, the title extracted will be ':title value'.

At the moment, for some weird reason, the regex leaves one whitespace in front
of the title. 's-trim-left is used to remove it."

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

(defun md-roam--extract-wiki-links (file-path)
  "Extract links in the form of [[link]].
FILE-PATH is mandatory as org-roam--extract-links identifies it."
  (let (md-links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
        (let* ((to-file (concat (match-string-no-properties 1) ".md"))
               (end (match-end 1))
               (begin-of-block)
               (end-of-block)
               (content)
               (link-type "roam"))
          (when (f-file-p to-file)
            ;; get the text block = content around the link as context
            (forward-sentence)
            (setq end-of-block (point))
            (backward-sentence)
            (setq begin-of-block (point))
            (setq content (buffer-substring-no-properties begin-of-block end-of-block))
            (goto-char end) ; move back to the end of the regexp for the loop
            (setq md-links
                  (append md-links
                          (list (vector file-path ; file-from
                                        (file-truename (expand-file-name to-file (file-name-directory file-path))) ; file-to
                                        link-type ;
                                        (list :content content :point begin-of-block))))))))) ; properties
      md-links))

(defun md-roam--extract-cite-links (file-path)
  "Extract cites defined by @bibkey.
FILE-PATH is mandatory as org-roam--extract-links identifies it."
  (let (md-cite-links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pandoc-regex-parenthetical-citation-single nil t)
        (let* ((to-file (match-string-no-properties 3))
               (end (match-end 1))
               (begin-of-block)
               (end-of-block)
               (content)
               (link-type "cite"))
          (when to-file)
          (forward-sentence)
          (setq end-of-block (point))
          (backward-sentence)
          (setq begin-of-block (point))
          (setq content (buffer-substring-no-properties begin-of-block end-of-block))
          (goto-char end) ; move back to the end of the regexp for the loop
          (setq md-cite-links
                (append md-cite-links
                        (list (vector file-path ; file-from
                                      to-file
                                      link-type
                                      (list :content content :point begin-of-block))))))))  ; properties
    md-cite-links))

(defun md-roam--extract-links (orginal-extract-links &optional file-path)
  "Add markdown links (wiki and cite) for FILE-PATH to org-roam the equivalent.
ORGINAL-EXTRACT-LINKS is supplemented with md-roam functions.
It should be used in 'advice-add'."
  (let* ((file-path (or file-path
                        (file-truename (buffer-file-name))))
         (links (apply orginal-extract-links '(file-path)))
         (md-links (md-roam--extract-wiki-links file-path))
         (md-cite-links (md-roam--extract-cite-links file-path)))
                                        ;TODO remove pandoc-mode dependency
    (when md-links
      (setq links (append md-links links)))
    (when md-cite-links
      (setq links (append md-cite-links)))
    links))

(advice-add 'org-roam--extract-links :around #'md-roam--extract-links)

(provide 'md-roam)
;;; md-roam.el ends here
