;;; md-roam.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Noboru Ota
;;
;; Author: Noboru Ota <https://github.com/nobiot>, <https://gitlab.com/nobiot>
;; Maintainer: Noboru Ota <me@nobiot.com>
;; Created: April 15, 2020
;; Modified: May 17, 2020
;; Version: 1.2.1
;; Keywords:
;; Homepage: https://github.com/nobiot/md-roam, https://gitlab.com/nobiot/md-roam
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Use org-roam with markdown files by adding md-roam to it. md-roam extends
;;  the features and functions provided by org-roam to support markdown files
;;  in addition to org files.
;;
;;  Refer to README in the GitHub / GitLab repo for instruction, configuraiton,
;;  and features supported.
;;
;;; Code:
;;;

(eval-when-compile (require 'subr-x))
(require 'dash)
(require 's)
(require 'f)
(declare-function org-roam--file-name-extension 'org-roam)
(declare-function org-roam--str-to-list 'org-roam)

;;; Md-roam addtional variables

;;; Regexp for the beginning and ending of YAML front matter section
;;; In markdown-mode, beginning and ending are the same: "---".
;;; Separate regular expressions are defined here because in some
;;; markdown conventions, the ending is delinated by "```".
;;; This can be potentially supported setting a custom regexp for
;;; the ending deliniator.

;;;; These regular expressions are modified versino of
;;;; `markdown-regex-yaml-metadata-border.
;;;; I am adding "^" to indicate that the a line needs to
;;;; start with the deliniator.

(defvar md-roam-regex-yaml-font-matter-beginning
  "\\(^-\\{3\\}\\)$")

(defvar md-roam-regex-yaml-font-matter-ending
  "\\(^-\\{3\\}\\)$")
  ;If you change this to "\\(^`'\\{3\\}\\)$"), you shoul dbe able to
  ;support YAML matter ending with "```". I am not testing it, though.

;;;  Regexp for pandoc style citation for link extraction
;;;  Copy from pandco-mode to remove dependency
;;
;;   [@bibkey], [@bibkey1, xx; bibkey2, yy]
;;   https://pandoc.org/MANUAL.html#citations
;;
;;   Blah blah [see @doe99, pp. 33-35; also @smith04, chap. 1].
;;   Blah blah [@doe99, pp. 33-35, 38-39 and *passim*].
;;   Blah blah [@smith04; @doe99].
;;   [@smith{ii, A, D-Z}, with a suffix]
;;   [@smith, {pp. iv, vi-xi, (xv)-(xvii)} with suffix here]
;;
;;   A minus sign (-) before the @ will suppress mention
;;
;;     Smith says blah [-@smith04].
;;
;;   in-text citation:
;;
;;     @smith04 says blah.
;;
;;     @smith04 [p. 33] says blah.

;; With my testing, citation-2 can detect all the cases for md-roam

(defvar md-roam-regex-in-text-citation-2
  "\\(?:[^[:alnum:]]\\|^\\)\\(-?@\\)\\([-a-zA-Z0-9_+:]+\\)"
  "Regular expression for stand-alone citation with no anchor.")

(defvar md-roam-verbose t)

;;; Md-roam customizing

(defcustom md-roam-file-extension-single "md"
  "Defines the extesion to be used for md-roam wihtin org-roam.
Unlike 'org-roam-file-extension', this is a single value, not a list.
It is assumed to be a markdown file extension, e.g. .md, and .markdown."
  :type '(repeat string)
  :group 'org-roam)

;;; Md-roam functions

;;;  Extracting title from markdown files (YAML frontmatter)
;;;  Add advice to org-roam--extract-and-format-titles

(defun md-roam-get-yaml-front-matter ()
  "Return the text of the YAML front matter of the current buffer.
Return nil if the front matter does not exist, or incorrectly deliniated by
'---'."

  (save-excursion
    (goto-char (point-min))
    (when-let
        ((startpoint (re-search-forward
                     md-roam-regex-yaml-font-matter-beginning nil t 1))
         (endpoint (re-search-forward
                    md-roam-regex-yaml-font-matter-ending nil t 1)))
      (buffer-substring-no-properties startpoint endpoint))))

(defun org-roam--extract-titles-mdtitle ()
  "Extract title from the current buffer (markdown file with YAML frontmatter).

This function looks for the YAML frontmatter deliniator '---' begining of
the buffer. No space is allowed before or after the deliniator.

It assumes:
 (1) Current buffer is a markdonw file (but does not check it)
 (2) It has title in the YAML frontmatter on top of the file
 (3) The format is 'title: The Document Title Value'"

    (let ((frontmatter (md-roam-get-yaml-front-matter)))
    (cond (frontmatter
           (string-match "\\(^title:[ \t]*\\)\\(.*\\)" frontmatter)
           (list (match-string-no-properties 2 frontmatter))))))

(defun org-roam--extract-titles-mdalias ()
  "Return list of aliases from the front matter section of the current buffer.
Return nil if none."
  (let ((frontmatter (md-roam-get-yaml-front-matter)))
    (cond (frontmatter
           (string-match "\\(^.*ROAM_ALIAS:[ \t]*\\)\\(.*\\)" frontmatter)
           (org-roam--str-to-list (match-string-no-properties 2 frontmatter))))))

(defun org-roam--extract-titles-mdheadline ()
  "WIP: Return the first headline of the current buffer."
  )

;;; Extract links in markdown file (wiki and pandocy-style cite)
;;; Add advice to org-roam--extract-links

(defun md-roam--extract-wiki-links (file-path)
  "Extract links in the form of [[link]].
FILE-PATH is mandatory as `org-roam--extract-links' identifies it."
  (let (md-links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
        (let* ((to-file (concat (match-string-no-properties 1) "." md-roam-file-extension-single))
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
FILE-PATH is mandatory as `org-roam--extract-links' identifies it."
  (let (md-cite-links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward md-roam-regex-in-text-citation-2 nil t)
        (let* ((to-file (match-string-no-properties 2))
               (end (match-end 1))
               (begin-of-block)
               (end-of-block)
               (content)
               (link-type "cite"))
          (when to-file)
          (forward-paragraph)
          (setq end-of-block (point))
          (backward-paragraph)
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

(defun md-roam--extract-links (original-extract-links &optional file-path)
  "Add markdown links (wiki and cite) for FILE-PATH to the org-roam equivalent.
ORIGINAL-EXTRACT-LINKS is supplemented with md-roam functions.
It should be used with 'advice-add'."
  (let* ((file-path (or file-path
                        (file-truename (buffer-file-name))))
         (links (apply original-extract-links file-path nil))
         (md-links (md-roam--extract-wiki-links file-path))
         (md-cite-links (md-roam--extract-cite-links file-path)))

    (when md-links
      (setq links (append md-links links)))
    (when md-cite-links
      (setq links (append md-cite-links links)))
    links))

(advice-add 'org-roam--extract-links :around #'md-roam--extract-links)


;;;; Adapt behaviour of org-roam-insert
;;;; Add advice to 'org-roam--format-link

(defun md-roam--format-link (target &optional description)
  "Formats a [[wikilink]] for a given file TARGET and link DESCRIPTION.
Add advice to 'org-roam--format-link' within 'org-roam-inert'.
Customize `org-roam--file-name-extension' to define the extesion (e.g. md) that
follow this behaviour."

  (let ((ext (org-roam--file-name-extension (buffer-file-name (buffer-base-buffer)))))
    (if (string= ext md-roam-file-extension-single)
        (let* ((here (ignore-errors
                       (-> (or (buffer-base-buffer)
                               (current-buffer))
                           (buffer-file-name)
                           (file-truename)
                           (file-name-directory)))))
          (concat "[[" (file-name-sans-extension (if here
                                                     (file-relative-name target here)
                                                   target))
                  "]]"
                  " " description))
      nil)))

(advice-add 'org-roam--format-link :before-until #'md-roam--format-link)

;;;; Add advice to org-roam-db-build-cache

(defun md-roam-add-message-to-db-build-cache (&optional force)
  "Add a message to the return message from `org-roam-db-build-cache'.
This is to simply indicate that md-roam is active. FORCE does not do anythying."
  (when force) ;do nothing
  (when md-roam-verbose
    (message "md-roam is active")))

(advice-add 'org-roam-db-build-cache :before #'md-roam-add-message-to-db-build-cache)


(provide 'md-roam)
;;; md-roam.el ends here
