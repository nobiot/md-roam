;;; md-roam.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Noboru Ota
;;
;; Author: Noboru Ota <https://github.com/nobiot>, <https://gitlab.com/nobiot>
;; Maintainer: Noboru Ota <me@nobiot.com>
;; Created: April 15, 2020
;; Modified: March 20, 2021
;; Version: 2.0.0
;; Keywords:
;; Homepage: https://github.com/nobiot/md-roam, https://gitlab.com/nobiot/md-roam
;; Package-Requires: ((emacs 26.3) (dash) (s) (f) (org-roam 2.0.0))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Use org-roam with markdown files by adding md-roam to it.  md-roam extends
;;  the features and functions provided by org-roam to support markdown files
;;  in addition to org files.
;;
;;  Refer to README in the GitHub / GitLab repo for instruction, configuraiton,
;;  and features supported.
;;
;;  With V2, bullet list with asterisk "*" conflicts with Org-mode's syntax
;;  for headlines. It does not correctly save the buffer when used in markdown
;;  file in `org-roam-directory'.
;;
;;; Code:
;;;

(eval-when-compile (require 'subr-x))
(require 'org)
;;(require 'dash)
;;(require 's)
;;(require 'f)
;;(require 'org-roam-mode)
;;(require 'org-roam-node)
;;(require 'org-roam-db)

;;; Md-roam addtional variables

;;; Regexp for the beginning and ending of YAML front matter section
;;; In markdown-mode, beginning and ending are the same: "---".
;;; Separate regular expressions are defined here because in some
;;; markdown conventions, the ending is delineated by "```".
;;; This can be potentially supported setting a custom regexp for
;;; the ending delineator.

;;;; These regular expressions are modified version of
;;;; `markdown-regex-yaml-metadata-border.
;;;; I am adding "^" to indicate that the a line needs to
;;;; start with the delineator.

(defvar md-roam-regex-yaml-font-matter-beginning
  "\\(^-\\{3\\}\\)$")

(defvar md-roam-regex-yaml-font-matter-ending
  "\\(^-\\{3\\}\\)$")
;; "If you change this to `\\(^`'\\{3\\}\\)$', you should be able to
;; support YAML matter ending with ```. I am not testing it, though.")

(defvar md-roam-regex-title
  "\\(^title:[ \t]*\\)\\(.*\\)")

(defvar md-roam-regex-id
  "\\(^id:[ \t]*\\)\\(.*\\)")

(defvar md-roam-regex-aliases
  ;; Assumed to be case insensitive
  "\\(^.*ROAM_ALIAS:[ \t]*\\)\\(.*\\)")

(defvar md-roam-regex-ref-key
  ;; Assumed to be case insensitive
  "\\(^.*ROAM_KEY:[ \t]*\\)\\(.*\\)")

(defvar md-roam-regex-headline
  (concat "^\s*\n"                   ;exludes YAML front matter
          "\\(.*$\\)\n\\(^[=-]+$\\)" ;heading with '=' and '-'
          "\\|"                      ;regex 'or'
          "\\(^#+ \\)\\(.*$\\)"))    ;heading with '#'

(defvar md-roam-regex-in-text-citation-2
  "\\(?:[^[:alnum:]]\\|^\\)\\(-?@\\)\\([-a-zA-Z0-9_+:]+\\)"
  "Regular expression for stand-alone citation with no anchor.
Regexp for pandoc style citation for link extraction
Copy from pandco-mode to remove dependency

[@bibkey], [@bibkey1, xx; bibkey2, yy]
https://pandoc.org/MANUAL.html#citations

Blah blah [see @doe99, pp. 33-35; also @smith04, chap. 1].
Blah blah [@doe99, pp. 33-35, 38-39 and *passim*].
Blah blah [@smith04; @doe99].
[@smith{ii, A, D-Z}, with a suffix]
[@smith, {pp. iv, vi-xi, (xv)-(xvii)} with suffix here]

A minus sign (-) before the @ will suppress mention

Smith says blah [-@smith04].

in-text citation:

@smith04 says blah.

@smith04 [p. 33] says blah.

With my testing, citation-2 can detect all the cases for md-roam")

(defvar md-roam-regex-tags-zettlr-style
  "\\([^/s]\\)\\([#@][[:alnum:]_-]+\\)"
  "Regexp for extracting tags.
This regexp is intended to be compatible with Zettlr:

     `#tag1 #tag-with-hyphen #tag_with_underscore'

Note that iA Writer treats hyphen (-) as a word delimiter
within a tag. That is, iA Writer treats #tag-hyphen tagged as
#tag, and ignores `-hyphen'.

If iA Writer's stricter style is preferred, the regexp should
be defined as:

     `\\([^/s]\\)\\([#@][[:alnum:]_]+\\)'")

(defconst md-roam-regex-link-inline
  "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?4:\\]\\)\\(?5:(\\)\\(?6:[^)]*?\\)\\(?:\\s-+\\(?7:\"[^\"]*\"\\)\\)?\\(?8:)\\)"
  "Copy of markdown-regex-link-inline from Markdown Mode.
Regexp for inline links [description](link) and images ![description](link).

Regular expression for a [text](file) or an image link ![text](file).
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket.
Group 5 matches the opening parenthesis.
Group 6 matches the URL.
Group 7 matches the title (optional).
Group 8 matches the closing parenthesis.")

(defvar md-roam-verbose t)

;;; Md-roam customizing

(defcustom md-roam-file-extension-single "md"
  "Defines the extesion to be used for Md-roam within Org-roam directory.
Unlike 'org-roam-file-extension', this is a single value, not a list.
It is intended to be used for you to define a different markdown extension,
such as .md and .markdown."

  :type 'string
  :group 'org-roam)

(defcustom md-roam-use-org-file-links t
  "Defines if Md-roam extracts Org's file link for backlinks in md files.
Default is t, that is to extract both wiki-links and Org's file links.
For faster performance, set it to nil to extract only
[[wiki-links]] of Md-roam and ignore the Org file links.
This does not affect Org files within Org-roam directory."

  :type 'boolean
  :group 'org-roam)

(defcustom md-roam-use-org-extract-ref t
  "Defines if Md-roam extracts REF_KEY using Org-roam logic.
Default is t, that is to use #+ref_key with Org-roam logic.
If nil, Md-roam uses its own regex to look for #+roam_key: or
roam_key: within YAML front matter only.
It's intended for better performance, and aesthetic style.
Recommended if your bibliographic notes are written in markdown files.
Leave it as default, if they are written in org files."

  :type 'boolean
  :group 'org-roam)

(defcustom md-roam-use-markdown-file-links nil
  "Defines if Md-roam extracts links defined via Markdown syntax.
Default is nil. If enabled, Md-roam searches the buffer for links
  [descriptoin](path/to/file.ext)."

  :type 'boolean
  :group 'org-roam)

;;; Md-roam commands
;;;###autoload
(define-minor-mode md-roam-mode
  "."
  :init-value nil
  :lighter "md-roam"
  :global t
  (cond
   (md-roam-mode
    ;; Activate
    (advice-add #'org-roam-db-update-file :before-until #'md-roam-db-update-file)
    (advice-add #'org-roam-node-at-point :before-until #'md-roam-node-at-point)
    (advice-add #'org-id-get :before-until #'md-roam-id-get)
    (advice-add #'org-id-find-id-in-file :before-until #'md-roam-find-id-in-file)
    (advice-add #'org-roam-node-insert :before-until #'md-roam-node-insert)
    (add-hook #'org-open-at-point-functions #'md-roam-open-id-at-point))
   (t
    ;; Deactivate
    (advice-remove #'org-roam-db-update-file #'md-roam-db-update-file)
    (advice-remove #'org-roam-node-at-point #'md-roam-node-at-point)
    (advice-remove #'org-id-get #'md-roam-id-get)
    (advice-remove #'org-id-find-id-in-file #'md-roam-find-id-in-file)
    (advice-remove #'org-roam-node-insert #'md-roam-node-insert)
    (remove-hook #'org-open-at-point-functions #'md-roam-open-id-at-point))))

;;; Md-roam functions
;;;  Tell if a file is an .org file (or encrypted org file)
(defun md-roam--org-file-p (path)
  "Check if PATH is pointing to an org file.
Return t or nil."
  (let ((ext (org-roam--file-name-extension path)))
    (when (string= ext "gpg")           ; Handle encrypted files
      (setq ext (org-roam--file-name-extension (file-name-sans-extension path))))
    (string= ext "org")))

;;;  Extracting title from markdown files (YAML frontmatter)
;;;  Add advice to org-roam--extract-and-format-titles

(defun md-roam-get-yaml-front-matter ()
  "Return the text of the YAML front matter of the current buffer.
Return nil if the front matter does not exist, or incorrectly delineated by
'---'. The front matter is required to be at the beginning of the file."

  (save-excursion
    (goto-char (point-min))
    (when-let
        ((startpoint (re-search-forward
                      md-roam-regex-yaml-font-matter-beginning 4 t 1))
         ;The beginning needs to be in the beginning of buffer
         (endpoint (re-search-forward
                    md-roam-regex-yaml-font-matter-ending nil t 1)))
      (buffer-substring-no-properties startpoint endpoint))))

(defun md-roam-get-yaml-front-matter-endpoint ()
  "Return the endpoint of the YAML front matter of the current buffer.
Return nil if the front matter does not exist, or incorrectly delineated by
'---'. The front matter is required to be at the beginning of the file."

  (save-excursion
    (goto-char (point-min))
    (when-let
        ((startpoint (re-search-forward
                      md-roam-regex-yaml-font-matter-beginning 4 t 1))
         ;The beginning needs to be in the beginning of buffer
         (endpoint (re-search-forward
                    md-roam-regex-yaml-font-matter-ending nil t 1)))
      endpoint)))

(defun md-roam-extract-title ()
  "Extract title from the current buffer (markdown file with YAML frontmatter).

This function looks for the YAML frontmatter delineator '---' begining of
the buffer. No space is allowed before or after the delineator.

It assumes:
 (1) Current buffer is a markdonw file (but does not check it)
 (2) It has title in the YAML frontmatter on top of the file
 (3) The format is 'title: The Document Title'"

    (let ((frontmatter (md-roam-get-yaml-front-matter)))
    (when (and frontmatter
               (string-match md-roam-regex-title frontmatter))
      (match-string-no-properties 2 frontmatter))))

(defun md-roam-extract-id ()
  "Extract id from the current buffer (markdown file with YAML frontmatter).

This function looks for the YAML frontmatter delineator '---' begining of
the buffer. No space is allowed before or after the delineator.

It assumes:
 (1) Current buffer is a markdonw file (but does not check it)
 (2) It has title in the YAML frontmatter on top of the file
 (3) The format is 'id: <string>'"

    (let ((frontmatter (md-roam-get-yaml-front-matter)))
    (when (and frontmatter
               (string-match md-roam-regex-id frontmatter))
      (match-string-no-properties 2 frontmatter))))

(defun md-roam--remove-single-quotes (str)
  "Check if STR is surrounded by single-quotes, and remove them.
If not, return STR as is."
  (let ((regexp "\\('\\)\\(.*\\)\\('\\)"))
    (if (string-match regexp str)
        (match-string-no-properties 2 str)
      str)))

(defun md-roam--yaml-seq-to-list (seq)
  "Return a list from YAML SEQ formatted in the flow style.
SEQ = sequence, it's an array. At the moment, only the flow style works.

See the spec at https://yaml.org/spec/1.2/spec.html
  Flow style: !!seq [ Clark Evans, Ingy döt Net, Oren Ben-Kiki ]."

;; The items in the sequence (array) can be separated by different ways.
;;   1. Spaces like the example from the spec above
;;   2. Single-quotes 'item'
;;   3. Double-quotes "item"
;; Do not escape the singe- or double-quotations. At the moment, that does
;; lead to error

;; The regexp is meant to to match YAML sequence formatted in the flow style.
;; At the moment, only the flow style is considered. The number of spaces
;; between the squeare bracket and the first/last item should not matter.
;; [item1, item2, item3] and [ item1, item2, item3 ] should be equally valid.

  (let ((regexp "\\(\\[\s*\\)\\(.*\\)\\(\s*\\]\\)")
        (separator ",\s*"))
    (when (string-match regexp seq)
      (let ((items (split-string-and-unquote
                    (match-string-no-properties 2 seq) separator)))
        (mapcar #'md-roam--remove-single-quotes items)))))

;;;; Synchronization
(defun md-roam-db-update-file (&optional file-path)
  "Update Org-roam cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information.

`org-element-parse-buffer' cannot be used for markdown files; it
causes infinite loop."
  (when (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer)))
    (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
    (let ((content-hash (org-roam-db--file-hash file-path))
          (db-hash (caar (org-roam-db-query [:select hash :from files
						     :where (= file $s1)] file-path)))
          info)
      (unless (string= content-hash db-hash)
	(org-roam-with-file file-path nil
	  (emacsql-with-transaction
	   (org-roam-db)
	   (save-excursion
	     ;; (org-set-regexps-and-options 'tags-only)
	     (org-roam-db-clear-file)
	     (org-roam-db-insert-file)
	     (md-roam-db-insert-file-node)
	     ;; (setq org-outline-path-cache nil)
	     ;; (org-roam-db-map-nodes
	     ;;  (list #'org-roam-db-insert-node-data
	     ;;        #'org-roam-db-insert-aliases
	     ;;        #'org-roam-db-insert-tags
	     ;;        #'org-roam-db-insert-refs))
	     ;; (setq org-outline-path-cache nil)
	     ;; (setq info (org-element-parse-buffer))
	     (md-roam-db-map-links)
	     ;; (when (fboundp 'org-cite-insert)
	     ;;   (require 'oc)             ;ensure feature is loaded
	     ;;   (org-roam-db-map-citations
	     ;;    info
	     ;;    (list #'org-roam-db-insert-citation)))
	     )))))
    ;; For before-until advice
    t))

(defun md-roam-db-insert-file-node ()
  ;; Check the exension. Only when md, use custom logc.
  ;; `org-roam-db-update-file' turns the mode to org-mode (in `org-roam-with-file' macro)
  (markdown-mode)
  ;; run org-roam hooks to re-set after-save-hooks, etc.
  (run-hooks 'org-roam-find-file-hook)
  ;; This can be gfm-mode
  ;; Need to remember it somwhere
  (goto-char (point-min))
  (when-let ((id (md-roam-extract-id)))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (title (or (md-roam-extract-title)
                      (file-relative-name file org-roam-directory)))
           (pos (point))
	   (todo nil)
	   (priority nil)
           (scheduled nil)
           (deadline nil)
           (level 0)
           (aliases)
           (tags nil)
	   (properties nil)
           (olp nil))
      (org-roam-db-query!
       (lambda (err)
         (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                (error-message-string err)
                title id file))
       [:insert :into nodes
                :values $v1]
       (vector id file level pos todo priority
               scheduled deadline title properties olp))
      ;; For now, tags are always nil
      (when tags
        (org-roam-db-query
         [:insert :into tags
		  :values $v1]
         (mapcar (lambda (tag)
                   (vector id (substring-no-properties tag)))
                 tags)))
      (md-roam-db-insert-aliases)
      (md-roam-db-insert-refs))))

(defun md-roam-db-insert-aliases ()
  "Insert aliases for node at point into Org-roam cache."
  )

(defun md-roam-db-insert-refs ()
  "Insert refs for node at point into Org-roam cache."
  )

(defun md-roam-node-at-point (&optional _assert)
  "Return the node at point.
If ASSERT, throw an error."
  (when (and (buffer-file-name (buffer-base-buffer))
             (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer))))
    (org-roam-populate (org-roam-node-create :id (md-roam-extract-id)))))

(defun md-roam-id-get (&optional _pom _create _prefix)
  "Can implement CREATE later."
  (when (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer)))
    (md-roam-extract-id)))

(defun md-roam--markdown-file-p (path)
  "Check if PATH is pointing to an org file.
Return t or nil."
  (let ((ext (org-roam--file-name-extension path)))
    (string= ext md-roam-file-extension-single)))

(defun md-roam-db-map-links-file-link (_fns)
  "NOT USED?
Extract links in the form of [[link]]
wiki-link destination is assumed to be under the same directory as the source file.
The destination node needs to be already part of the database"
  (when (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer)))
    (save-excursion
      (let ((file (buffer-file-name (buffer-base-buffer)))
            (type "id")
            (source (md-roam-extract-id))
            (properties (list :outline nil)))
        (when source
          (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
            (when-let*
                ((to-file-name (concat (match-string-no-properties 1)
                                       "." md-roam-file-extension-single))
                 (to-file-path (expand-file-name to-file-name
                                                 (file-name-directory file)))
                 (dest (caar (org-roam-db-query
                              [:select [id] :from nodes :where (= file $s1)]
                              to-file-path))))
              (org-roam-db-query
               [:insert :into links :values $v1]
               (vector (point) source dest type properties)))))))
    t))

(defun md-roam-db-map-links ()
  "Extract links in the form of [[ID]]."
  (save-excursion
    (let ((file (buffer-file-name (buffer-base-buffer)))
          (type "id")
          (source (md-roam-extract-id))
          (properties (list :outline nil)))
      (when source
        (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
          (when-let*
              ((dest (match-string-no-properties 1)))
            (org-roam-db-query
             [:insert :into links :values $v1]
             (vector (point) source dest type properties))))))))			  

(defun md-roam-open-id-at-point ()
  "Open link, timestamp, footnote or tags at point.
The function tries to open ID-links with Org-roam’s database
before falling back to the default behaviour of
`org-open-at-point'. It also asks the user whether to parse
`org-id-files' when an ID is not found because it might be a slow
process.
This function hooks into `org-open-at-point' via
`org-open-at-point-functions'."
  (when (org-roam-file-p)
    (let* ((context (org-element-context))
           (type (org-element-property :type context))
           (id (org-element-property :path context)))
      (when (string= type "id")
        (find-file
         (caar (org-roam-db-query
                [:select [file] :from nodes :where (= id $s1)] id)))))))

(defun md-roam-find-id-in-file (id file &optional markerp)
  "Return the position of the entry ID in FILE.
If that files does not exist, or if it does not contain this ID,
return nil.
The position is returned as a cons cell (file-name . position).  With
optional argument MARKERP, return the position as a new marker."
  (when (and file
             (file-exists-p file)
             (md-roam--markdown-file-p file))
    (let ((pos 1)
          (buf (find-file-noselect file)))
      (with-current-buffer buf
        (when pos
          (if markerp
              (move-marker (make-marker) pos buf)
            (cons file pos)))))))

(defun md-roam-node-insert (&optional filter-fn)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions."
  (interactive)
  (when (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer)))
    (unwind-protect
        ;; Group functions together to avoid inconsistent state on quit
        (atomic-change-group
          (let* (region-text
                 beg end
                 (_ (when (region-active-p)
                      (setq beg (set-marker (make-marker) (region-beginning)))
                      (setq end (set-marker (make-marker) (region-end)))
                      (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
                 (node (org-roam-node-read region-text filter-fn))
                 (description (or region-text
                                  (org-roam-node-title node))))
            (if (org-roam-node-id node)
                (progn
                  (when region-text
                    (delete-region beg end)
                    (set-marker beg nil)
                    (set-marker end nil))
                  ;; Adapt for Markdown-mode Wikisyntax
                  (insert (concat "[[" (org-roam-node-id node) "]] " description)))
              (let ((org-roam-capture--info
                     `((title . ,(org-roam-node-title node))
                       (slug . ,(funcall org-roam-title-to-slug-function (org-roam-node-title node)))))
                    (org-roam-capture--context 'title))
                (setq org-roam-capture-additional-template-props
                      (list :region (when (and beg end)
                                      (cons beg end))
                            :insert-at (point-marker)
                            :link-description description
                            :finalize 'insert-link))
                (org-roam-capture--capture)))))
      (deactivate-mark))
    t))

(provide 'md-roam)
;;; md-roam.el ends here
