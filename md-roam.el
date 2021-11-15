;;; md-roam.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 Noboru Ota
;;
;; Author: Noboru Ota <https://github.com/nobiot>
;; URL: https://github.com/nobiot/md-roam
;; Version: 2.0.0
;; Last Modified: 2021-11-09
;; Package-Requires: ((emacs "27.1") (org-roam "2.1.0") (markdown-mode "2.5"))
;; Keywords: markdown, zettelkasten, note-taking, writing, org, org-roam

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

;;  Use org-roam with markdown files by adding md-roam to it.  md-roam extends
;;  the features and functions provided by org-roam to support markdown files
;;  in addition to org

;;  Refer to README in the GitHub repo for instruction, configuraiton,
;;  and features supported in more detail

;;; Code:

;;;; Requirements

(eval-when-compile (require 'subr-x))
;; Markdown
(require 'markdown-mode)
;; Org
(require 'org)
(require 'ol)
(require 'org-macs)
;; Org-roam and its prequisites directly used by Md-roam
(require 'org-roam)
(require 'org-roam-db)
(require 'org-roam-utils)
(require 'emacsql)

;;;; Customization

(defgroup md-roam nil
  "Use markdown files in Org-roam."
  :group 'org-roam
  :prefix "md-roam-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/md-roam"))

(defcustom md-roam-file-extension "md"
  "Define the extesion to be used for Md-roam within Org-roam directory.
Unlike 'org-roam-file-extension', this is a single value, not a list.
It is intended to be used for you to define a different markdown extension,
such as .md and .markdown."
  :type 'string
  :group 'md-roam)

(defcustom md-roam-node-insert-type 'title-or-alias
  "Define whether ID or title/aliase should be inserted.
This is for `md-roam-node-insert'.  If 'title-and-alias, the
resultant wiki link will be \"[[title]]\.  If 'ID, it will be
\"[[ID]] title\"."
  :type '(choice (const :tag "Title or alias" title-or-alias)
		 (const :tag "Node ID" ID))
  :group 'md-roam)
  
;;;; Variables

(defvar md-roam-db-compatible-version 18
  "`Compatible 'org-roam-db-version'.
This is as described in \(info \"(org-roam\)Developer's Guide to Org-roam\"\).")

;;;; These regular expressions are modified version of
;;;; `markdown-regex-yaml-metadata-border.
;;;; I am adding "^" to indicate that the a line needs to
;;;; start with the delineator.

(defvar md-roam-regex-yaml-font-matter-beginning
  "\\(^-\\{3\\}\\)$"
  "Regexp for the beginning of YAML front matter section.
In `markdown-mode', beginning and ending are the same: \"---\".")

(defvar md-roam-regex-yaml-font-matter-ending
  "\\(^-\\{3\\}\\)$"
  "Regexp for the ending of YAML front matter section.
Separate regular expressions for beginning and ending are defined
here because in some markdown conventions, the ending is
delineated by \"```\".

This can be potentially supported setting a custom regexp for
the ending delineator.

If you change this to `\\(^`'\\{3\\}\\)$', you should be able to
support YAML matter ending with ```. I am not testing it, though.")

(defvar md-roam-regex-title
  "\\(^title:[ \t]*\\)\\(.*\\)")

(defvar md-roam-regex-id
  "\\(^id:[ \t]*\\)\\(.*\\)")

(defvar md-roam-regex-aliases
  ;; Assumed to be case insensitive
  "\\(^.*ROAM_ALIASES:[ \t]*\\)\\(.*\\)")

(defvar md-roam-regex-ref-keys
  ;; Assumed to be case insensitive
  "\\(^.*ROAM_REFS:[ \t]*\\)\\(.*\\)")

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
within a tag.  That is, iA Writer treats #tag-hyphen tagged as
#tag, and ignores `-hyphen'.

If iA Writer's stricter style is preferred, the regexp should
be defined as:

     `\\([^/s]\\)\\([#@][[:alnum:]_]+\\)'")

(defconst md-roam-regex-link-inline
  "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?4:\\]\\)\\(?5:(\\)\\(?6:[^)]*?\\)\\(?:\\s-+\\(?7:\"[^\"]*\"\\)\\)?\\(?8:)\\)"
  "Copy of `markdown-regex-link-inline' from Markdown Mode.
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

;;;; Commands

;;;###autoload
(define-minor-mode md-roam-mode
  "Md-roam mode needs to be turned before `org-roam-db-sync'.
It needs to be turned on before `org-roam-db-autosync-mode'."
  :init-value nil
  :lighter "md-roam"
  :global t
  (cond
   (md-roam-mode
    ;; Check org-roam-db-version
    ;; This is as described in (info "(org-roam)Developer's Guide to Org-roam")
    (if (not (eq org-roam-db-version md-roam-db-compatible-version))
        (progn
          (message (format "Md-roam not turned on; `org-roam-db-version' %d is not compatible" org-roam-db-version))
          (setq md-roam-mode nil))
      ;; Org-roam cache
      (advice-add #'org-roam-db-update-file :before-until #'md-roam-db-update-file)
      ;; Other interactive commands
      (advice-add #'org-roam-node-insert :before-until #'md-roam-node-insert)
      (advice-add #'markdown-follow-wiki-link :before-until #'md-roam-follow-wiki-link)
      ;; This avoids capture process to add ID in the Org property drawer
      (advice-add #'org-id-get :before-until #'md-roam-id-get)
      ;; `org-roam-mode' buffer
      (advice-add #'org-roam-node-at-point :before-until #'md-roam-node-at-point)
      ;; Completion-at-point
      ;; Append to the back of the functions list so that md-roam's one get called
      ;; before org-roam ones (org-roam dolist, resulting in reversing the order)
      (add-to-list 'org-roam-completion-functions
                   #'md-roam-complete-wiki-link-at-point 'append)))
   (t
    ;; Deactivate
    (advice-remove #'org-roam-db-update-file #'md-roam-db-update-file)
    (advice-remove #'org-roam-node-insert #'md-roam-node-insert)
    (advice-remove #'markdown-follow-wiki-link #'md-roam-follow-wiki-link)
    (advice-remove #'org-id-get #'md-roam-id-get)
    (advice-remove #'org-roam-node-at-point #'md-roam-node-at-point)
    (remove-hook 'org-roam-completion-functions #'md-roam-complete-wiki-link-at-point))))

;;;; Functions

;;; File utilities
;;; Macros seem to need to be defined before functions that use them
(defmacro md-roam-with-file (file keep-buf-p &rest body)
  "Execute BODY within FILE for Md-roam.
If FILE is nil, execute BODY in the current buffer.
Kills the buffer if KEEP-BUF-P is nil, and FILE is not yet visited."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (auto-mode-alist nil)
          (find-file-hook nil)
          (buf (or (and (not ,file)
                        (current-buffer)) ;If FILE is nil, use current buffer
                   (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                   (progn
                     (setq new-buf t)
                     (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (unless (equal major-mode 'markdown-mode)
         ;; Don't delay mode hooks
         ;; not done again.
         (markdown-mode))
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
         (when (find-buffer-visiting ,file)
           (kill-buffer (find-buffer-visiting ,file))))
     res))

;;;;; Private
;;    None of the functions in Md-roam are meant to be interactive commands.
;;    This is because they are meant to extend Org-roam functions (mostly in
;;    `org-roam-db') by means of advice.  This means that you as a user don't
;;    have to thinkg about the difference between Org-roam and Md-roam.  You can
;;    just use the same Org-roam commands such as `org-roam-node-insert' and
;;    `org-roam-node-find' within markdown files.  The switch of context is
;;    detected by the file extension -- e.g. .md vs .org -- and Md-roam will
;;    take care of the difference of the context.

;;------------------------------------------------------------------------------
;;;;; DB related functions for `org-roam-db'

(defun md-roam-db-update-file (&optional file-path _no-require)
  "Update Org-roam cache for FILE-PATH.
This function is meant to be used as advising function for
`org-roam-db-update-file.'"
  (when (md-roam--markdown-file-p (or file-path (buffer-file-name (buffer-base-buffer))))
    (md-roam-with-file file-path nil
        (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
        (let ((content-hash (org-roam-db--file-hash file-path))
              (db-hash (caar (org-roam-db-query [:select hash :from files
					                 :where (= file $s1)] file-path))))
          (unless (string= content-hash db-hash)
            (md-roam-db-do-update))
          ;; For before-until advice
          t))))

(defun md-roam-db-do-update ()
  "Update db cache without checking if the file has been changed.
Requied for wiki link capture."
  (emacsql-with-transaction (org-roam-db)
    (save-excursion
      (org-roam-db-clear-file)
      (org-roam-db-insert-file)
      (md-roam-db-insert-file-node)
      (md-roam-db-insert-wiki-links)
      (md-roam-db-insert-citations)
      (md-roam-db-insert-links))))

(defun md-roam-db-insert-file-node ()
  "Insert the file-level node into the Org-roam cache."
  ;; `org-roam-db-update-file' turns the mode to org-mode (in `org-roam-with-file' macro)
  ;; `markdown-mode' needs to be explicitly turned back on.
  ;; (markdown-mode)
  ;; Run org-roam hooks to re-set after-save-hooks, etc.
  (run-hooks 'org-roam-find-file-hook)
  ;; `org-with-point-at' macro does not seem to assume the buffer is Org
  (org-with-point-at 1
    (when-let ((id (md-roam-get-id)))
      (let* ((file (buffer-file-name (buffer-base-buffer)))
             (title (or (md-roam-get-title)
                        (file-relative-name file org-roam-directory)))
             (pos (point))
	     (todo nil)
	     (priority nil)
             (scheduled nil)
             (deadline nil)
             (level 0)
             (tags (md-roam-get-tags))
             ;; Properties are required for `org-roam-ui'
             ;; TODO other properties in frontmatter?
	     (properties (list (cons "TITLE" title) (cons "ID" id)))
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
        (when tags
          (org-roam-db-query
           [:insert :into tags
		    :values $v1]
           (mapcar (lambda (tag)
                     (vector id (substring-no-properties tag)))
                   tags)))
        (md-roam-db-insert-aliases)
        (md-roam-db-insert-refs)))))

(defun md-roam-db-insert-wiki-links ()
  "Insert Markdown wiki links in current buffer into Org-roam cache."
  (org-with-point-at (md-roam-get-yaml-front-matter-endpoint)
    (let ((type "id")
          (source (md-roam-get-id))
          (properties (list :outline nil)))
      (when source
        (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
          (let* ((name (match-string-no-properties 1))
                 (node (or (org-roam-node-from-title-or-alias name)
                           (org-roam-node-create :id name)))
                 (path (org-roam-node-id node)))
            ;; insert to cache the link only there is a file for the
            ;; destination node
            (when (org-roam-node-file (org-roam-populate node))
              (org-roam-db-query
               [:insert :into links
                        :values $v1]
               (vector (point) source path type properties)))))))))

(defun md-roam-db-insert-links ()
  "Insert URL links in current buffer into Org-roam cache.
This is for refs."
  (org-with-point-at (md-roam-get-yaml-front-matter-endpoint)
    (let  ((source (md-roam-get-id))
           (properties (list :outline nil)))
      (while (re-search-forward md-roam-regex-link-inline nil t)
        (when-let* ((url (match-string-no-properties 6))
                    (type (url-type (url-generic-parse-url url)))
                    (path (progn (string-match org-link-plain-re url)
	                         (match-string-no-properties 2 url))))
          (org-roam-db-query
           [:insert :into links
                    :values $v1]
           (vector (point) source path type properties)))))))

(defun md-roam-db-insert-citations ()
  "Insert data for citations in the current buffer into Org-roam cache.
The citation is defined in Pandoc syntax such as
\"[@citation-key]\"."
  (org-with-point-at (md-roam-get-yaml-front-matter-endpoint)
    (let ((source (md-roam-get-id))
          ;; TODO outline path always nil
          (properties (list :outline nil)))
      (when source
        (while (re-search-forward md-roam-regex-in-text-citation-2 nil t)
          (when-let
              ;; remove "@" for key
              ((key (match-string-no-properties 2)))
            (org-roam-db-query
             [:insert :into citations
                      :values $v1]
             (vector source key (match-beginning 2) properties))))))))

(defun md-roam-db-insert-aliases ()
  "Insert aliases in current buffer into Org-roam cache.
The aliases must be defined witin the frontmatter.

Aliases must be defined within square brakets:
    [\"alias1\", \"alias of this note\"]

TODO: Other formats?"
  (when-let* ((node-id (md-roam-get-id))
              (frontmatter (md-roam-get-yaml-front-matter))
              (aliases (and frontmatter
                            (string-match md-roam-regex-aliases frontmatter)
                            (md-roam--yaml-seq-to-list
                             (match-string-no-properties 2 frontmatter)))))
    (org-roam-db-query [:insert :into aliases
                                :values $v1]
                       (mapcar (lambda (alias)
                                 (vector node-id alias))
                               aliases))))

(defun md-roam-db-insert-refs ()
  "Insert refs in current buffer into Org-roam cache.
The refs must be defined witin the frontmatter.

TODO other formats?"
  (when-let* ((node-id (md-roam-get-id))
              (frontmatter (md-roam-get-yaml-front-matter))
              (refs (and frontmatter
                         (string-match md-roam-regex-ref-keys frontmatter)
                         (split-string-and-unquote
                          (match-string-no-properties 2 frontmatter)))))
    (let (rows)
      (dolist (ref refs)
        (save-match-data
          (cond ((string-match org-link-plain-re ref)
                 (push (vector node-id (match-string 2 ref) (match-string 1 ref)) rows))
                (t
                 (push (vector node-id ref "cite") rows)))))
      (when rows
        (org-roam-db-query [:insert :into refs
                            :values $v1]
                           rows)))))

;;------------------------------------------------------------------------------
;;;;; Functions for other commands: node-insert and follow-wiki-link

(cl-defun md-roam-node-insert (&optional filter-fn &key templates info)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'."
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
                                  (org-roam-node-formatted node))))
            (if (org-roam-node-id node)
                (progn
                  (when region-text
                    (delete-region beg end)
                    (set-marker beg nil)
                    (set-marker end nil))
                  (insert (concat "[["
                                  (cond
                                   ((eq md-roam-node-insert-type 'id)
                                    (concat (org-roam-node-id node) "]] " description))
                                   ((eq md-roam-node-insert-type 'title-or-alias)
                                    (concat (org-roam-node-title node) "]]")))))
                  ;; for advice
                  t)
              (org-roam-capture-
               :node node
               :info info
               :templates templates
               :props (append
                       (when (and beg end)
                         (list :region (cons beg end)))
                       (list :insert-at (point-marker)
                             :link-description description
                             :finalize 'insert-link)))
              ;; for advice
              t)))
      (deactivate-mark)
      ;; for advice
      t)))

(defun md-roam-follow-wiki-link (name &optional other)
  "Follow wiki link NAME if there is the linked file exists.
If the linked NAME does not yet exist, call `org-roam-find-node'
to capture a new file with using the text as the title.

It is meant to advice `markdown-follow-wiki-link'.

When OTHER is non-nil by using prefix argument, open the file in
another window.  This is not relevant if file does not exist."
  (when (org-roam-file-p (buffer-file-name (buffer-base-buffer)))
    (if-let* ((node (or (org-roam-node-from-title-or-alias name)
                        (org-roam-node-create :id name)))
              (node-populated (org-roam-populate node))
              (file (org-roam-node-file node-populated)))
        (when file
          (if other (find-file-other-window file) (find-file file)))
      (message (format "No Org-roam node found for \"%s\"" name))
      ;; TODO the source file needs to be saved again to update link for the
      ;; backlink to be cached.  This should be in the capture finaliztion
      ;; procss after the new buffer is saved (aborted should not be saved)
      (org-roam-capture-
       :node (org-roam-node-create :title name)
       :props '(:finalize md-find-file)))
    t))

;;;; Capture needs to update the cache when wikilink does not have a target file.
(defun md-find-file ()
  "."
  (let ((new-file (org-roam-capture--get :new-file)))
    (unless org-note-abort
      (when-let ((original-buf (org-capture-get :original-buffer)))
        (with-current-buffer original-buf
          (md-roam-db-do-update))
        (find-file new-file)))))

;;------------------------------------------------------------------------------
;;;;; Functions for `org-roam-capture'

(defun md-roam-id-get (&optional _pom _create _prefix)
  "This is meant to replace `org-id-get' for markdown buffers.
`org-roam-capture' process tries to create and add ID in the
Org's property drawer for a new file is being created.  For
markdown files, this should be prevented.  We can achieve this
because currently this function does not implement the create
process \(for _create argument\).

TODO CREATE process to insert a new ID within frontmatter."
  (when (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer)))
    (md-roam-get-id)))

;;------------------------------------------------------------------------------
;;;;; Functions for `org-roam-buffer'

(defun md-roam-node-at-point (&optional _assert)
  "Return the node at point.
If ASSERT, throw an error."
  (when (and (buffer-file-name (buffer-base-buffer))
             (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer))))
    (org-roam-populate (org-roam-node-create :id (md-roam-get-id)))))

;;------------------------------------------------------------------------------
;;;;; Get functions, mainly for properties in frontmatter

(defun md-roam-get-yaml-front-matter ()
  "Return the text of the YAML front matter of the current buffer.
Return nil if the front matter does not exist, or incorrectly delineated by
'---'.  The front matter is required to be at the beginning of the file."

  (save-excursion
    (goto-char (point-min))
    (when-let
        ((startpoint (re-search-forward
                      md-roam-regex-yaml-font-matter-beginning 4 t 1))
         ;; The beginning needs to be in the beginning of buffer
         (endpoint (re-search-forward
                    md-roam-regex-yaml-font-matter-ending nil t 1)))
      (buffer-substring-no-properties startpoint endpoint))))

(defun md-roam-get-yaml-front-matter-endpoint ()
  "Return the endpoint of the YAML front matter of the current buffer.
Return nil if the front matter does not exist, or incorrectly delineated by
'---'.  The front matter is required to be at the beginning of the file."

  (save-excursion
    (goto-char (point-min))
    (when-let
        ((startpoint (re-search-forward
                      md-roam-regex-yaml-font-matter-beginning 4 t 1))
         ;; The beginning needs to be in the beginning of buffer
         (endpoint (re-search-forward
                    md-roam-regex-yaml-font-matter-ending nil t 1)))
      endpoint)))

(defun md-roam-get-tags ()
  "Get tags defined in the Zettlr style within frontmatter."
  (let ((endpoint (md-roam-get-yaml-front-matter-endpoint)))
    (cond (endpoint
           (save-excursion
             (let (tags)
               (goto-char (point-min))
               (while (re-search-forward md-roam-regex-tags-zettlr-style endpoint t)
                 (let ((tag (match-string-no-properties 2)))
                   (when tag
                     (setq tags
                           ;; Remove the first char @ or #
                           (append tags (list (substring tag 1)))))))
               tags))))))

(defun md-roam-get-title ()
  "Extract title from the current buffer (markdown file with YAML frontmatter).

This function looks for the YAML frontmatter delineator '---' begining of
the buffer.  No space is allowed before or after the delineator.

It assumes:
 (1) Current buffer is a markdonw file (but does not check it)
 (2) It has title in the YAML frontmatter on top of the file
 (3) The format is 'title: The Document Title'"

  (let ((frontmatter (md-roam-get-yaml-front-matter)))
    (when (and frontmatter
               (string-match md-roam-regex-title frontmatter))
      (match-string-no-properties 2 frontmatter))))

(defun md-roam-get-id ()
  "Extract id from the current buffer (markdown file with YAML frontmatter).

This function looks for the YAML frontmatter delineator '---' begining of
the buffer.  No space is allowed before or after the delineator.

It assumes:
 (1) Current buffer is a markdonw file (but does not check it)
 (2) It has title in the YAML frontmatter on top of the file
 (3) The format is 'id: <string>'"

  (let ((frontmatter (md-roam-get-yaml-front-matter)))
    (when (and frontmatter
               (string-match md-roam-regex-id frontmatter))
      (match-string-no-properties 2 frontmatter))))

;;------------------------------------------------------------------------------
;;;;; Completion at point

(defun md-roam-complete-wiki-link-at-point ()
  "Complete wiki link at point to an existing Org-roam node.
It puts the title, not IDs."
  (when (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer)))
    (let (start end)
      (when (org-in-regexp org-roam-bracket-completion-re 1)
        (setq start (match-beginning 2)
              end (match-end 2))
        (list start end
              (org-roam--get-titles)
              :exit-function
              (lambda (&rest _)
                (forward-char 2)))))))

;;------------------------------------------------------------------------------
;;;;; Utility functions

(defun md-roam--markdown-file-p (path)
  "Return t if PATH is pointing to a markdown file.
`md-roam-file-extension' defines the extension.
Return nil if not."

  (let ((ext (org-roam--file-name-extension path)))
    (string-equal ext md-roam-file-extension)))

(defun md-roam--remove-single-quotes (str)
  "Check if STR is surrounded by single-quotes, and remove them.
If not, return STR as is."
  (let ((regexp "\\('\\)\\(.*\\)\\('\\)"))
    (if (string-match regexp str)
        (match-string-no-properties 2 str)
      str)))

(defun md-roam--yaml-seq-to-list (seq)
  "Return a list from YAML SEQ formatted in the flow style.
SEQ = sequence, It's an array.  At the moment, only the flow
style works.

See the spec at https://yaml.org/spec/1.2/spec.html Flow style:

    !!seq [ Clark Evans, Ingy döt Net, Oren Ben-Kiki ].

The items in the sequence (array) can be separated by different ways.

    1. Spaces like the example from the spec above
    2. Single-quotes 'item'
    3. Double-quotes \"item\"

Do not escape the singe- or double-quotations. At the moment,
that doeslead to error.

The regexp is meant to to match YAML sequence formatted in the
flow style.  At the moment, only the flow style is
considered. The number of spaces between the squeare bracket and
the first/last item should not matter.

    [item1, item2, item3]
    [ item1, item2, item3 ]

These should be equally valid."

  (let ((regexp "\\(\\[\s*\\)\\(.*\\)\\(\s*\\]\\)")
        (separator ",\s*"))
    (when (string-match regexp seq)
      (let ((items (split-string-and-unquote
                    (match-string-no-properties 2 seq) separator)))
        (mapcar #'md-roam--remove-single-quotes items)))))

(provide 'md-roam)
;;; md-roam.el ends here
