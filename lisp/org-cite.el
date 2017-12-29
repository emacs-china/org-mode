;;; org-cite.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Aaron Ecay <aaronecay@gmail.com>

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

;;; Code:

(require 'org-bibtex)
(require 'cl-lib)
(require 'subr-x)
(require 'let-alist)
;;; TODO: would also be nice to have a let-plist, and let-org-element
(require 'regexp-opt)
(require 'format-spec)

;;; TODO: calling with args backend and info is redundant, since
;;; backend can be gotten from info


;;; Utilities

(defmacro org-cite--plist-put-multiple (plist &rest rest)
  ;; TODO:
  ;; - tests etc.
  ;; - maybe make more general and move to org-macs
  (declare (indent 1))
  (if rest
      (progn
	(when (= (length rest) 1)
	  (error "Odd number of values"))
	(cl-destructuring-bind (key val . more) rest
	  `(org-cite--plist-put-multiple
	    (plist-put ,plist ,key ,val) ,@more)))
    plist))

;;; TODO: defcustom
(defconst org-cite--citeproc-java-lib-path
  (expand-file-name "../lib/"
		    (file-name-directory
		     (file-truename (executable-find "citeproc-java"))))
  "The path to the citeproc-java installation.")

(defconst org-cite--org-csl-dir
  (expand-file-name "../etc/csl/"
		    (file-name-directory (or load-file-name (buffer-file-name)))))

(defconst org-cite--citeproc-classpath
  (concat (mapconcat (lambda (x) (concat org-cite--citeproc-java-lib-path x))
		     '("citeproc-java-0.6.jar"
		       "commons-lang-2.6.jar"
		       "jbibtex-1.0.8.jar"
		       "antlr4-runtime-4.1.jar"
		       "rhino-1.7R4.jar"
		       "mapdb-0.9.6.jar"
		       "styles-1.0.1-SNAPSHOT.jar"
		       "locales-1.0.1-SNAPSHOT.jar"
		       "org.abego.treelayout.core-1.0.1.jar")
		     ":")
	  ":" org-cite--org-csl-dir))

(defun org-cite--run-citeproc-java (&rest args)
  "Run the javadoc program, passing it ARGS.

Raises an error if the program exits with a non-zero status,
otherwise returns stdout as a string.

We need to hack our own CSL file into the classpath, which is why
we can't just use the wrapper script distributed by citeproc-java."
  ;; TODO: does this work with windows paths?
  (with-temp-buffer
    (let ((result-code
	   (apply #'call-process "java" nil '(t nil) nil
		  "-classpath"
		  org-cite--citeproc-classpath
		  "de.undercouch.citeproc.CSLTool"
		  args)))
      (unless (zerop result-code)
	(error "Non-zero exit from citeproc-java; args = %S"
	       args))
      (buffer-string))))


;;; Citation modes

(defvar org-cite--citation-modes nil
  "An alist of mappings of citation modes to functions to format
a citation in that mode.  A citation mode is a manner of
formatting an in-text citation.  See:
<http://mid.gmane.org/m2k2z0mekp.fsf@tsdye.com>.

The formatting function is called with 4 arguments:
- the backend
- the info plist
- a plist of options for this citation containing:
  - :capitalized (TODO: unimplemented in the parser)
  - :parenthesized
  - :prefix
  - :suffix
- the citation database entry for this citation (cons key alist-of-vals)
- a function which can be called to retrieve the exported full
  citation of this key, using the current document's settings.
  This is useful for e.g. footnote styles.  It is passed as a
  function, rather than as a value, to avoid computing it when it
  is not needed.  (TODO: memoize the citation generation function
  to avoid needing this).")

(defun org-cite-add-citation-mode (mode fn)
  "Add a citation mode.

See `org-cite--citation-modes'."
  (setq org-cite--citation-modes
	(cons (cons mode fn)
	      org-cite--citation-modes)))

(defun org-cite-add-citation-mode-latex (mode nonparen paren)
  "A convenience function to add a citation mode for latex export only.

Will generate an error when used in other document types."
  ;; TODO: capitalized/non
  (org-cite-add-citation-mode mode
    (cl-function
     (lambda (backend _ (&key parenthesized prefix suffix) (cite-key . _) _)
       (if (org-export-derived-backend-p backend 'latex)
	   (format (if parenthesized paren nonparen)
		   prefix suffix cite-key)
	 (error "Citation mode `%s' is only defined for latex backends" mode))))))

(defconst org-cite--author-year-format
  "%p%a (%y%s)")

(defconst org-cite--author-year-parens-format
  "(%p%a %y%s)")

;;; TODO: add a convenience function to add a mode which is defined by
;;; latex commands and a set of template

(defun org-cite--mode-author-year (backend info cite-opts db-entry _full-cite-fn)
  (let ((parenthesized (plist-get cite-opts :parenthesized))
	(prefix (plist-get cite-opts :prefix))
	(suffix (plist-get cite-opts :suffix))
	(cite-key (car db-entry))
	(_cite-info (cdr db-entry)))
    (if (org-export-derived-backend-p backend 'latex)
	(format
	 (if parenthesized "\\parencite[%s][%s]{%s}" "\\textcite[%s][%s]{%s}")
	 ;; TODO: check on whether biblatex is going to automatically
	 ;; insert a semicolon after the year; I fear it is:
	 ;; \textcite[][foo]{bar} -> Bar (2015; foo)
	 prefix suffix cite-key)
      (format-spec
       (if parenthesized
	   org-cite--author-year-format
	 org-cite--author-year-parens-format)
       `((?a . ,(nth 1 (assoc cite-key (plist-get info :cite-author-years))))
	 (?y . ,(nth 2 (assoc cite-key (plist-get info :cite-author-years))))
	 (?p . ,prefix)
	 (?s . ,suffix)
	 ;; TODO: what else?
	 )))))

(org-cite-add-citation-mode "author-year" #'org-cite--mode-author-year)

;;; TODO: footnote -- will require fiddling with the info plist to get
;;; the proper definition/reference pair

;;; TODO: numbered citations (referring to numbers in bibliography,
;;; not footnotes) -- will require a counter mechanism in info
;;; additional to the footnote one.  A general counter might be
;;; desirable elsewhere (linguistic examples), so maybe it can be made
;;; generic.


;;; Citation styles

(defun org-cite--replace-in-string (orig rep str)
  (replace-regexp-in-string (regexp-quote orig) rep str
			    nil t))

(defmacro org-cite--replacements-in-string (str &rest reps)
  ;; TODO: indent declaration
  `(thread-last ,str
     ,@(mapcar (lambda (x)
		 `(org-cite--replace-in-string ,(nth 0 x) ,(nth 1 x)))
	       reps)))

(defun org-cite--html-to-org-inner (node)
  (cond
   ((null node) "")
   ((stringp node)
    ;; TODO: this should be a general fn like org-export-quote or
    ;; something.
    (org-trim (org-cite--replacements-in-string node
						;; Undo html quoting
						("&#38;" "&")
						("&#60;" "<")
						("&#62;" ">")
						;; TODO: superscripts

						;; Quote meaningful characters
						;; for org.
						("*" "\\ast{}")
						("_" "\\under{}")
						("/" "\\slash{}")
						("^" "\\asciicirc{}")
						("+" "\\plus{}")
						("~" "\\tilde{}")
						;; TODO: any more?
						)))
   ((listp node)
    (cl-destructuring-bind (tag attrs &rest children) node
      (let ((kids (mapconcat #'org-cite--html-to-org-inner children
			     ;; TODO: verify that this is the correct separator
			     "")))
	;; See file src/formatters.js in citeproc-js:
	;; https://bitbucket.org/fbennett/citeproc-js/src/default/src/formats.js
	(cl-case tag
	  ((i em)
	   ;; TODO: is there a difference between i and em?
	   (format "/%s/" kids))
	  (b
	   (format "*%s*" kids))
	  ;; TODO: what happens to this further along, if the parent
	  ;; document has turned off sub/superscripts?
	  (sub
	   (format "_{%s}" kids))
	  (sup
	   (format "^{%s}" kids))
	  (span
	   (let ((style (alist-get 'style attrs)))
	     (cond
	      ((null style)
	       (org-trim kids))
	      ((string= style "text-decoration:underline;")
	       (format "_%s_" kids))
	      ((member style '(
			       ;; TODO: these probably happen in the
			       ;; middle of e.g. underlining, to
			       ;; switch it off.  How can we implement
			       ;; this in org?  Maybe it's better to
			       ;; use asciidoc as the input format???
			       "font-style:normal;"
			       "font-variant:normal;"
			       "font-weight:normal;"
			       "text-decoration:none;"
			       ;; TODO: figure out what to do for
			       ;; these cases.  Small-caps can be
			       ;; hacked with upcase.
			       "font-variant:small-caps;"
			       "baseline"))
	       (org-trim kids)))))
	  (div
	   (if (equal "csl-entry" (alist-get 'class attrs))
	       (concat "- " kids "\n")
	     (org-trim kids)))
	  (otherwise
	   (org-trim kids))))))
   (t (error "wtf")
      ;; TODO: better error handling
      )))

(defun org-cite--html-to-org (html)
  (let ((parse (with-temp-buffer
		 (insert html)
		 (libxml-parse-html-region (point-min) (point-max)))))
    (org-cite--html-to-org-inner parse)))

(defun org-cite-format-bibliography (info)
  ;; TODO: sort the bibliography
  (let ((backend (plist-get info :back-end)))
    (if (org-export-derived-backend-p backend 'latex)
	;; TODO:
	;; - options for the bibliography command
	;; - support plain bibtex and/or natbib as well
	"\\printbibliography{}"
      ;; TODO:
      ;; - add section header to output
      ;; - figure out the in-buffer format for this (keyword vs. section
      ;;   with special property vs ...)
      ;; - less hacky way of generating the org syntax than inserting
      ;;   textually (ideally, generate the org syntax tree directly)
      ;; - formatting options for the bibliography (numbered
      ;;   vs. unnumbered list, ...)
      (with-temp-buffer
	(insert
	 (org-cite--html-to-org
	  (apply #'org-cite--run-citeproc-java
		 ;; TODO: make customizable
		 "-s" "chicago-author-date"
		 "-f" "html"
		 "-b" (plist-get info :cite-bibtex-file)
		 (plist-get info :cite-used-keys))))
	;; TODO: how expensive is this copy-sequence?  Maybe a plist
	;; extension to letf (or a manually-constructed equivalent)
	;; would be better.
	(let ((new-info (copy-sequence info)))
	  (plist-put new-info :cite-used-keys nil)
	  ;; TODO: do we need to strip other citation-related keywords
	  ;; as well?
	  (org-export-as backend nil nil t new-info))))))


;;; Lookup types

(defvar org-cite--lookup-types nil
  "Types of citation lookup backends.

Alist from type to list of:

- Function called at the beginning of export, with the rest of
  the keyword line after #+BIBDB: type, and the info plist.
  Should cache whatever it needs in the info plist.

- Function to lookup a citation.  Called with the key and the
  info plist.  Will be memoized by
  `org-cite--lookup' (TODO).  Should return an alist of
  keys and values about the citation (author, year, title, etc.)

- A boolean; non-nil = this lookup type is remote.  All local
  lookups will be tried before any remote one is.
  TODO: not yet implemented")

(defun org-cite-add-lookup-type (type prep-fn lookup-fn remotep)
  (declare (indent 1))
  (setq org-cite--lookup-types
	(cons (list type prep-fn lookup-fn remotep)
	      org-cite--lookup-types)))

(defun org-cite--org-bibtex-prep (path info)
  (plist-put info :cite-org-bibtex-files
	     (cons path (plist-get info :cite-org-bibtex-files))))

(defun org-cite--org-bibtex-lookup (key info)
  (let ((files (plist-get info :cite-org-bibtex-files)))
    (or (cl-dolist (file files)
	  (with-current-buffer (find-file-noselect file)
	    ;; TODO: close the buffer if it was newly opened by us
	    ;; TODO: more efficient way to do this?
	    (org-map-entries
	     (lambda ()
	       (cl-return (mapcar (lambda (x) (cons (intern (car x)) (cdr x)))
				  ;; TODO: push this into org-bibtex;
				  ;; diallow customizing the bibtex
				  ;; type property.
				  (cons (cons org-bibtex-type-property-name
					      (org-bibtex-get org-bibtex-type-property-name))
					(org-bibtex--all-properties)))))
	     (format "+%s=\"%s\"" org-bibtex-key-property key)
	     'file)))
	(error "Could not find key %s" key))))

(org-cite-add-lookup-type "org-bibtex"
  #'org-cite--org-bibtex-prep
  #'org-cite--org-bibtex-lookup
  nil)

;;; TODO: bibtex, DOI resolver via internet

(defun org-cite-lookup (key info)
  ;; TODO: memoize, document
  (cl-dolist (lookup-type (plist-get info :cite-lookup-types))
    (when-let ((result (funcall (nth 2 (assoc lookup-type org-cite--lookup-types))
				key info)))
      (cl-return result))))

;;; TODO: to support latex, we need to insert the results of the
;;; lookup (from remote sources in particular) into a temporary bib
;;; file that can be used during compilation.  Should we use
;;; \begin{filecontents} to insert into the latex document itself?

(defun org-cite--to-bibtex (cite-info)
  (let* ((key (car cite-info))
	 (info (cdr cite-info))
	 (type (alist-get 'btype info))
	 (info (remove (assq 'btype info) info)))
    (concat (format
	     ;; TODO: use the proper entry type
	     "@%s{%s,\n" type key)
	    (mapconcat (lambda (pair)
			 (format "%s = {%s}"
				 (symbol-name (car pair))
				 ;; TODO: Quote the value for bibtex?
				 ;; Since this is currently used to
				 ;; communicate with citeproc-java and
				 ;; (ideally) biber, maybe it should
				 ;; be pseudo-bibtex allowing UTF8
				 ;; etc.
				 (cdr pair)))
		       info
		       ",\n")
	    "}")))


;;; Integration with export functions

(defun org-cite--collect-citation (info citation)
  (let ((used-keys (plist-get info :cite-used-keys))
	(key (org-element-property :key citation)))
    (unless (member key used-keys)
      (plist-put info :cite-used-keys
		 (cons key used-keys)))))

(defun org-cite--make-bibtex (info)
  (if-let (used-keys (plist-get info :cite-used-keys))
      (let ((file (make-temp-file "org-cite")))
	(plist-put info :cite-bibtex-file file)
	(with-temp-file file
	  (insert
	   (mapconcat
	    (lambda (key)
	      (org-cite--to-bibtex (cons key (org-cite-lookup key info))))
	    used-keys
	    "\n\n"))))
    (message "No citations")
    nil))

(defun org-cite--get-author-year (info)
  (if-let ((keys (plist-get info :cite-used-keys)))
      (let* ((author-years
	      ;; TODO: does this reorder citations within the cite?  If
	      ;; so it will be necessary to call this one-by-one... :-/
	      (org-trim (apply #'org-cite--run-citeproc-java
			       "-b" (plist-get info :cite-bibtex-file)
			       "-s" "org-csl"
			       "-c"
			       keys))))
	(plist-put info :cite-author-years
		   (cl-mapcar (lambda (key ay)
				(let* ((s (split-string ay "////"))
				       (a (nth 0 s))
				       (y (nth 1 s)))
				  (list key a y)))
			      keys
			      (split-string author-years "||||"))))
    (message "No citations")
    nil))

(defun org-cite-export-prepare (tree info)
  "Build a citation database from the #+BIBDB keywords in TREE.

Store the information in INFO.  Returns a modified copy of INFO;
does not modify TREE."
  (let* ((lookup-types (mapcar #'car org-cite--lookup-types))
	 (lookup-types-re (regexp-opt lookup-types))
	 cite-mode cite-style
	 used-lookup-types)
    (org-element-map tree 'keyword
      (lambda (kw)
	(let ((kw-key (org-element-property :key kw))
	      (kw-val (org-element-property :value kw)))
	  (when (and (string= kw-key "BIBDB")
		     (string-match (rx-to-string `(and
						   string-start
						   (group (regexp ,lookup-types-re))
						   (? " " (group (* not-newline)))))
				   kw-val))
	    ;; TODO: Here we need \addbibresource support for Latex
	    (let ((lookup-type (match-string 1 kw-val)))
	      (setq info
		    (funcall (nth 1 (assoc lookup-type org-cite--lookup-types))
			     (match-string 2 kw-val)
			     info))
	      (add-to-list 'used-lookup-types lookup-type)))
	  (when (string= kw-key "CITATION_MODE")
	    ;; TODO: handle multiple specifications of CITATION_MODE
	    (setq cite-mode kw-val))
	  (when (string= kw-key "CITATION_STYLE")
	    ;; TODO: handle multiple specifications of CITATION_STYLE
	    (setq cite-style kw-val)))))
    ;; TODO: is it possible for this procedure to overgenerate?
    ;; Better might be to add the keys one by one in
    ;; `org-cite--do-export', but IDK how to ensure this is all done
    ;; before the bibliography is exported.
    (org-element-map tree 'citation
      (apply-partially #'org-cite--collect-citation info))
    ;; TODO: populate a temp file with all entries in bibtex format,
    ;; needed for the citation processor.  Provide an option to stuff
    ;; this into filecontents in latex.  Requires greedy lookup of
    ;; citations from remote resources.
    (org-cite--plist-put-multiple info
      :cite-function (cdr (assoc cite-mode org-cite--citation-modes))
      :cite-bibentry-function (cdr (assoc cite-style org-cite--citation-styles))
      :cite-lookup-types used-lookup-types)
    ;; Now that the lookup info is in the plist, we'll generate a temp
    ;; file with all the bib entries in it and stash the path to it in
    ;; the plist.  TODO: arrange to remove this file later.
    (org-cite--make-bibtex info)
    ;; The next step is to use our custom CSL file to extract the
    ;; correct author and year for each citation in the document.
    ;; TODO: this can be safely skipped if we're using a latex-derived
    ;; backend.
    (org-cite--get-author-year info)
    info))

(defun org-cite-format-citation (citation _contents info)
  "Export a citation object.

Export backends should call this function to get a general
citation text, and wrap its return value in any backend-specific
markup they wish."
  (let ((key (org-element-property :key citation)))
    (funcall (plist-get info :cite-function)
	     (plist-get info :back-end)
	     info
	     `(:capitalized
	       nil  ; TODO: pending parser support
	       :parenthesized ,(org-element-property :parentheticalp citation)
	       :prefix ,(org-export-data (org-element-property :prefix citation) info)
	       :suffix ,(org-export-data (org-element-property :suffix citation) info))
	     (cons key (org-cite-lookup key info))
	     (lambda () (org-cite-format-bibentry key info)))))


(provide 'org-cite)
;;; org-cite.el ends here
