;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sat Oct 16 19:03:43 2010 +0530 <enometh@meer.net>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2010-2021 Madhu.  All Rights Reserved.
;;;
;;; Transliterate between various sanskrit encodings using the data in
;;; transcoder files from the mw_offline programs, from the IITS
;;; Cologne Digital Sanskrit Dictionaries project:
;;; <URL:http://www.sanskrit-lexicon.uni-koeln.de/>


(defun sktl--keywordify (symbol)
  (when symbol
    (let ((name (downcase (if (symbolp symbol) (symbol-name symbol) symbol))))
      (intern (if (eq ?\: (aref name 0))
                  name
		(concat ":" name))))))

;;; ----------------------------------------------------------------------
;;;
;;; Q&D CATALOG:= (NIL . FROM-SPEC+)
;;; FROM-SPEC = (<from> . TO-SPEC+)
;;; TO-SPEC = (<to> pathname)
;;;
;;; <form>,<to> are keywords. <pathname> is a common lisp pathname to
;;; an xml file describing the FSM and is named of the form
;;; <from>_<to>.xml.

(defun sktl--make-cat () (list nil))

(defun sktl--add-to-cat (cat from to pathname)
  (cl-assert (consp cat))
  (let* ((from-cons (or (assoc from (cdr cat))
			(let ((ent (list from)))
			  (setf (cdr cat) (push ent (cdr cat)))
			  ent)))
	 (bogus-cons (assoc to (cdr from-cons))))
    (if (cl-endp bogus-cons)
	(push (list to pathname) (cdr from-cons))
      (let ((old-path (cadr bogus-cons)))
	(setf (cadr bogus-cons) pathname)
	(unless (cl-equalp old-path pathname)
	  (warn "cat: %s: path changed from %s to %s" (list from to)
		old-path pathname))
	bogus-cons))))

(defun sktl-buildcat (dir)
  (cl-loop with froms finally (return froms)
	   for pathname in (directory-files
			     (file-name-as-directory dir)
			     'full-path
			     ".*\\.xml")
	   for name = (file-name-nondirectory pathname)
	   for len = (length name)
	   for pos = (cl-position ?\_ name)
	   unless (and pos (> len 4) (not (cl-find ?\_ name :start (1+ pos)))
		       (string-equal ".xml" (cl-subseq name (- len 4))))
	   do (warn "Skipping %s." pathname)
	   else do
	   (let* ((from (sktl--keywordify (cl-subseq name 0 pos)))
		  (to (sktl--keywordify (cl-subseq name (1+ pos) (- len 4))))
		  (from-cons (or (assoc from froms)
				 (car (push (list from) froms))))
		  (bogus-cons (assoc to (cdr from-cons))))
	     (cl-assert (null bogus-cons))
	     (push (list to pathname) (cdr from-cons)))))

(defvar $sktl-cat nil)

(defun sktl-get-transcoder-specs (from to cat)
  (cl-destructuring-bind (from1 . to-spec)
      (assoc from cat)
    (cl-destructuring-bind (to1 path)
	(assoc to to-spec)
      path)))

(defun sktl--snarf-xml-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (libxml-parse-xml-region (point-min) (point-max))))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun sktl--parse-states (s)
  (split-string s "," t "\t\n\r "))

(cl-defun sktl--parse-string (string &key (start 0) (end (length string)))
  (when (< start end)
    (cond (;;"\\u0938\\u094d"
	   (and (< (+ start 5) end)
		(eql (elt string start) ?\\)
		(cl-equalp (elt string (+ start 1)) ?u))
	   (cons (identity ;code-char
		  (string-to-number (cl-subseq string (+ start 2) (+ start 6))
				    16))
		 (sktl--parse-string string :start (+ start 1 4 1) :end end)))
	  ;; S/^([^aAiIuUfFxXeEoO^/\\\\])
	  ((and (< (+ start 9) end)
		(eql (elt string start) ?\/))
	   (cl-assert (string= "/^([^" (cl-subseq string start (+ start 5))))
	   (cl-assert (string= "])" (cl-subseq string (- end 2) end)))
	   (list (cons :not (cl-loop for prev = nil then curr
				     for idx from (+ start 5) below (- end 2)
				     for curr = (elt string idx)
				     if (eql prev ?\\) collect curr
				     else unless (eql curr ?\\) collect curr))))
	  (t (cons (elt string start)
		   (sktl--parse-string string :start (1+ start) :end end))))))

(when nil
  (sktl--parse-string "\\u0938\\u094d")
  (sktl--parse-string "s\\u0938/^([^aAiIuUfFxXeEoO^/\\\\])"))


;;; ----------------------------------------------------------------------
;;;
;;; TRIE WITH NEGATION
;;;
;;; TRIE: (VAL . ALIST)         |  ALIST: (SUB-KEY . SUB-TRIE)+
;;; KEY: SUB-KEY+               |  SUB-KEY: ATOM | (:NOT ATOM+)
;;;
;;; TRIE-SET will merge `NOT'-ATOMS in the SUB-KEY of the LAST element
;;; of ALIST, and overwrite any values.  FIND-SEQ will DFS for
;;; explicit ATOMS before backtracking for `NOT'-ATOMS.

(defun sktl--make-trie () (cons nil nil))

(cl-defun sktl--trie-set (trie key &optional (val t))
  (if (null key)
      (setf (car trie) val)		;TODO *warn-on-overwrite*
    (if (or (atom (car key)) (cl-endp (cdr trie)))
	(let ((sub-trie (cdr (assoc (car key) (cdr trie)))))
	  (unless sub-trie
	    (setq sub-trie (sktl--make-trie))
	    (push (cons (car key) sub-trie) (cdr trie)))
	  (sktl--trie-set sub-trie (cdr key) val))
      (let ((last-cons (last (cdr trie))))
	(cl-assert last-cons)
	(cl-assert (eql (car (car key)) :not))
	(cl-destructuring-bind ((sub-key . sub-trie)) last-cons
	  (cond ((atom sub-key)
		 (setq sub-trie (sktl--make-trie))
		 (push (cons (car key) sub-trie) (cdr last-cons)))
		(t (cl-assert (eql (car sub-key) :not))
		   (dolist (x (cdr (car key))) ;merge
		     (cl-pushnew x (cdr sub-key)))))
	  (sktl--trie-set sub-trie (cdr key) val))))))

(cl-defun sktl--find-seq (seq trie &key (start 0) (end (length seq)))
  (when (< start end)
    (let (x retval retidx foundp)
      (when (setq x (assoc (elt seq start) (cdr trie)))
	(when (cdr x) ;; x = (sub-key . sub-trie)
	  (cl-multiple-value-setq (retval retidx foundp)
	    (sktl--find-seq seq (cdr x) :start (1+ start) :end end))))
      (cond (foundp (cl-values retval retidx foundp))
	    ((and x (car (cdr x)))
	     (cl-assert (not (consp (car x))))
	     (cl-values (car (cdr x)) (1+ start) t))
	    ((setq x (car (last (cdr (member x trie)))))
	     (cl-destructuring-bind (sub-key . sub-trie) x
	       (when (consp sub-key)
		 (cl-assert (eql (car sub-key) :not))
		 (unless (cl-find (elt seq start) (cdr sub-key))
		   (cl-values (car sub-trie) start t)))))))))


;;; ----------------------------------------------------------------------
;;;
;;; madhu 101017: just interpret the fsm for now; Generating code is
;;; no win.
;;;
(cl-defstruct fsm state start-state state-table inputdecoding outputencoding)

(cl-defun sktl--compile-entries (ents &aux ret)
  (cl-destructuring-bind (fsm1 fsm1-attribs . fsm1-rest)
      ents
    (cl-assert (eql fsm1 'fsm))
    (dolist (ent fsm1-rest)
      ;; (e nil (s nil "INIT") (in nil "f") (out nil "R"))
      (cl-destructuring-bind (e1 _e1-attribs . e1-rest) ent
	(if (eql e1 'e)
	    (progn
	      (cl-assert (eql e1 'e))
	      ;;(cl-assert (cl-endp _e1-attribs))
	      (let ((s (caddr (assoc 's e1-rest)))
		    (in (caddr (assoc 'in e1-rest)))
		    (out (caddr (assoc 'out e1-rest)))
		    (next (caddr (assoc 'next e1-rest))))
		(cl-dolist (state (sktl--parse-states s))
		  (let* ((state (sktl--keywordify state))
			 (in (sktl--parse-string in))
			 (out (sktl--parse-string out))
			 (next (sktl--keywordify next))
			 (trie (or (cdr (assoc state ret))
				   (let ((x (sktl--make-trie)))
				     (push (cons state x) ret)
				     x)))
			 (value (if (and out next)
				    (list out next)
				  (if out
				      (list out nil)
				    (if next
					(list nil next)
				      nil)))))
		    (cond (value (sktl--trie-set trie in value))
			  (t (warn "Skipping empty OUT/NEXT for IN %s." in)))))))
	  (warn "skipping entry %s" e1))))
    (make-fsm :start-state (sktl--keywordify (cdr (assoc 'start fsm1-attribs)))
	      :inputdecoding (cdr (assoc 'inputencoding fsm1-attribs))
	      :outputencoding(cdr (assoc 'outputencoding fsm1-attribs))
	      :state-table ret)))

(cl-defun sktl--decode-next (fsm string &key (start 0) (end (length string)))
  (unless (fsm-state fsm)
    (setf (fsm-state fsm) (fsm-start-state fsm)))
  (let ((trie (cdr (assoc (fsm-state fsm) (fsm-state-table fsm)))))
    (cl-assert trie nil "No trie for state %s." (fsm-state fsm))
    (cl-multiple-value-bind (retval pos)
	(sktl--find-seq string trie :start start :end end)
      (if retval
	  (cl-destructuring-bind (out next) retval
	    (when next (setf (fsm-state fsm) next))
	    (cl-values out pos))
	(cl-values (list (elt string start)) (1+ start))))))

(cl-defun sktl-decode (fsm string &key (start 0) (end (length string)))
  (with-output-to-string
    (cl-loop initially (setf (fsm-state fsm) (fsm-start-state fsm))
	     with out
	     while (< start end) do
	     (cl-multiple-value-setq (out start)
	       (sktl--decode-next fsm string :start start :end end))
	     (mapc 'write-char out))))
