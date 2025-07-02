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
  "Create a catalog of transcoders from fsm xml files in directory `dir'"
  (cl-loop with froms = (sktl--make-cat)
	   finally (return froms)
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
		  (to (sktl--keywordify (cl-subseq name (1+ pos) (- len 4)))))
	     (sktl--add-to-cat froms from to pathname))))

(defvar $sktl-cat nil)

(defun sktl-get-transcoder-path (from to cat)
  (let ((to-spec (cdr (assoc from (cdr cat)))))
    (when to-spec
      (cadr (assoc to to-spec)))))

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
  (if (seq-empty-p key)
      (setf (car trie) val)		;TODO *warn-on-overwrite*
    (if (or (atom (seq-first key)) (cl-endp (cdr trie)))
	(let ((sub-trie (cdr (assoc (seq-first key) (cdr trie)))))
	  (unless sub-trie
	    (setq sub-trie (sktl--make-trie))
	    (push (cons (seq-first key) sub-trie) (cdr trie)))
	  (sktl--trie-set sub-trie (seq-rest key) val))
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
(cl-defstruct sktl-fsm state start-state state-table inputdecoding outputencoding from to)

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
    (make-sktl-fsm
     :start-state (sktl--keywordify (cdr (assoc 'start fsm1-attribs)))
     :inputdecoding (cdr (assoc 'inputDecoding fsm1-attribs))
     :outputencoding(cdr (assoc 'outputEncoding fsm1-attribs))
     :state-table ret)))

(cl-defun sktl--process-next (fsm string &key (start 0) (end (length string)))
  (unless (sktl-fsm-state fsm)
    (setf (sktl-fsm-state fsm) (sktl-fsm-start-state fsm)))
  (let ((trie (cdr (assoc (sktl-fsm-state fsm) (sktl-fsm-state-table fsm)))))
    (cl-assert trie nil "No trie for state %s." (sktl-fsm-state fsm))
    (cl-multiple-value-bind (retval pos)
	(sktl--find-seq string trie :start start :end end)
      (if retval
	  (cl-destructuring-bind (out next) retval
	    (when next (setf (sktl-fsm-state fsm) next))
	    (cl-values out pos))
	(cl-values (list (elt string start)) (1+ start))))))

(cl-defun sktl-process (fsm string &key (start 0) (end (length string)))
  (with-output-to-string
    (cl-loop initially (setf (sktl-fsm-state fsm) (sktl-fsm-start-state fsm))
	     with out
	     while (< start end) do
	     (cl-multiple-value-setq (out start)
	       (sktl--process-next fsm string :start start :end end))
	     (mapc 'write-char out))))

(defvar $sktl-fsm-cache (list nil))

(defun sktl-file-write-date (file)
  (file-attribute-modification-time (file-attributes file)))

(cl-defun sktl-cached-fsm (from to &key (cat $sktl-cat)
				(fsm-cache $sktl-fsm-cache)
				reload-if-newer reload)
  (let* ((key (cons from to))
	 (ent (cl-assoc key (cdr fsm-cache) :test #'equal)))
    (if (and ent (not reload))
	(cadr ent)
      (let* ((path (sktl-get-transcoder-path from to cat))
	     (tmp-xml (sktl--snarf-xml-file path))
	     (fsm (sktl--compile-entries tmp-xml)))
	(setf (sktl-fsm-from fsm) from)
	(setf (sktl-fsm-to fsm) to)
	(if ent
	    (setf (cadr ent) fsm)
	  (prog1 fsm
	    (setq ent (list key fsm))
	    (setf (cdr fsm-cache)
		  (push ent (cdr fsm-cache)))))))))

(cl-defun sktl-process-string (string from to &key
				      (cat $sktl-cat)
				      (fsm-cache $sktl-fsm-cache))
  "Uses `$sktl-fsm-cache' to cache FSMs. Ensure that `$skt-cat' is initialized
via `sktl-buildcat' before calling this function."
  (if (eql from to)
      string
    (if (sktl-get-transcoder-path from to cat)
	(sktl-process (sktl-cached-fsm from to :cat cat :fsm-cache fsm-cache)
		      string)
      (let ((ret (sktl-process (sktl-cached-fsm from :slp1
						:cat cat :fsm-cache fsm-cache)
			       string)))
	(sktl-process (sktl-cached-fsm :slp1 to
				       :cat cat :fsm-cache fsm-cache)
		      ret)))))

(cl-defun mwdict-convert (from to &optional beginning end)
  "Replace region between `beginning' and `end' which has text encoded in `from'
with corresponding text encoded in `to'.  Ensure `$skt-cat' is initialized via
`sktl-buildcat' before calling this function."
  (interactive
   (let ( list1 from1 )
   (list (setq from1 (intern-soft
		      (completing-read "From "
				       (setq list1
					     (mapcar #'car $sktl-cat))
				       nil t)))
	 (intern-soft (completing-read "To " (progn
;;					       (message "from1=%s" from1)
					       (remove from1 list1))
				       nil t))
	 (region-beginning)
	 (region-end))))
  (unless beginning (setq beginning (region-beginning)))
  (unless end (setq end (region-end)))
  (let ((replacement (sktl-process-string
		      (buffer-substring-no-properties beginning end)
		      from
		      to)))
    (progn
      (delete-region beginning end)
      (goto-char beginning)
      (insert replacement))))



;;; ----------------------------------------------------------------------
;;;
;;; additions to sktl-trie for the non-exceptional case
;;;

(defun sktl--trie-value (trie) (car trie))

(defun sktl--trie-sub-tries (trie) (mapcar 'cdr (cdr trie)))

(defun sktl--trie-descend (trie sub-key)
  (let ((cons (assoc sub-key (cdr trie))))
    (when cons (cdr cons))))

(defun sktl--gettrie (key trie &optional default)
  (seq-map (lambda (subkey)
	     (when trie
	       (setq trie (cdr (assoc subkey (cdr trie))))))
	   key)
  (if trie
      (car trie)
    default))

(gv-define-setter sktl--gettrie (value key trie &optional _default)
  `(sktl--trie-set ,trie ,key ,value))


;;; ----------------------------------------------------------------------
;;;
;;; segmenter
;;;

(defun sktl--single-match-failer (_tok _revtoks solutions)
  (and solutions t))

(defun sktl--strategy-linear (new-resumptions resumptions)
  (setq resumptions (nconc new-resumptions resumptions)) ;XXX
  (let ((last (last resumptions 1)))
    (and last (cons (car last) (cl-ldiff resumptions last)))))

;; FAIL (TOK REVTOKS SOLUTIONS) => T prunes branch
;; STRATEGY (NEW-RESUMPTIONS RESUMPTIONS) => combined resumptions

(cl-defun sktl--segment-string (string trie &key (start 0) (end (length string)) fail junk-allowed-p strategy)
  (cl-block nil
    (let ((resumptions (list (cons start nil))) p sub-trie output solutions
	  parsed-word new-resumptions)
      (cl-tagbody
       cont
	 (when new-resumptions
	   (setq resumptions
		 (if strategy
		     (funcall strategy new-resumptions resumptions)
		   (nreconc new-resumptions resumptions)))
	   (setq new-resumptions nil))
	 (cond ((cl-endp resumptions) (cl-return (nreverse solutions)))
	       (t (cl-destructuring-bind (start1 . output1) (pop resumptions)
		    (setq output output1 start start1))
		  (setq p start sub-trie trie)))
       loop
	 (cl-assert (< p end) nil "Start index %S > end index %S." start end)
	 (cond ((setq sub-trie (sktl--trie-descend sub-trie (aref string p)))
		(setq parsed-word (sktl--trie-value sub-trie))
		(when (not parsed-word)
		  (cond ((= (1+ p) end) ; fail at end-of-input
			 (go cont))
			(t (cl-incf p) (go loop))))
		(cl-assert parsed-word)
		(if (or (not fail)
			(not (funcall fail parsed-word output solutions)))
		    (cond ((= (1+ p) end)
			   (push (reverse (cons parsed-word output)) solutions)
			   (go cont))
			  ;; choice-point
			  ((sktl--trie-sub-tries sub-trie)
			   (push (cons (1+ p) (cons parsed-word output))
				 new-resumptions)
			   (cl-incf p) (go loop))
			  ;; next word
			  (t (push parsed-word output)
			     (setq sub-trie trie start (1+ p))
			     (cl-incf p) (go loop)))
		  (go cont)))
	       ;; fail at end-of-input
	       ((= (1+ p) end)
		(when junk-allowed-p
		  (push (reverse (cons (cl-subseq string start (1+ p)) output))
			solutions))
		(go cont))
	       ;; fail at end-of-trie
	       (t (when junk-allowed-p
		    (push (cons (1+ p)
				(cons (cl-subseq string start (1+ p)) output))
			  resumptions))
		  (go cont)))))))

(when nil
(defvar $test (sktl--make-trie))
(progn (setf (sktl--gettrie "abc" $test) 'abc)
       (setf (sktl--gettrie "a" $test) 'a)
       (setf (sktl--gettrie "b" $test) 'b)
       (setf (sktl--gettrie "ab" $test) 'ab))
(sktl--segment-string "aaabca" $test :junk-allowed-p nil)
(length (car (sktl--segment-string "aabcabcabaabc" $test :junk-allowed-p nil)))
(length (cadr (sktl--segment-string "aabcabcabaabc" $test)))
(sktl--segment-string "d" $test)
(sktl--segment-string "dadaabc" $test :junk-allowed-p t)
(sktl--segment-string "abab" $test
		:fail
		(lambda (tok revtoks solutions)
		  (cond (solutions
			 (warn "MATCH: %s sol=%s" (reverse (cons tok revtoks))
			       solutions)
			 nil)))))

