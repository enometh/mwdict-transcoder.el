;;; FSM XML MANIPULATING UTILS

(cl-defun sktl-extract-entries (fsm-xml &key raw-p)
  "Return edges as (<state> . ( <in> <out> [ <next> [ <attribs> ] ]  )+ )+"
  (let (ret)
    (cl-destructuring-bind (fsm1 fsm1-attribs . fsm1-rest)
	fsm-xml
      (cl-assert (eql fsm1 'fsm))
      (dolist (ent fsm1-rest)
	;; (e nil (s nil "INIT") (in nil "f") (out nil "R"))
	(cl-destructuring-bind (e1 e1-attribs . e1-rest) ent
	  (if (eql e1 'e)
	      (dolist (state (sktl--parse-states (caddr (assoc 's e1-rest))))
		(let* ((state (sktl--keywordify state))
		       (ent (or (assoc state ret)
				(car (push (list state) ret))))
		       (in-raw (sktl--parse-string
				(caddr (assoc 'in e1-rest))))
		       (in (if raw-p
			       in-raw
			     (apply #'string in-raw)))
		       (out-raw (sktl--parse-string
				 (caddr (assoc 'out e1-rest))))
		       (out (if raw-p
				out-raw
			      (apply #'string out-raw)))
		       (next (sktl--parse-string
			      (cdddr (assoc 'next e1-rest)))))
		  (push (append (list in out)
				(if e1-attribs (list next))
				e1-attribs)
			(cdr ent))))))))
    ret))

(defun sktl-unkeywordify (symbol)
  "Returns an UPCASE string without a leading colon"
  (when symbol
    (let ((name (upcase (if (symbolp symbol) (symbol-name symbol) symbol))))
      (if (eq ?\: (aref name 0))
	  (cl-subseq name 1)
	name))))

(defun sktl-dump-attribs (attribs)
  (apply #'concat (cl-loop for (key . val) in attribs
			   collect (format "%s='%s'" key val))))

(defun sktl-maybe-dump-unicode (seq)
  (cl-etypecase seq
    (character (sktl-maybe-dump-unicode (list seq)))
    (string (sktl-maybe-dump-unicode (cl-coerce seq 'list)))
    (sequence
     (with-output-to-string
      (seq-doseq (c seq)
	(if (characterp c)
	    (if (and (< c 128) (aref printable-chars c))
		(write-char c)
	      (princ (format "\\u%04x" c)))
	  (progn
	    (cl-assert (eql (car c) :not))
	    (princ (format "/^([^"))
	    (seq-doseq (c (cdr c))
	      (if (char-equal c ?\\) (write-char c))
	      (write-char c))
	      (princ (format "])")))))))))

(defun sktl-dump-entries (entries start-state path)
  (with-temp-buffer
    (insert (format "<fsm start='%s'>\n" (sktl-unkeywordify start-state)))
    (cl-loop for (state . rest) in entries
	     do
	     (cl-loop for (in out next . attribs) in rest
		      do
		      (insert "<e")
		      (when attribs
			(insert " " (sktl-dump-attribs attribs)))
		      (insert ">")
		      (insert
		       (format " <s>%s</s> <in>%s</in> <out>%s</out>"
			       (sktl-unkeywordify start-state)
			       (sktl-maybe-dump-unicode in)
			       (sktl-maybe-dump-unicode out)))
		      (if next
			  (insert (format " <next>%s</next>"
					  (sktl-unkeywordify next)))
			(insert " </e>\n"))))
    (insert "</fsm>\n")
    (write-region (point-min) (point-max) path)))

(when nil
(setq $from :slp1)
(setq $to :deva)
(setq $xml
      (sktl--snarf-xml-file (sktl-get-transcoder-path $from $to $sktl-cat)))
(setq $a (sktl-extract-entries $xml :raw-p t))
(sktl-dump-entries $a :init (format "/dev/shm/%s_%s.xml"
				   (downcase (sktl-unkeywordify $from))
				   (downcase (sktl-unkeywordify $to))
				   ))
(setq $s "S/^([^aAiIuUfFxXeEoO^/\\\\])")
(princ $s)
(pp (sktl-maybe-dump-unicode (sktl--parse-string $s)))
)
