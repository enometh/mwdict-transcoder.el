;;; -*- Mode: Emacs-Lisp; lexical-binding: t; coding: utf-8-unix; lisp-indent-function: common-lisp-indent-function -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Jul 21 12:25:55 2006 +0530 <enometh@net.meer>
;;;   Bugs-To: <enometh@net.meer>
;;;   Status: Experimental. Do Not Redistribute
;;;   Copyright (C) 2006-2021 Madhu. All Rights Reserved.
;;;

;;; ----------------------------------------------------------------------
;;;
;;; EXTERNAL SANDHI IMPLEMENTATION
;;;

(cl-defun match-external-visarga-sandhi-rule (final initial preceding-final)
  "Internal. 27 and 54."
  (cl-ecase final ((sktchar:|r| sktchar:|.h|)))
  ;;#+NIL(cl-assert (get preceding-final :vowel)) ; XXX
  ;; when final visarga: is preceeded by any vowel and followed by a
  ;; hard consonant: 15.2
  (cond ((and (get initial :consonant)		 (get initial :hard))
	 (cl-ecase initial
	   ;; remains unchanged before...
	   ((sktchar:|k| sktchar:|kh| sktchar:|p| sktchar:|ph|
		     sktchar:|\"s| sktchar:|.s| sktchar:|s|)
	    (if (eq final 'sktchar:|.h|) ; XXX pretty. don't return FINAL
		(cl-values '(sktchar:|.h| sktchar:|\ |))
	        (cl-values '(sktchar:|.h|))))
	   ;; NOTE final visarga was unchanged. when followed by a
	   ;; sibilant "s, .s or s, it is optionally changed to the
	   ;; sibilant. TODO
	   ((sktchar:|c| sktchar:|ch|)		  (cl-values 'sktchar:|\"s|))
	   ((sktchar:|.t| sktchar:|.th|)	  (cl-values 'sktchar:|.s|))
	   ((sktchar:|t| sktchar:|th|)		  (cl-values 'sktchar:|s|))))
	;; ========================================================-
	;; exception for final r: Visarga standing for final r even
	;; when preceeded by a or aa and followed by a vowel or soft
	;; consonant is changed to r. 54.
	;;
	;; When visarga is changed to r and is followed by r the first
	;; r is dropped and the preceding vowel, if short, is
	;; lengthened. 54.2
	;;
	((and (eq final 'sktchar:|r|)
	      (or (get initial :vowel)
		  (and (get initial :consonant)
		       (get initial :soft))))
	 (cond ((eq initial 'sktchar:|r|) ; 54.2 visarga changed to r
		(cl-values :zero nil
			(if (and ;;#+NIL(get preceding-final :vowel)
				 (get preceding-final :short))
			    (cl-ecase preceding-final
			      (sktchar:|a| 'sktchar:|aa|)
			      (sktchar:|i| 'sktchar:|ii|)
			      (sktchar:|u| 'sktchar:|uu|)))))
	       (t (cl-values 'sktchar:|r|))))
	;; -=========================================================
	;; When final visarga is preceeded by any vowel except a or
	;; aa, followed by vowel or soft consonant, is changed to
	;; r. 26.
	((and (not (cl-case preceding-final ((sktchar:|a| sktchar:|aa|) t)))
	      (or (and (get initial :consonant)
		       (get initial :soft))
		  (get initial :vowel)))
	 (cond ((eq initial 'sktchar:|r|) ; 54.2 visarga changed to r
		(cl-values :zero nil
			(if (and ;;#+NIL(get preceding-final :vowel)
				 (get preceding-final :short))
			    (cl-ecase preceding-final
			      (sktchar:|a| 'sktchar:|aa|)
			      (sktchar:|i| 'sktchar:|ii|)
			      (sktchar:|u| 'sktchar:|uu|)))))
	       (t (cl-values 'sktchar:|r|))))
	;; when preceeded by aa and followed by a soft consonant, it
	;; is dropped.
	((and (eq preceding-final 'sktchar:|aa|)
	      (or (and (get initial :consonant)
		       (get initial :soft))
		  (get initial :vowel)))
	 (cl-values :zero))
	;; when preceeded by a
	((eq preceding-final 'sktchar:|a|)
	 (cond ((and (get initial :consonant)
		     (get initial :soft))
		;; and followed by a soft consonant, a.h becomes o
		(cl-values 'sktchar:|o| nil :zero))
	       ;; and followed by any vowel except a, it is
	       ;; dropped. when followed by a, a.h becomes a and the
	       ;; following a is elided.
	       ((get initial :vowel)
		(if (eq initial 'sktchar:|a|)
		    (cl-values 'sktchar:|o| 'sktchar:|.a| :zero)
		    (cl-values :zero)))))))

;;; 31.
(cl-defun match-external-vowel-sandhi-rule (final initial _preceding-final)
  "Internal."
  (cl-assert (get final :vowel))
  (cl-assert (get initial :vowel))
  (cl-ecase final
    ((sktchar:|a| sktchar:|aa|)
     (cl-ecase initial
       ((sktchar:|a| sktchar:|aa|) (cl-values 'sktchar:|aa| :zero))
       ((sktchar:|i| sktchar:|ii|) (cl-values 'sktchar:|e| :zero))
       ((sktchar:|u| sktchar:|uu|) (cl-values 'sktchar:|o| :zero))
       ((sktchar:|.r|  sktchar:|.r.r|) (cl-values 'sktchar:|a| 'sktchar:|r|))
       (sktchar:|e| (cl-values 'sktchar:|ai| :zero))
       (sktchar:|o| (cl-values 'sktchar:|au| :zero))
       (sktchar:|ai| (cl-values 'sktchar:|ai| :zero))
       (sktchar:|au| (cl-values 'sktchar:|au| :zero))))
    ;; i and ii followed by a dissimilar vowel is changed to y.
    ((sktchar:|i| sktchar:|ii|)
     (cl-ecase initial
       ((sktchar:|a| sktchar:|aa| sktchar:|u| sktchar:|uu| sktchar:|e|
		 sktchar:|o| sktchar:|ai| sktchar:|au|
		 sktchar:|.r| sktchar:|.l|) ; XXX
	(cl-values 'sktchar:|y|))
       ((sktchar:|i| sktchar:|ii|) (cl-values 'sktchar:|ii| :zero))))
    ;; u and uu followed by a dissimilar vowel is changed to v.
    ((sktchar:|u| sktchar:|uu|)
     (cl-ecase initial
       ((sktchar:|u| sktchar:|uu|) (cl-values 'sktchar:|uu| :zero))
       ((sktchar:|a| sktchar:|aa| sktchar:|i| sktchar:|ii| sktchar:|e|
		 sktchar:|o| sktchar:|ai| sktchar:|au|
		 sktchar:|.r| sktchar:|.l|) ; XXX
	(cl-values 'sktchar:|v|))))
    ;; .r and .r.r followed by a dissimilar vowel is changed to r.
    ((sktchar:|.r| sktchar:|.r.r|)
     (cl-ecase initial
       ((sktchar:|.r|  sktchar:|.r.r|) (cl-values 'sktchar:|.r.r| :zero))
       ((sktchar:|a| sktchar:|aa| sktchar:|i| sktchar:|ii|
		 sktchar:|u| sktchar:|uu| sktchar:|e|
		 sktchar:|o| sktchar:|ai| sktchar:|au|
		 sktchar:|.r| sktchar:|.l|) ; XXX
	(cl-values 'sktchar:|r|))))
    ;; e and o followed by any vowel except a are changed to ay and
    ;; av. when followed by a, they remain unchanged while the a is
    ;; elided.
    ((sktchar:|e| sktchar:|o|)
     (cl-case initial
       (sktchar:|a| (cl-values final 'sktchar:|.a|))
       (otherwise (cl-ecase final		; XXX
		    (sktchar:|e|  (cl-values '(sktchar:|a| sktchar:|y|)))
		    (sktchar:|o|  (cl-values '(sktchar:|a| sktchar:|y|)))))))
    ;; before any vowel except a, ay and av may optionally drop the y
    ;; and v. TODO
    ;;
    ;; ai and au followed by any vowel are changed to aay and aav but
    ;; aay and aav may optionally drop the y and v. TODO
    ;;;
    (sktchar:|ai| (cl-values '(sktchar:|aa| sktchar:|y|)))
    (sktchar:|au| (cl-values '(sktchar:|aa| sktchar:|v|)))))

(cl-defun match-external-n-sandhi-rule (final initial preceding)
  "Internal."
  (cl-assert (eq final 'sktchar::|n|))
  ;; final n preceeded by a short vowel and followed by any
  ;; vowel is doubled. 87.1
  (cond ((and (and (get preceding :vowel)
		   (get preceding :short))
	      (get initial :vowel))
	(cl-values '(sktchar:|n| sktchar:|n|)))
	(t (cl-case initial 	;; 87.2:
	     ((sktchar:|c| sktchar:|ch|) (cl-values '(sktchar:|.m| sktchar:|\"s|)))
	     ((sktchar:|.t| sktchar:|.th|) (cl-values '(sktchar:|.m| sktchar:|.s|)))
	     ((sktchar:|t| sktchar:|th|) (cl-values '(sktchar:|.m| sktchar:|s|)))
	     ;; n followed by l is changed to nasalized ~m.l. 88.5
	     ;; TODO sktchar:|~m|
	     (sktchar:|l| (cl-values '(sktchar:|.n| sktchar:|l|)))))))

(cl-defun match-further-or-return (final initial preceding-final)
  "Internal."
  (cl-multiple-value-bind (replacement next prev)
      (match-external-sandhi-rule final initial preceding-final)
    (cond ((null replacement) (cl-assert (null next)) (cl-assert (null prev))
	   (cl-values final initial preceding-final))
	  (t (cl-values replacement next prev)))))

(cl-defun match-external-dpcl-sandhi-rule (final initial preceding-final)
  "Internal."
  ;; any dental coming in contact with a palatal is changed to the
  ;; corresponding palatal. 88.1
  (let (dental palatal cerebral)
    (cond ((or (and (get initial :dental) (get final :palatal)
		    (setq dental initial palatal final))
	       (and (get initial :palatal) (get final :dental)
		    (setq dental final palatal initial)))
	   (let ((corresponding-palatal
		  (cl-ecase dental
		    (sktchar:|t| 'sktchar:|c|)
		    (sktchar:|th| 'sktchar:|ch|)
		    (sktchar:|d| 'sktchar:|j|)
		    (sktchar:|dh| 'sktchar:|bh|)
		    (sktchar:|n| 'sktchar:|~n|)
		    (sktchar:|l| 'sktchar:|y|)
		    (sktchar:|s| 'sktchar:|\"s|))))
	     (match-further-or-return
		  (if (eq dental final) corresponding-palatal final)
		  (if (eq dental final) initial corresponding-palatal)
		  preceding-final)))
	  ;;
	  ;; initial "s preceeded by any of the first four letters of
	  ;; a class is optionally changed to ch. 88.2. TODO
	  ;;
	  ;; any dental coming in contact with a cerebral is changed
	  ;; to the corresponding cerebral. 88.3
	  ;;
	  ((and (or (and (get initial :dental) (get final :cerebral)
			 (setq dental initial cerebral final))
		    (and (get initial :cerebral) (get final :dental)
			 (setq dental final cerebral initial)))
		;; the preceding rule does not apply when a dental is
		;; followed by .s. 88.4
		(not (and (eq dental final) (eq initial 'sktchar:|.s|))))
	   (let ((corresponding-cerebral
		  (cl-ecase dental
		    (sktchar:|t| 'sktchar:|.t|)
		    (sktchar:|th| 'sktchar:|.th|)
		    (sktchar:|d| 'sktchar:|.d|)
		    (sktchar:|dh| 'sktchar:|.dh|)
		    (sktchar:|n| 'sktchar:|.n|)
		    (sktchar:|l| 'sktchar:|r|)
		    (sktchar:|s| 'sktchar:|.s|))))
	     (if (eq dental final)
		 (match-further-or-return corresponding-cerebral
					     initial preceding-final) ;XXX
		 (match-further-or-return final corresponding-cerebral
					     preceding-final))))
	  ;;
	  ;; any dental followed by a l is changed to l. 88.4
	  ;;
	  ((and (get final :dental) (eq initial 'sktchar:|l|))
	   (cl-values initial)))))

;;
(cl-defun match-external-consonant-sandhi-rule (final initial _preceding-final)
  "Internal."
  ;;(declare (ignore preceding-final))
  (cl-assert (get final :consonant))
  ;; a final hard consonant becomes soft before a vowel or a soft
  ;; consonant. 72.4.
  (cl-values
  (cond ((and (get final :hard)
	      (or (get initial :vowel)
		  (and (get initial :consonant)
		       (get initial :soft))))
	 (cl-ecase final
	   (sktchar:|k| 'sktchar:|g|)
	   (sktchar:|kh| 'sktchar:|gh|)
	   (sktchar:|c| 'sktchar:|j|)
	   (sktchar:|ch| 'sktchar:|jh|)
	   (sktchar:|.t| 'sktchar:|.d|)
	   (sktchar:|.th| 'sktchar:|.dh|)
	   (sktchar:|t| 'sktchar:|d|)
	   (sktchar:|th| 'sktchar:|dh|)
	   (sktchar:|p| 'sktchar:|b|)
	   (sktchar:|ph| 'sktchar:|bh|)))
	;; a final soft consonant becomes hard before a hard
	;; consonant. 72.6
	((and (get final :soft)
	      (get initial :hard))
	 (cl-case final ;; ;madhu 060722 cl-ecase does not handle .n
	   (sktchar:|g| 'sktchar:|k|)
	   (sktchar:|gh| 'sktchar:|kh|)
	   (sktchar:|j| 'sktchar:|c|)
	   (sktchar:|jh| 'sktchar:|ch|)
	   (sktchar:|.d| 'sktchar:|.t|)
	   (sktchar:|.dh| 'sktchar:|.th|)
	   (sktchar:|d| 'sktchar:|t|)
	   (sktchar:|dh| 'sktchar:|th|)
	   (sktchar:|b| 'sktchar:|p|)
	   (sktchar:|bh| 'sktchar:|ph|))))))

(cl-defun match-external-sandhi-rule (final initial preceding-final)
  "Internal. Returns :zero indicating no replacement, or the
replacement for FINAL. First additional value indicates a
replacement for INITIAL. Second additional value indicates a
replacement for PRECEDING-FINAL. Returns NIL if no known rule
matches."
  (cond
    ;; final m when followed by a consonant is changed to anusvara.
    ((and (eq final 'sktchar:|m|)
	  (get initial :consonant))
     (cl-values 'sktchar:|.m|))			; 15.1
    ((eq final 'sktchar:|n|)
     (match-external-n-sandhi-rule final initial preceding-final))
    ((or (eq final 'sktchar:|.h|)
	 (eq final 'sktchar:|r|))	; final visarga
     (match-external-visarga-sandhi-rule final initial preceding-final))
    ((and (get final :vowel) (get initial :vowel))
     (match-external-vowel-sandhi-rule
      final initial preceding-final))
    (t (cl-multiple-value-bind (repl next prev)
	   (match-external-dpcl-sandhi-rule final initial preceding-final)
	 (cond (repl (values repl next prev))
	       ((get final :consonant)
		(match-external-consonant-sandhi-rule
		 final initial preceding-final)))))))

(defun get-preceding-letter-and-final (final-word)
  (let (preceding final ret)
    (cl-maplist (lambda (list)
		  (cl-assert list)
		  (cond ((cl-endp (cdr list))
			 (setq preceding (pop ret) final (car list)))
			(t
			 (push (car list) ret))))
		final-word)
    (cl-values (nreverse ret) preceding final)))

(cl-defun append-applying-external-sandhi (final-word initial-word)
  (cl-multiple-value-bind (ret preceding-letter final)
      (get-preceding-letter-and-final final-word)
    (let ((initial (car initial-word)))
      (cl-multiple-value-bind (replacement next prev)
	  (match-external-sandhi-rule final initial preceding-letter)
	(cond ((null replacement)
	       (warn "Sandhi of %S + %S may have failed." final initial)
	       (cl-assert (null next)) (cl-assert (null prev))
	       (append ret (list preceding-letter final 'sktchar:|\ |)
		       initial-word))
	      (t (append ret
			 (cond ((null prev) (list preceding-letter))
			       ((eq prev :zero) nil)
			       ((consp prev) prev)
			       (t (list prev)))
			 (cond ((eq replacement :zero)
				(list 'sktchar:|\ |)) ;XXX
			       ((consp replacement) replacement)
			       (t (list replacement)))
			 (cond ((null next) initial-word)
			       ((eq next :zero) (cdr initial-word))
			       ((consp next) (append next (cdr initial-word)))
			       (t (cons next (cdr initial-word)))))))))))

(cl-defun test-sandhi (x y &rest rest)
  "Internal. Testing. Syntax:
  TEST-SANDHI STRING1 STRING2 [ STRING3 ... ] [ := RESULT ]"
  (cond ((cl-endp rest)
	 (let ((ret (append-applying-external-sandhi
		     (parse-skt x)
		     (parse-skt y))))
	   (apply 'cl-concatenate 'string (mapcar 'symbol-name (mapcar 'symbol-value ret)))))
	(t (let ((member (cl-member ':= rest :test #'eq)) result)
	     (when member
	       (setq result (cadr member))
	       (cl-assert (cl-endp (cddr member)))
	       (setq rest (nbutlast rest 2)))
	     (push y rest) (push x rest)
	     (let ((ret (cl-reduce (lambda (a b) (test-sandhi a b)) rest)))
	       (when result (cl-assert (string= ret result)))
	       ret)))))


(when nil
(append-applying-external-sandhi (parse-skt "muninaa")(parse-skt "ataami"))

(match-external-sandhi-rule 'sktchar:|.h| 'sktchar:|j| 'sktchar:|a|)
(match-external-sandhi-rule 'sktchar:|.h| 'sktchar:|s| 'sktchar:|a|)

;;; 31.1
(test-sandhi "muninaa" "ataami")
(test-sandhi "muninaa" "ataami" := "muninaataami")
(test-sandhi "namasi" "ii\"svaram"  := "namasii\"svaram")
(test-sandhi "kintu" "uvaca" := "kintuuvaca")
(test-sandhi "kart.r" ".rju.h" := "kart.r.rju.h")
;;; 31.2
(test-sandhi "namatha" "ii\"svaram" := "namathe\"svaram")
(test-sandhi "munina" ".r.si.h" :=  "muninar.si.h")
(test-sandhi "ti.s.thatha" "eva" := "ti.s.thathaiva")
(test-sandhi "khaadatha" "odanam" := "khaadathaudanam")
(test-sandhi "indra.h" "ca" "eraavata.h" "ca" := "indra\"scairaavata\"sca")
(test-sandhi "pa\"syatha" "audham" "pasyathaudham")
(test-sandhi "dhavati" "a\"sva.h" := "dhavatya\"sva.h")
(test-sandhi "nanu" "eva" := "nanveva")
;;; 31.3
(test-sandhi "kart.r" "iti" := "kartriti")
(test-sandhi "munave" "anna.m" "yacchaami" := "munave.anna.m yacchaami")
(test-sandhi "prabho" "adhunaa" := "prabho.adhunaa")

(trace match-external-sandhi-rule
       match-external-consonant-sandhi-rule
       match-external-dpcl-sandhi-rule
       match-external-vowel-sandhi-rule
       match-external-visarga-sandhi-rule
       match-external-n-sandhi-rule)
(untrace)
(test-sandhi "kave" "icchasi" )		; XXX
(test-sandhi "gure" "iti" )		; XXX
;;;
;;; 72.4
(test-sandhi "n.rpaat" "alabhat" := "n.rpaadalabhat")
(test-sandhi "gramaat" "gacchaami" := "gramaadgacchaami")
;;;  72.6
(test-sandhi "suh.rd" "su" := "suh.rtsu")
(test-sandhi "etad" "patati" := "etatpatati")
;;; 87.1
(test-sandhi "prahasan" "aagacchati")
(test-sandhi "balin" "ajaya.h" := "balinnajaya.h")
;;; 87.2
(test-sandhi "taan" "ca" := "taa.m\"sca")
(test-sandhi "dhiimaan" ".tiikaa.m" "pa.thati" :=
	     "dhiimaa.m.s.tiikaa.m pa.thati")
(test-sandhi "ariin" "taa.dayati")
;;; 88.1
(test-sandhi "suh.rt" "calati"  := "suh.rccalati")
(test-sandhi "aanayat" "jalam" := "aanayajjalam")
(test-sandhi "tat" "\"srutvaa")		; XXX
;;; 88.3
(test-sandhi "apibat" ".ta.nkam" := "apiba.t.ta.nkam")
(test-sandhi "pu.s" "ta" := "pu.s.ta")
(test-sandhi "abhak.sayat" ".sa.davam")	; fail
(test-sandhi "etad" "labhate")
(test-sandhi "v.rk.saan" "lumpati")

;;; 15
(test-sandhi "n.rpa.h" "jayati")
(test-sandhi "raamam" "viiram" "bodhaama.h" := "raama.mviira.mbodhaama.h")
(test-sandhi "putra.h" "khanati" := "putra.h khanati")
(test-sandhi "janaa.h" "patanti" := "janaa.h patanti")
(test-sandhi "baala.h" "sarati" := "baala.h sarati")
(test-sandhi "janaa.h" "calanti" := "janaa\"scalanti")
(test-sandhi "pa.thata.h" ".tiikaam" := "pa.thata.s.tiikaam")
(test-sandhi "putra.h" "tarati" := "putrastarati")
(test-sandhi "baalaa.h" "dhaavanti" := "baalaa dhaavanti")
(test-sandhi "janaa.h" "a.tanti" := "janaa a.tanti")
(test-sandhi "putra.h" "dhaavati" := "putrodhaavati")
(test-sandhi "dhaavata.h" "aakulau" := "dhaavata aakulau")
(test-sandhi "dhaavata.h" "a\"svau" := "dhaavato.a\"svau")

;; 54
(test-sandhi "puna.h" "api") ; XXX punarapi
(test-sandhi "punar" "api")
(test-sandhi "pita.h" "vadasi");; XXX pitarvadasi
(test-sandhi "pitar" "vadasi")
(test-sandhi "mata.h" "indu.m" "pa\"syasi") ;; XXX matarindu.m pa"syasi
)



;;; ----------------------------------------------------------------------
;;;
;;;

(cl-defun make-u-v-w-sandhi-rule (final initial preceding-final)
  (cl-flet ((ensure-list (x) (cl-etypecase x
			    (keyword (cl-ecase x (:zero nil)))
			    (symbol (list x))
			    (cons x))))
    (cl-multiple-value-bind (replacement next prev)
	(match-external-sandhi-rule final initial preceding-final)
      (cond ((null replacement) (cl-assert (null next)) (cl-assert (null prev)) (values nil))
	    (t (cond ((and (null prev) (null next))
		      (values t (list final) ; u
			      (list initial) ; v
			      (append (ensure-list replacement)
				      (list initial))))	; w
		     ((or (null prev) (eq preceding-final  prev)) ; XXX
		      (values t (list final)		      ; u
			      (list initial)		      ; v
			      (append (ensure-list replacement)	; and w
				      (if next
					  (ensure-list next)
					  (list initial)))))
		     (t (values t (list preceding-final final)	; u
				(list initial)			; v
				(append (ensure-list prev) ; and w
					(ensure-list replacement)
					(if next
					    (ensure-list next)
					    (list initial)))))))))))

(when nil
(make-u-v-w-sandhi-rule 'sktchar:|i| 'sktchar:|o| 'sktchar:|m|)
(make-u-v-w-sandhi-rule 'sktchar:|.h| 'sktchar:|k| 'sktchar:|a|)
(make-u-v-w-sandhi-rule 'sktchar:|.h| 'sktchar:|j| 'sktchar:|a|)
)

(cl-defun collect-sandhi-rules (word initial-letters-list)
  (cl-loop for x on word
	for (preceding-final final .  rest) on x
	if (cl-endp rest) return
	(let (ret)
	  (map nil
	       (lambda (initial)
		 (cl-multiple-value-bind (foundp u v w)
		     (make-u-v-w-sandhi-rule final initial preceding-final)
		   (when foundp (cl-pushnew (list u v w) ret :test #'cl-equalp))))
	       initial-letters-list)
	  ret)))

(when nil
(collect-sandhi-rules (skt-tagger::parse-skt "namaami") *initial-letters*)
(collect-sandhi-rules (skt-tagger::parse-skt "n.rpaya.h") *initial-letters*)
)
