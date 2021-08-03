;; -*- Mode: emacs-lisp; lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue Jul 18 10:31:25 2006 +0530 <enometh@net.meer>
;;;   Bugs-To: <enometh@net.meer>
;;;   Status: Experimental. Do Not Redistribute
;;;   Copyright (C) 2006-2021 Madhu. All Rights Reserved.
;;;


;;; ----------------------------------------------------------------------
;;;
;;; sktchar input encoding scheme from wikner's /sanskrit/
;;;

(defvar sktl--*sktchar-table*
  '( ;; vowels
    ("a" 	:simple-vowel :short :vowel)
    ("aa"	:simple-vowel :long :vowel)
    ("i"	:simple-vowel :short :vowel)
    ("ii"	:simple-vowel :long :vowel)
    ("u"	:simple-vowel :short :vowel)
    ("uu"	:simple-vowel :long :vowel)
    (".r"	:simple-vowel :short :vowel)
    (".r.r"	:simple-vowel :long :vowel)
    (".l"	:simple-vowel :short :vowel)
    (".l.l"	:simple-vowel :long :vowel)
    ("e"	:diphthong :vowel)
    ("ai"	:diphthong :vowel)
    ("o"	:diphthong :vowel)
    ("au"	:diphthong :vowel)
    ;; consonants
    ("k"	:guttural :hard :non-aspirate :mute :consonant)
    ("kh"	:guttural :hard :aspirate :mute :consonant)
    ("g"	:guttural :soft :non-aspirate :mute :consonant)
    ("gh"	:guttural :soft :aspirate :mute :consonant)
    ("\"n"	:guttural :soft :nasal :mute :consonant)
    ("c"	:palatal :hard :non-aspirate :mute :consonant)
    ("ch"	:palatal :hard :aspirate :mute :consonant)
    ("j"	:palatal :soft :non-aspirate :mute :consonant)
    ("jh" 	:palatal :soft :aspirate :mute :consonant)
    ("~n" 	:palatal :soft :nasal :mute :consonant)
    (".t" 	:cerebral :hard :non-aspirate :mute :consonant)
    (".th"	:cerebral :hard :aspirate :mute :consonant)
    (".d"	:cerebral :soft :non-aspirate :mute :consonant)
    (".dh" 	:cerebral :soft :aspirate :mute :consonant)
    (".n"	:cerebral :soft :nasal :mute :consonant)
    ("t"	:dental :hard :non-aspirate :mute :consonant)
    ("th"	:dental :hard :aspirate :mute :consonant)
    ("d"	:dental :soft :non-aspirate :mute :consonant)
    ("dh"	:dental :soft :aspirate :mute :consonant)
    ("n"	:dental :soft :nasal :mute :consonant)
    ("p"	:labial :hard :non-aspirate :mute :consonant)
    ("ph"	:labial :hard :aspirate :mute :consonant)
    ("b"	:labial :soft :non-aspirate :mute :consonant)
    ("bh"	:labial :soft :aspirate :mute :consonant)
    ("m"	:labial :soft :nasal :mute :consonant)
    ("y"	:palatal :soft :semi-vowel :sonorant :consonant)
    ("r"	:cerebral :soft :semi-vowel :sonorant :consonant)
    ("l"	:dental :soft :semi-vowel :sonorant :consonant)
    ("v"	:labial :soft :semi-vowel :sonorant :consonant)
    ("\"s"	:palatal :hard :sibilant :consonant)
    (".s"	:cerebral :hard :sibilant :consonant)
    ("s"	:dental :hard :sibilant :consonant)
    ("h"	:guttural :aspirate :semi-vowel :consonant)
    ;; misc
    (".h"	:visarga)
    ;; double-dot -- standing for final s or r
    (".m"	:anusvara)
    ;; dot above vowel. standing for final m or for any of the five
    ;; nasals followed by the first four mutes of its own class
    (".a"	:avagraha)
    (".o"	:om)
    ;; additions
    (" "	:space)))


;; obarray unused
(when nil
(defvar sktl--sktchar-obarray (make-vector 63 0))

(seq-map (lambda (x) (intern (car x) sktl--sktchar-obarray))
	  sktl--*sktchar-table*)

(defvar sktl--*sktchar-trie*
  (let ((trie (sktl--make-trie)))
    (cl-loop for (code . properties) in sktl--*sktchar-table*
	     for sym = (intern-soft code sktl--sktchar-obarray)
	     do (cl-assert sym)
	     (setf (sktl--gettrie code trie) sym)
	     (cl-loop for property in properties do
		      (setf (get sym property) t)))
    trie))
)

;; fake sktchar package
(defvar sktl--*sktchar-trie*
  (let ((trie (sktl--make-trie)))
    (cl-loop for (code . properties) in sktl--*sktchar-table*
	     for sym = (intern (format "sktchar:|%s|" code))
	     do
	     (setf (sktl--gettrie code trie) sym)
	     (cl-loop for property in properties do
		      (setf (get sym property) t)))
    trie))



;;; ----------------------------------------------------------------------
;;;
;;; parse-skt
;;;

(defvar $sktl--sktchar-debug-segmenter-failures nil)

(defun sktl--sktchar-single-match-failer (tok revtoks solutions)
  (cond (solutions
	 (when $sktl--sktchar-debug-segmenter-failures
	      (warn "SMF: Rejecting %S" (reverse (cons tok revtoks))))
	 t)))

(defun sktl--sktchar-all-purpose-failer (tok revtoks)
  (cond ((get tok :vowel)
	 (cond
	   ((and (get tok :short)
		 (eq tok (car revtoks)))
	    (when $sktl--sktchar-debug-segmenter-failures
	      (warn "APF: failing double %S." (list tok revtoks)))
	    t)))
	((get tok :anusvara)
	 (cond
	   ((not (get (car revtoks) :vowel))
	    (when $sktl--sktchar-debug-segmenter-failures
	      (warn "APF: failing anusvara %S without vowel."
		    (list tok revtoks))
	      t))))))

(defun sktl--sktchar-parse-skt (string)
  "Parse ascii STRING into a list of SKTCHAR symbols."
  (let ((solutions
	 (sktl--segment-string
	  string
	  sktl--*sktchar-trie*
	  :start 0 :end (length string)
	  :fail (lambda (tok revtoks solution)
		  (or (sktl--sktchar-all-purpose-failer tok revtoks)
		      (sktl--sktchar-single-match-failer
		       tok revtoks solution))))))
    (cl-values (car solutions) (cdr solutions))))


(defun parse-skt (string) (car (sktl--sktchar-parse-skt string)))

(when nil
(parse-skt "ka.n.thya")			; guttural
(parse-skt "taalavya") 			; palatal
(parse-skt "murdhnya")			; cerebral
(parse-skt "dantya")			; dental
(parse-skt "o.s.tya")			; labial
(parse-skt "a.mo.s.tya")
(parse-skt "lak.mos")
(parse-skt "va.h")
(get (intern-soft ".m" sktl--sktchar-obarray)
		  :anusvara)
(apply 'cl-concatenate 'string
       (mapcar (lambda (x) (symbol-name x))
	       (car (parse-skt "bhvaadi"))))
)
