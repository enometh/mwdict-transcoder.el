;;; -*- Mode: Emacs-Lisp; lexical-binding: t; coding: utf-8-unix; lisp-indent-function: common-lisp-indent-function -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Wed Aug 04 07:15:31 2021 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2021 Madhu.  All Rights Reserved.
;;;

(defvar sktl--slp-sktchar-table
  '( ;; vowels
    ("a" ?a)
    ("aa" ?A)
    ("i" ?i)
    ("ii" ?I)
    ("u" ?u)
    ("uu" ?U)
    (".r" ?f)
    (".r.r" ?F)
    (".l" ?x)
    (".l.l" ?X)
    ("e" ?e)
    ("ai" ?E)
    ("o" ?o)
    ("au" ?O)
    ;; consonants
    ("k" ?k)
    ("kh" ?K)
    ("g" ?g)
    ("gh" ?G)
    ("\"n" ?N)
    ("c" ?c)
    ("ch" ?C)
    ("j" ?j)
    ("jh" ?J)
    ("~n" ?Y)
    (".t" ?w)
    (".th" ?W)
    (".d" ?q)
    (".dh" ?Q)
    (".n" ?R)
    ("t" ?t)
    ("th" ?T)
    ("d" ?d)
    ("dh" ?D)
    ("n" ?n)
    ("p" ?p)
    ("ph" ?P)
    ("b" ?b)
    ("bh" ?B)
    ("m" ?m)
    ("y" ?y)
    ("r" ?r)
    ("l" ?l)
    ("v" ?v)
    ("\"s" ?S)
    (".s" ?z)
    ("s" ?s)
    ("h" ?h)
    ;; misc
    (".h" ?H)
    ;; double-dot -- standing for final s or r
    (".m" ?M)
    ;; dot above vowel. standing for final m or for any of the five
    ;; nasals followed by the first four mutes of its own class
    (".a"       ?')
    ;;    (".o" :om)
    ;; additions
    (" "  ?\ )))

(defmacro sktl--define-lookup-function-1 (name)
  `(cl-defun ,name (c &optional _d _e)
     (cl-case c
     ,@(cl-loop for (str slp1-code) in  sktl--slp-sktchar-table
	     collect `(,slp1-code ',(intern-soft (format "sktchar:|%s|" str))))
     (?\\ ,'(intern-soft (format "sktchar:|\\|")))
     (?\/ ,'(intern-soft (format "sktchar:|/|")))
     (?\^ ,'(intern-soft (format "sktchar:|^|")))
     (otherwise c))))

(sktl--define-lookup-function-1 sktl-lookup-slp1)


(cl-defun parse-slp1 (string)
  (mapcar 'sktl-lookup-slp1 string))

(when nil
(parse-slp1 "pu/nar e/hi vacas pate deve/na ma/nasA saha/"))
