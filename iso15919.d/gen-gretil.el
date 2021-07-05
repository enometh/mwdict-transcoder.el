;;; generate fsm xml files for Gretil's unicode roman encodings of
;;; iso-15911
(defvar $sktl-gretil-iso-15911-specs nil)

(setq $sktl-gretil-iso-15911-specs
  '(
("a" "a")
("A" "a")
("i" "i")
("I" "i")
("e" "e")
("E" "e")
("ai" "E")
("Ai" "E")
("o" "o")
("O" "o")
("au" "O")
("Au" "O")

;;long a
("ā" "A")
;;long A
("Ā" "A")
;;long i
("ī" "I")
;;"long I
("Ī" "I")
;;long u
("ū" "U")
;;long U
("Ū" "U")
;;vocalic r
("r̥" "f")
;;vocalic R
("R̥" "f")
;;long vocalic r
("r̥̄" "F")
;;vocalic l
("l̥" "x")
;;long vocalic l
("l̥̄" "X")


;;velar n
("ṅ" "N")
;;velar N
("Ṅ" "N")
;;palatal n
("ñ" "Y")
;;palatal N
("Ñ" "Y")
;;retroflex t
("ṭ" "w")
;;retroflex T
("Ṭ" "w")
;;retroflex d
("ḍ" "q")
;;retroflex d, intervoc. ??
("ḷ" "q")
;;retroflex D
("Ḍ" "q")
;;retroflex n
("ṇ" "R")
;;retroflex N
("Ṇ" "R")
;;palatal s
("ś" "S")
;;palatal S
("Ś" "S")
;;retroflex s
("ṣ" "z")
;;retroflex S
("Ṣ" "z")

;;anusvara
("ṃ" "M")
;;visarga
("ḥ" "H")

("K" "k")
("k" "k")
("kh" "K")
("Kh" "K")
("g" "g")
("G" "g")
("gh" "G")
("Gh" "G")
("c" "c")
("C" "c")
("ch" "C")
("Ch" "C")
("jh" "J")
("Jh" "J")
("th" "T")
("Th" "T")
("ṭh" "W")
("Ṭh" "W")
("d" "d")
("D" "d")
("dh" "D")
("Dh" "D")
("ḍh" "Q")
("Ḍh" "Q")
("n" "n")
("N" "n")
("p" "p")
("P" "p")
("ph" "P")
("Ph" "P")
("b" "b")
("B" "b")
("bh" "B")
("Bh" "B")
("m" "m")
("M" "m")
("y" "y")
("Y" "y")
("r" "r")
("R" "r")
("l" "l")
("L" "l")
("v" "v")
("V" "v")
("s" "s")
("S" "s")
("h" "h")
("H" "h")

;;a udatta
("á" "a/")
("Á" "a/")
;;a svarita
("à" "a\\")
("À" "a\\")
;;long a udatta
("ā́" "A/")
("Ā́" "A/")
;;long a svarita
("ā̀" "A\\")
("Ā̀" "A\\")
;;i udatta
("í" "i/")
("Í" "i/")
;;i svarita
("ì" "i\\")
("Ì" "i\\")
;;long i udatta
("ī́" "I/")
("Ī́" "I/")
;;long i svarita
("ī̀" "I\\")
("Ī̀" "I\\")
;;u udatta
("ú" "u/")
("Ú" "u/")
;;u svarita
("ù" "u\\")
("Ù" "u\\")
;;long u udatta
("ū́" "U/")
("Ū́" "U/")
;;long u svarita
("ū̀" "U\\")
("Ū̀" "U\\")
;; voc. r udatta
("ŕ̥" "f/")
("Ŕ̥" "f/")
;;voc. r svarita
("r̥̀" "f\\")
("R̥̀" "f\\")
;;long voc. r udatta
("r̥̄́" "F/")
("R̥̄́" "F/")
;;vocalic l udatta
("ĺ̥" "x/")
("Ĺ̥" "x/")
;;vocalic l svarita
("l̥̀" "x\\")
("L̥̀" "x\\")
;;long vocalic l udatta
("l̥̄́" "X/")
("L̥̄́" "X/")
;;long vocalic l svarita
("l̥̄̀" "X\\")
("L̥̄̀" "X\\")
;;e udatta
("é" "e/")
("É" "e/")
;;e svarita
("è" "e\\")
("È" "e\\")
;;o udatta
("ó" "o/")
("Ó" "o/")
;;o svarita
("ò" "o\\")
("Ò" "o\\")
;;accented ai
("aí" "E/")
("Aí" "E/")
("aì" "E\\")
("Aì" "E\\")
;;accented au
("aú" "O/")
("Aú" "O/")
("aù" "O\\")
("Aù" "O\\")
))

(when nil
(let ((start-state :init))
  (sktl-dump-entries
   (list
    (cons start-state
	  (cl-loop for (lhs rhs) in $sktl-gretil-iso-15911-specs
		   unless (equal lhs rhs)
		   collect (list
			    (cl-coerce lhs 'list)
			    (cl-coerce rhs 'list)))))
   start-state
   "iso15991_slp1.xml"))

(let ((start-state :init))
  (sktl-dump-entries
   (list
    (cons start-state
	  (cl-loop for (lhs rhs) in $sktl-gretil-iso-15911-specs
		   unless (or (equal lhs rhs)
			      (let ((initc (elt lhs 0)))
				(equal (upcase initc) initc)))
		   collect (list
			    (cl-coerce rhs 'list)
			    (cl-coerce lhs 'list)))))
    start-state
   "slp1_iso15991.xml"))
)

