(defun gen_rows (row cols)
	(if (eq cols 0)
		nil
		(append (gen_rows row (- cols 1)) (list (list row cols)))
	)
)


(defun range (start end)
	(if (> start end)
		nil
		(cons start (range (+ start 1) end))
	)
)

;=============================
;testing function
;=============================
(defun test (name got expected)
        (if (equal expected got)
                t
                (progn (print '==error==) (print name) (print 'expected) (print expected) (print 'got) (print got))
        )
)
;=============================

;=============================
;tests
;=============================

(test 'genrows1 (gen_rows 1 2) '((1 1) (1 2)))
(test 'genrows2 (gen_rows 1 3) '((1 1) (1 2) (1 3)))
(test 'genrows3 (gen_rows 2 3) '((2 1) (2 2) (2 3)))

(test 'range1 (range 2 3) '(2 3))
(test 'range2 (range 0 9) '(0 1 2 3 4 5 6 7 8 9))
