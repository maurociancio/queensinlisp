;autor mauro ciancio

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

(defun gen_tablero (n &optional (desde 1))
	(gen_tablero_fila (range desde n) n)
)

(defun gen_tablero_fila (filas n)
	(if (null filas)
		nil
		(cons (gen_rows (car filas) n) (gen_tablero_fila (cdr filas) n))
	)
)

(defun armar_selected (tablero selectedpos)
	(cons (caar tablero) selectedpos)
)

;===========================================================================
;(x1 y1) (x2 y2)
;alineado
;dx * dy != 0
;(x2 - x1) * (y2 - y1) == 0
(defun alineado (nuevo viejo)
	(eq (* (- (car nuevo) (car viejo)) (- (cadr nuevo) (cadr viejo))) '0)
)

(defun alineados (nuevo viejos)
	(if (null viejos)
		nil
		(or 	(alineado nuevo (car viejos))
			(alineados nuevo (cdr viejos))
		)
	)
)

;abs(dx) == abs(dy)
;abs(x2 - x1) == abs (y2 - y1)
;abs(x2 - x1) - abs (y2 - y1) == 0
(defun diagonal (nuevo viejo)
	(eq (- (abs(- (car nuevo) (car viejo) ) ) (abs(- (cadr nuevo) (cadr viejo)) ) ) '0)
)

(defun diagonales (nuevo viejos)
	(if (null viejos)
		nil
		(or 	(diagonal nuevo (car viejos))
			(diagonales nuevo (cdr viejos))
		)
	)
)

;revisa que el primer elemento de selectedpos verifique la condicion
;de reinas. los restantes elementos deben verificarlo
(defun es_reinas_parcial (n selectedpos)
	(if (null selectedpos)
		t
		(and (not (alineados (car selectedpos) (cdr selectedpos)))
                     (not (diagonales   (car selectedpos) (cdr selectedpos)))
		)
	)
)
;===========================================================================

(defun do_reinas (n selectedpos tablero)
	(if (eq (length selectedpos) n)
		selectedpos
		(if (es_reinas_parcial n (armar_selected tablero selectedpos))
			(do_reinas n (armar_selected tablero selectedpos) (cdr tablero))
			(if (hay_mas_casilleros n tablero)
				(do_reinas n (armar_selected tablero selectedpos) (cadar tablero))
				(do_reinas n (cdr selectedpos) (gen_tablero n (- (length selectedpos) 1)))
			)
		)
	)
)

(defun reinas (n)
	(do_reinas n nil (gen_tablero n))
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

(test 'tablero1 (gen_tablero 1) '( ((1 1)) ))

(test 'tablero2 (gen_tablero 2) '( ((1 1)(1 2))
                                   ((2 1)(2 2)) ))

(test 'tablero3 (gen_tablero 3) '( ((1 1)(1 2)(1 3))
                                   ((2 1)(2 2)(2 3))
                                   ((3 1)(3 2)(3 3)) ))

(test 'tablero4 (gen_tablero 4) '( ((1 1)(1 2)(1 3)(1 4))
                                   ((2 1)(2 2)(2 3)(2 4))
                                   ((3 1)(3 2)(3 3)(3 4))
                                   ((4 1)(4 2)(4 3)(4 4)) ))

(test 'tablero5 (gen_tablero 4 1) '( ((1 1)(1 2)(1 3)(1 4))
                                     ((2 1)(2 2)(2 3)(2 4))
                                     ((3 1)(3 2)(3 3)(3 4))
                                     ((4 1)(4 2)(4 3)(4 4)) ))

(test 'tablero6 (gen_tablero 4 2) '( ((2 1)(2 2)(2 3)(2 4))
                                     ((3 1)(3 2)(3 3)(3 4))
                                     ((4 1)(4 2)(4 3)(4 4)) ))

(test 'do_reinas (do_reinas 2 '((1 1)(2 2)) nil) '((1 1)(2 2)))

(test 'es_reinas1 (es_reinas_parcial 1 '((1 1))) t)
(test 'es_reinas2 (es_reinas_parcial 1 nil) t)
(test 'es_reinas3 (es_reinas_parcial 2 '((1 1)(1 2))) nil)
(test 'es_reinas4 (es_reinas_parcial 3 '((1 1)(2 4))) t)
(test 'es_reinas5 (es_reinas_parcial 3 '((1 1)(2 3))) t)
(test 'es_reinas6 (es_reinas_parcial 3 '((1 1)(2 2))) nil)
(test 'es_reinas7 (es_reinas_parcial 3 '((2 1)(3 3)(1 4))) t)

(test 'alineados1 (alineados '(1 1) '((1 2))) t)
(test 'alineados2 (alineados '(1 1) nil) nil)
(test 'alineados3 (alineados '(1 1) '((1 3))) t)

(test 'diagonales1 (diagonales '(1 1) nil) nil)

(test 'alineado1 (alineado '(1 1) '(1 2)) t)
(test 'alineado2 (alineado '(1 1) '(2 2)) nil)
(test 'alineado3 (alineado '(1 1) '(3 3)) nil)
(test 'alineado4 (alineado '(1 1) '(3 1)) t)
(test 'alineado5 (alineado '(3 1) '(3 1)) t)
