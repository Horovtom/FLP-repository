(define (vloz-do-vlny name vlny hloubka)
  (if (null? vlny) (vloz-do-vlny name '(()) hloubka)
      (if (= hloubka 0)
          (cons (cons name (car vlny)) (cdr vlny))
          (cons (car vlny) (vloz-do-vlny name (cdr vlny) (- hloubka 1))))))

(define (zapis-vetve hloubka vlny strom)
  (if (null? strom) vlny
      (zapis-vetve hloubka (zapis-uzel (+ hloubka 1) vlny (car strom)) (cdr strom))))

(define (zapis-uzel hloubka vlny strom)
  (if (null? strom) vlny
      (zapis-vetve hloubka
                   (vloz-do-vlny (car strom) vlny hloubka)
                   (cdr strom))))

(define (tree->wave-list tree)
  (zapis-uzel 0 '() tree))