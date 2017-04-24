;(simulate <state> <expr> <program> <limit>)
;<state> is a state of the robot of the form: (<maze> (<coordinate-x> <coordinate-y>) <orientation>)
;<expr> can content form, command, or procedure call.
;<program> can content list of procedure definitions.
;<limit> is a number which means maximum nested procedure calls. If a new procedure call exceeds the limit then this call cannot be done.

(define (is-less? elem other)
  (cond
    ((null? elem) #f)
    ((< (car elem) (car other)) #t)
    ((> (car elem) (car other)) #f)
    (else (is-less? (cdr elem) (cdr other)))))

(define (get-nth ls n)
  (if (not (list? ls)) (begin (display ls) (newline) (display n) (newline) (car ls))
      (if (<= n 0) (car ls)
          (get-nth (cdr ls) (- n 1)))))

(define (simulate state expr program limit)
  (define (wall? current-state) (equal? 'w (get-val-at-coords (car current-state) (get-coords-at-direction (cadr current-state) (caddr current-state)))))
  
  (define (do-turn-left current-state)
    (cond
      ((equal? (caddr current-state) 'north) (list (car current-state) (cadr current-state) 'west))
      ((equal? (caddr current-state) 'west) (list (car current-state) (cadr current-state) 'south))
      ((equal? (caddr current-state) 'south) (list (car current-state) (cadr current-state) 'east))
      ((equal? (caddr current-state)  'east) (list (car current-state) (cadr current-state) 'north))
      )
    )
  
  (define (mark? current-state)
    (< 0 (get-marks current-state))
    )
  
  (define (passable? maze location)
    (not (equal? (get-val-at-coords maze location) 'w))
    )
  
  (define (get-val-at-coords ls_map ls_current_coords )
    (let (
          (curr_x (car ls_current_coords))
          (curr_y (cadr ls_current_coords))
          )
      (if (= curr_y 0)
          (if (list? (car ls_map))
              ;Need to extract the row-list
              (get-val-at-coords (car ls_map) (list curr_x 0))
              
              ;I have only the row-list by now
              (if (= curr_x 0) 
                  (car ls_map)
                  (get-val-at-coords (cdr ls_map) (list (- curr_x 1) 0))
                  )
              )
          
          ;Do not have the row-list yet! Gotta go deeper
          ;The next is the row-list
          (if (= curr_y 1)
              (get-val-at-coords (cadr ls_map) (list curr_x 0))
              ;Gotta go deeeeeeper
              (get-val-at-coords (cdr ls_map) (list curr_x (- curr_y 1)))
              )
          )
      )
    )
  
  
  (define (get-coords-at-direction curr_coords direction)
    (cond
      ((null? curr_coords) '())
      ((null? direction) '())
      (else
       (let (
             (curr_x (car curr_coords))
             (curr_y (cadr curr_coords))
             )
         (cond
           ((equal? direction 'north) (list curr_x (- curr_y 1)))
           ((equal? direction 'south) (list curr_x (+ curr_y 1)))
           ((equal? direction 'west) (list (- curr_x 1) curr_y))
           ((equal? direction 'east) (list (+ curr_x 1) curr_y))
           (else (reporterr (list "I do not know this direction! " direction)))
           )
         )
       )
      )
    )
  
  ;Output - (new-executed-commands new-state)
  (define (custom-keyword  keyword  current-state  executed-commands  robot-program  depth)
    (let (
          (error-output (list executed-commands current-state '1))
          )
      
      (cond
        ((> depth limit) error-output)
        ((null? robot-program) error-output)
        ((not (list? robot-program)) error-output)
        ((equal? (cadar robot-program) keyword)
         (if (equal? (caar robot-program) 'procedure)
             (parse-expressions (caddar robot-program) current-state executed-commands depth) 
             error-output
             )
         )
        (else (custom-keyword keyword current-state executed-commands (cdr robot-program) depth))
        )
      )
    )
  
  (define (do-step state)
    (if (null? state) state
        (let* (
               (maze (car state))
               (old-location (cadr state))
               (robot-x (car old-location))
               (robot-y (cadr old-location))
               (orientation (caddr state))
               (new_location (get-coords-at-direction old-location orientation))
               )
          (if (passable? maze new_location) 
              (list maze new_location orientation)
              state
              )
          )
        )
    )
  
  ;Pass in #t or #f to add or remove mark from location respectively
  (define (edit-mark state add)
    (define (edit_mark_on_location maze location)
      (let (
            (curr_x (car location))
            (curr_y (cadr location))
            )
        
        (if (= curr_y 0)
            (if (list? (car maze))
                (cons (edit_mark_on_location (car maze) (list curr_x 0)) (cdr maze))
                (if (= curr_x 0)
                    (if add
                        (cons (+ (car maze) 1) (cdr maze))
                        (if (> (car maze) 0)
                            (cons (- (car maze) 1) (cdr maze))
                            )
                        )
                    (cons (car maze) (edit_mark_on_location (cdr maze) (list (- curr_x 1) 0)))
                    )
                )
            (if (= curr_y 1)
                (if (null? (cddr maze))
                    (list (car maze) (edit_mark_on_location (cadr maze) (list curr_x 0)))
                    (cons (car maze) (cons (edit_mark_on_location (cadr maze) (list curr_x 0)) (cddr maze)))
                    )
                ;Gotta go deeeeeeper
                (cons (car maze) (edit_mark_on_location (cdr maze) (list curr_x (- curr_y 1))))
                )
            )
        )
      )
    (list (edit_mark_on_location (car state) (cadr state)) (cadr state) (caddr state))
    )
  
  (define (get-marks current-state) (get-val-at-robot current-state))
  
  (define (get-val-at-robot current-state)
    (get-val-at-coords (car current-state) (cadr current-state))
    )
  
  (define (my-append ls1 ls2)
    (cond
      ((and (list? ls1) (list? ls2)) (append ls1 ls2))
      ((list? ls1) (append ls1 (list ls2)))
      ((list? ls2) (append (list ls1) ls2))
      (else
       (append (list ls1) (list ls2))))
    )
  
  (define (north? current-state) (equal? (caddr current-state) 'north))
  
  (define (parse-expressions expression-list current-state executed-commands depth)
    (let (
          (error-output (list executed-commands current-state '1))
          )
      (cond
        ((> depth limit) error-output)
        ((null? expression-list) (list executed-commands current-state '0))
        ((not (list? expression-list)) (parse-expressions (list expression-list) current-state executed-commands depth))
        ((equal? (car expression-list) 'step)
         (if (wall? current-state)
             ;It crashed:
             error-output
             ;Do-step
             (parse-expressions (cdr expression-list) (do-step current-state) (cons 'step executed-commands) depth)
             )
         )
        ((equal? (car expression-list) 'turn-left)
         (parse-expressions (cdr expression-list) (do-turn-left current-state) (cons 'turn-left executed-commands) depth)
         )
        ((equal? (car expression-list) 'put-mark)
         (parse-expressions (cdr expression-list) (edit-mark current-state #t) (cons 'put-mark executed-commands) depth)
         )
        ((equal? (car expression-list) 'get-mark)
         (if (mark? current-state)
             (parse-expressions (cdr expression-list) (edit-mark current-state #f) (cons 'get-mark executed-commands) depth)
             error-output
             )
         )
        ((equal? (car expression-list) 'if)
         (parse-expressions (cons (list 'if (cadr expression-list) (caddr expression-list) (cadddr expression-list)) (cddddr expression-list)) current-state executed-commands depth)
         )
        ((null? (car expression-list)) (parse-expressions (cdr expression-list) current-state executed-commands depth))
        ((list? (car expression-list))
         (if (equal? (caar expression-list) 'if)
             (let* (
                    (statement (car expression-list))
                    (condition (cadr statement))
                    (positive (caddr statement))
                    (negative (cadddr statement))
                    )
               (cond
                 ((equal? condition 'north?)
                  (if (north? current-state)
                      (parse-expressions (my-append positive (cdr expression-list)) current-state executed-commands depth)
                      (parse-expressions (my-append negative (cdr expression-list)) current-state executed-commands depth)
                      )
                  )
                 ((equal? condition 'mark?)
                  (if (mark? current-state)
                      (parse-expressions (my-append positive (cdr expression-list)) current-state executed-commands depth)
                      (parse-expressions (my-append negative (cdr expression-list)) current-state executed-commands depth)
                      )
                  )
                 ((equal? condition 'wall?)
                  (if (wall? current-state)
                      (parse-expressions (my-append positive (cdr expression-list)) current-state executed-commands depth)
                      (parse-expressions (my-append negative (cdr expression-list)) current-state executed-commands depth)
                      )
                  )
                 (else error-output)
                 )
               )    
             )
         )
        (else
         ;It is a custom command -> find it:
         (let (
               (result (custom-keyword (car expression-list) current-state executed-commands program (+ depth 1)))
               )
           (if (= (caddr result) 1)
               result
               (parse-expressions (cdr expression-list) (cadr result) (car result) depth)
               )
           )
         )
        )
      )
    )
  
  (if (not (list? expr))
      (let ((result (parse-expressions (list expr) state '() 0)))
        (list (reverse (car result)) (cadr result))
        )
      
      (let ((result (parse-expressions expr state '() 0)))
        (list (reverse (car result)) (cadr result))
        )
      )
  )


(define (evaluate programs pairs thresholds limit)
  ;Tested
  (define (get-commands program)
    (cond
      ((null? program) 0)
      ((not (list? program)) 1)
      ((list? (car program)) (+ (get-commands (car program)) (get-commands (cdr program))))
      ((equal? (car program) 'procedure) (+ 1 (get-commands (caddr program)) (get-commands (cdddr program))))
      ((equal? (car program) 'if) (+ 1 (get-commands (caddr program)) (get-commands (cadddr program))))
      (else (+ 1 (get-commands (cdr program))))
      )
    )
  ;Tested
  (define (test-state actual expected)
    (define (test-map act exp)
      (define (test-row a e)
        (define (test-elem m n) (if (equal? m 'w) 0 (abs (- m n))))
        (let ((result (test-elem (car a) (car e))))
          (if (null? (cdr a)) result (+ result (test-row (cdr a) (cdr e))))
          )
        )
      (let ((result (test-row (car act) (car exp))))
        (if (null? (cdr act)) result (+ result (test-map (cdr act) (cdr exp))))
        )
      )
    
    (define (test-robot act exp)
      (define (test-orientation a e) (if (equal? a e) 0 1))
      (define (test-coordinates a e) (+ (abs (- (car a) (car e))) (abs (- (cadr a) (cadr e)))))
      (+ (test-orientation (cadr act) (cadr exp)) (test-coordinates (car act) (car exp)))
      )
    (list (test-map (car actual) (car expected)) (test-robot (cdr actual) (cdr expected)))
    )
  
  (define (testProgram program)
    (define (testPairs program pairs manhattan state command-length)
      (if (null? pairs) (list manhattan state command-length)
          (let* ((our-pair (car pairs))
                 (source (car our-pair))
                 (goal (cadr our-pair))
                 (simulation (simulate source 'start program limit))
                 (result (test-state (cadr simulation) goal))
                 (new-manhattan (+ manhattan (car result)))
                 (new-state (+ state (cadr result)))
                 (new-command-length (+ command-length (length (car simulation)))))
            
            (if (or (> new-manhattan (car thresholds)) (> new-state (cadr thresholds)) (> new-command-length (cadddr thresholds)))
                (list -1 -1 -1 -1)
                (testPairs program (cdr pairs) new-manhattan new-state new-command-length)
                )
            )
          )
      )
    (let ((command-length (get-commands program)))
      (if (> command-length (caddr thresholds))
          (list -1 -1 -1 -1)
          (let ((result (testPairs program pairs 0 0 0)))
            (list (car result) (cadr result) (get-commands program) (caddr result))
            )
          )
      )
    )
  
  (define (insert-sort element list)
    (if (null? list) (cons element list)
        (if (is-less? (car element) (caar list)) (cons element list)
            (cons (car list) (insert-sort element (cdr list))))))
  
  
  (define (is-invalid? result)
    (cond ((null? result) #f)
          ((equal? (car result) '-1) #t)
          (else (is-invalid? (cdr result)))))
  
  
  (define (evaluatePrograms programs)
    (if (null? programs) '()
        (let ((result (testProgram (car programs))))
          (if (is-invalid? result)
              (evaluatePrograms (cdr programs))
              (insert-sort (list result (car programs)) (evaluatePrograms (cdr programs)))
              )
          )      
        )
    )
  
  (evaluatePrograms programs)
  
  )

;----------



(define (congruential-rng seed)
  (let ((a 16807 #|(expt 7 5)|#)
        (m 2147483647 #|(- (expt 2 31) 1)|#))
    (let ((m-1 (- m 1)))
      (let ((seed (+ (remainder seed m-1) 1)))
        (lambda (b)
          (let ((n (remainder (* a seed) m)))
            (set! seed n)
            (quotient (* (- n 1) b) m-1)))))))
(define random (congruential-rng 12345))


;Example program
(define (get-example)
  '((procedure start (
                      step start))))


(define (get-procedure-list program)
  (define (get-procedure-list-inner ls prog)
    (if (null? prog) ls
        (get-procedure-list-inner (cons (cadar prog) ls) (cdr prog))))
  (get-procedure-list-inner '() program))



;Remove empty procedures (except for start procedure)
;Remove uncalled procedures (except for start procedure)
;Erase empty brackets
;Bracket (procedure start (if wall? () ())) -> (procedure start ((if wall? () ()))
;Bracket (if wall? step step) -> (if wall? (step) (step))
(define (validate program)
  (define (remove-proc name) ;Searches for calls and deletes them
    (define (inner-procedures prog)
      (define (inner prog)
        (if (null? prog) prog
            (if (list? prog)
                (cons (inner (car prog)) (cdr prog))
                (if (equal? prog name) '() prog))))
      
      (cond
        ((null? prog) prog)
        ((equal? (cadar prog) name) (inner-procedures (cdr prog)))
        (else
         (cons (list 'procedure (cadar prog) (inner (caddar prog))) (inner-procedures (cdr prog))))))
    
    (inner-procedures program))
  
  (define (remove-empty-proc prog) ;Removes empty procedures and fixes lowest if
    (cond
      ((null? prog) prog)
      ((equal? (cadar prog) 'start) (cons (car prog) (remove-empty-proc (cdr prog))))
      ((null? (caddar prog)) (validate (remove-proc (cadar prog))))
      ((equal? (car (caddar prog)) 'if) (cons (list 'procedure (cadar prog) (list (caddar prog))) (remove-empty-proc (cdr prog))))
      (else
       (cons (car prog) (remove-empty-proc (cdr prog))))))
  
  
  (remove-empty-proc program)
  )

#|
(define (validate prog)
  (define (has-call? prog call)
    (cond
      ((equal? call 'start) #t)
      ((null? prog) #f)
      ((equal? prog call) #t)
      ((list? prog) (or (has-call? (car prog) call) (has-call? (cdr prog) call)))
      (else #f)))
  
  (define (remove-procedure-call procedure call)
    (cond
      ((null? procedure) procedure)
      ((equal? procedure call) '())
      ((list? procedure) (cons (remove-procedure-call (car procedure) call) (remove-procedure-call (cdr procedure) call)))
      (else procedure)))
  
  (define (remove-procedure prog procedure)
    (if (null? prog) prog
        (if (equal? (cadar prog) procedure) (cdr prog)
            (cons (remove-procedure-call (car prog) procedure) (remove-procedure (cdr prog) procedure)))))
  
  (define (validate-procedures prog procedure-list)
    (if (null? procedure-list) prog
        (if (has-call? prog (car procedure-list)) (validate-procedures prog (cdr procedure-list))
            (validate-procedures (remove-procedure prog (car procedure-list)) (cdr procedure-list)))))
  
  (define (erase-empty-procedures program)
    (define (inner prog)
      (cond
        ((null? prog) program)
        ((null? (caddar prog)) (erase-empty-procedures (remove-procedure program (cadar prog))))
        (else (inner (cdr prog)))))
    (inner program))
  
  (define (erase-empty-brackets prog)
    #|(display prog) (newline) (newline)|#
    (cond
      ((null? prog) prog)
      ((list? prog)
       (cond
         ((equal? (car prog) 'if)
          (if (and (null? (caddr prog)) (null? (cadddr prog))) '()
              (list 'if (cadr prog) (erase-empty-brackets (caddr prog)) (erase-empty-brackets (cadddr prog)))))
         ((null? (car prog)) (erase-empty-brackets (cdr prog)))
         (else
          (cons (erase-empty-brackets (car prog)) (erase-empty-brackets (cdr prog))))))
      (else prog)))
  
  (define (bracket-singles prog)
    (cond
      ((null? prog) prog)
      ((list? prog)
       (if (equal? (car prog) 'if)
           (if (not (list? (caddr prog)))
               (if (not (list? (cadddr prog)))
                   (list 'if (cadr prog) (list (caddr prog)) (list (cadddr prog)))
                   (list 'if (cadr prog) (list (caddr prog)) (bracket-singles (cadddr prog))))
               (if (not (list? (cadddr prog)))
                   (list 'if (cadr prog) (bracket-singles (caddr prog)) (list (cadddr prog)))
                   (list 'if (cadr prog) (bracket-singles (caddr prog)) (bracket-singles (cadddr prog)))))
           (cons (bracket-singles (car prog)) (bracket-singles (cdr prog)))))
      (else prog)))
  
  (if (null? prog) '((procedure start ()))
      (bracket-singles (erase-empty-brackets (erase-empty-procedures (validate-procedures prog (get-procedure-list prog)))))))
|#

(define (mutate program)
  (validate (call-mutate (validate program))))

(define (call-mutate program)
  
  
  (define (get-highest-procedure)
    (define (inner ls)
      (if (null? ls) 0
          (if (not (number? (car ls))) (inner (cdr ls))
              (let ((result (inner (cdr ls))))
                (if (> (car ls) result) (car ls) result)))))
    (inner (get-procedure-list program)))
  
  (define (add)  
    (define (add-call)
      (define (add-procedure-call)
        (define (get-procedure-from-list ls index)
          (if (= index 0) (car ls)
              (get-procedure-from-list (cdr ls) (- index 1))))
        
        (let ((procedure-list (get-procedure-list program)))
          (get-procedure-from-list procedure-list (random (length procedure-list)))))
      
      (define (add-if)
        (define (get-question)
          (let ((rand (random 5)))
            (cond
              ((< rand 3) 'wall?)
              ((< rand 5) 'north?)
              (else 'mark?))))
        (if (= 0 (random 4))
            '(if wall? (turn-left) (step))
            (list 'if (get-question) (list (add-call)) (list (add-call)))))
      
      (let ((rand (random 14)))
        (cond
          ((< rand 6) 'step)
          ((< rand 9) 'turn-left)
          ((= rand 9) (add-procedure-call))
          ((= rand 10) 'put-mark)
          ((= rand 11) 'get-mark)
          (else (add-if)))))
    
    (define (add-procedure)
      ;Create function with random call
      ;Place function call at random place of 1st level
      (define (place-call func-name program)
        (let* ((first-func (car program))
               (first-name (cadr first-func))
               (first-code (caddr first-func)))
          (if (list? first-code)
              (cons (list 'procedure first-name (cons func-name first-code)) (cdr program))
              (cons (list 'procedure first-name (list func-name first-code)) (cdr program)))))
      
      (define (createProcedure)
        (let ((name (+ (get-highest-procedure) 1)))
          (list name (append program (list (list 'procedure name (list (add-call))))))))
      
      (let* ((result (createProcedure))
             (name (car result))
             (new-prog (cadr result)))
        (place-call name new-prog))
      )
    
    (define (add-element)
      (define (find-place procedure) ;Calls add-call at random point of program
        (define (find-place-inner rest)
          (define (handle-condition rest)
            (if (= (random 2) 0)
                (append (list (car rest) (cadr rest) (find-place-inner (caddr rest))) (cdddr rest))
                (append (list (car rest) (cadr rest) (caddr rest) (find-place-inner (cadddr rest))) (cddddr rest))))
          
          (define (goto-place rest index)
            (if (= index 0)
                (if (list? (car rest))
                    (let ((rand (random 4)))
                      (cond
                        ((= rand 0) (cons (add-call) rest))
                        ((= rand 1) (cons (car rest) (cons (add-call) (cdr rest))))
                        (else (cons (handle-condition (car rest)) (cdr rest)))))
                    (if (= (random 2) 0)
                        (cons (add-call) rest)
                        (cons (car rest) (cons (add-call) (cdr rest)))))
                (cons (car rest) (goto-place (cdr rest) (- index 1)))))
          
          (if (null? rest)
              (add-call)
              (if (list? rest)
                  (if (equal? (car rest) 'if)
                      (let ((rand (random 4)))
                        (cond
                          ((= rand 0) (list (add-call) rest))
                          ((= rand 1) (list rest (add-call)))
                          (else (handle-condition rest)) ))
                      (goto-place rest (random (length rest))))
                  (if (= (random 2) 0)
                      (list (add-call) rest)
                      (list rest (add-call))))))
        
        
        (list (car procedure) (cadr procedure) (find-place-inner (caddr procedure))))
      
      (define (select-procedure) ;Calls find-place on randomly chosen procedure
        
        (define (select-procedure-inner rest index)
          (if (= 0 index)
              (cons (find-place (car rest)) (cdr rest))
              (cons (car rest) (select-procedure-inner (cdr rest) (- index 1)))))
        
        (select-procedure-inner program (random (length program))))
      
      (select-procedure))
    
    (if (= (random 10) 0)
        (add-procedure)
        (add-element)))
  
  (define (remove)
    (define (find-call procedure)
      (define (find-call-inner procedure index)
        (display procedure) (newline)
        (if (null? procedure) procedure
            (if (= index 0)
                (if (list? (car procedure))
                    (if (= (random 4) 0) (cdr procedure)
                        (if (= (random 2) 0) (cons (list 'if (cadar procedure) (find-call-inner (caddar procedure) (random (length (caddar procedure)))) (car (cdddar procedure))) (cdr procedure))
                            (cons (list 'if (cadar procedure) (caddar procedure) (find-call-inner
                                                                                  (car (cdddar procedure)) (random (length (car (cdddar procedure)))))) (cdr procedure))))
                    (cdr procedure))
                (cons (car procedure) (find-call-inner (cdr procedure) (- index 1))))))
      
      (if (list? (caddr procedure))
          (list 'procedure (cadr procedure) (find-call-inner (caddr procedure) (random (length (caddr procedure)))))
          (find-call (list 'procedure (cadr procedure) (list (caddr procedure))))))
    
    
    (define (select-procedure prog index)
      (if (= index 0) (cons (find-call (car prog)) (cdr prog))
          (cons (car prog) (select-procedure (cdr prog) (- index 1)))))
    
    (select-procedure program (random (length program))))
  
  
  (if (null? program) '((procedure start ()))
      (if (> (random 2) 0)
          (validate (add))
          (validate (remove)))))

#|
(evolve <pairs> <threshold> <stack_size>) where
<pairs>
    is a list of pairs of states, including the position and the orientation of the robot;
<threshold>
    is lower bounds on the quality of the program in order to appear in the output;
<stack_size>
    is the limit on the robot simulator stack size. |#
(define (evolve pairs thresholds stack-size)
  (define (POPSIZE) 100)
  (define (create-initial)
    (define (get-start) '(
                          ((procedure start (step)))
                          ((procedure start ((if wall? (turn-left) (step)))))
                          ((procedure start ((if mark? () (1)) 1 step step step)) (procedure 1 (put-mark get-mark step step step)))
                          ((procedure start (turn-right (if wall? (turn-left (if wall? (turn-left (if wall? turn-left step)) step)) step) put-mark start))
                           (procedure turn-right (turn-left turn-left turn-left)))
                          ((procedure turn-north ((if north? () (turn-left turn-north)))))
                          ((procedure add-one ((if mark? (get-mark step add-one turn-180 step turn-180) (put-mark)))) 
                           (procedure turn-180 (turn-left turn-left)))
                          ((procedure start ((if wall? (turn-left turn-left) (step start step)))))
                          ((procedure start ((if mark? (get-mark step start turn-180 step turn-180) (put-mark))) (procedure turn-180 (turn-left turn-left))))
                          ((procedure start ((if wall? put-mark step))))
                          ((procedure start fill-maze)
                           (procedure fill-maze ((if mark? () (put-mark (if wall? () (step fill-maze step-back)) turn-left (if wall? () (step fill-maze step-back)) turn-left turn-left (if wall? () (step fill-maze step-back)) turn-left))))
                           (procedure step-back (turn-left turn-left step turn-left turn-left)))
                          ))
    
    (define (load-start population)
      (if (> (length population) (POPSIZE)) (load-start (cdr population))
          (if (= (length population) (POPSIZE)) population
              (load-start (append population (get-start))))))
    (load-start '()))
  
  (define (get-next-pop population old-pop)
    (define (cut percentage) ;Cuts bottom [percentage] percent of programs
      (define (inner n threshold prog)
        (if (<= n threshold) '()
            (cons (car prog) (inner (- n 1) threshold (cdr prog)))))
      (inner (length population) (* (POPSIZE) (/ percentage 100)) population))
    
    
    (define (get-from-percentage percentage)
      (let* ((res (cut percentage))
             (which (random (length res))))
        (get-nth res which)))
    
    (define (next-wave new-population len)
      (if (>= len (POPSIZE)) (map mutate new-population)
          (next-wave (cons (get-from-percentage (+ (* (random 9) 10) 10)) new-population) (+ len 1))))
    
    (if (< (length population) (POPSIZE)) (get-next-pop (cons (get-nth old-pop (random (length old-pop))) population) old-pop) 
        (next-wave '() 0)))
  
  (define (eval-pop population best)
    (display "popsize: ") (display (length population)) (display " result: ")
    (let* ((result (evaluate population pairs thresholds stack-size))
           (best-score (caar result))
           (new-pop (map (lambda (n) (cadr n)) result))
           (best-code (car new-pop)))
      (display (length result)) (newline)
      (if (> 2 (length result)) ((display population) (newline)))
      (if (is-less? best-score best)
          (begin (display (list best-score best-code)) (newline) (flush-output) (eval-pop (get-next-pop new-pop population) best-score))
          (eval-pop (get-next-pop new-pop population) best))))
  
  (eval-pop (create-initial) '(999999 0 0 0)))



;----------TEST
(define s0
  '((
     (w w w)
     (w 1 w)
     (w w w)
     )
    (1 1) south
    ))

(define t0
  '((
     (w w w)
     (w 1 w)
     (w w w)
     )
    (1 1) south
    ))

(define s1
  '((
     (w w w)
     (w 1 w)
     (w 0 w)
     (w w w)
     )
    (1 1) west
    ))

(define t1
  '((
     (w w w)
     (w 1 w)
     (w 0 w)
     (w w w)
     )
    (1 1) west
    ))

(define s2
  '((
     (w w w)
     (w 0 w)
     (w 0 w)
     (w 0 w)
     (w 0 w)
     (w 1 w)
     (w 0 w)
     (w 0 w)
     (w w w)
     )
    (1 1) south
    ))

(define t2
  '((
     (w w w)
     (w 0 w)
     (w 0 w)
     (w 0 w)
     (w 0 w)
     (w 1 w)
     (w 0 w)
     (w 0 w)
     (w w w)
     )
    (1 7) south
    ))


