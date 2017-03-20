;(simulate <state> <expr> <program> <limit>)
;<state> is a state of the robot of the form: (<maze> (<coordinate-x> <coordinate-y>) <orientation>)
;<expr> can content form, command, or procedure call.
;<program> can content list of procedure definitions.
;<limit> is a number which means maximum nested procedure calls. If a new procedure call exceeds the limit then this call cannot be done.
 
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

  (define (is-less? elem other)
    (let ((elem-1 (car elem))
          (elem-2 (cadr elem))
          (elem-3 (caddr elem))
          (elem-4 (cadddr elem))
          (other-1 (car other))
          (other-2 (cadr other))
          (other-3 (caddr other))
          (other-4 (cadddr other)))
      (cond
        ((< other-1 elem-1) #f)
        ((< other-2 elem-2) #f)
        ((< other-3 elem-3) #f)
        ((< other-4 elem-4) #f)
        (else #t)
        )
      )
    )
  
  (define (insert-sort element list)
    (if (null? list) (cons element list)
        (if (not (is-less? element (car list))) (cons element list)
            (cons (car list) (insert-sort element (cdr list))))))
  
  
  (define (evaluatePrograms)
    
    )
  
  (testProgram programs)
  

  ;For testing only
  )




    
    
;TEST-----
(define right-hand-rule-prg
  '(
    (procedure start
               ( turn-right
                 (if wall?
                     ( turn-left
                       (if wall?
                           (turn-left
                            (if wall?
                                turn-left
                                step
                                )
                            )
                           step
                           )
                       )
                     step  
                     )
                 put-mark
                 start
                 )
               )   
    (procedure turn-right (turn-left turn-left turn-left))
    )
  )

(define test-1
  '((procedure start (put-mark (if wall? turn-left step) start)))
  )

(define simple-rule
  '(turn-left turn-left step turn-left))

(define simple-rule-2
  '(turn-left (if (north?) (step turn-left) (turn-left step)) step))

(define pairs
  '(
    (
     (((w w w w w w) 
       (w 0 w 0 w w) 
       (w 1 w 0 0 w) 
       (w 1 0 0 w w) 
       (w w w w w w)) 
      (1 3) south)

     (((w w w w w w) 
       (w 0 w 0 w w) 
       (w 0 w 0 0 w) 
       (w 0 0 0 w w) 
       (w w w w w w)) 
      (1 1) north)
     )
    (
     (((w w w w w w) 
       (w 0 w 0 w w) 
       (w 0 w 2 0 w) 
       (w 1 3 0 w w) 
       (w w w w w w)) 
      (3 3) north)

     (((w w w w w w) 
       (w 0 w 0 w w) 
       (w 0 w 0 0 w) 
       (w 0 0 0 w w) 
       (w w w w w w)) 
      (1 1) north)
     ))
  )

(define stuff '((procedure start (put-mark (if wall? turn-left step) start))))

(define get-maze
  '(
    (w w w w w w)
    (w 0 w 0 w w)
    (w 0 w 0 0 w)
    (w 0 0 0 w w)
    (w w w w w w)
    )
  )
(define right-hand-rule-prg
  '(
    (procedure start
               ( turn-right
                 (if wall?
                     ( turn-left
                       (if wall?
                           (turn-left
                            (if wall?
                                turn-left
                                step
                                )
                            )
                           step
                           )
                       )
                     step  
                     )
                 put-mark
                 start
                 )
               )   
    (procedure turn-right (turn-left turn-left turn-left))
    )
  )
;----------
