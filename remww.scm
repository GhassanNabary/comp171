(define get-read-reg
  (lambda (inst)
    (cadr inst)
    ))

(define get-write-reg
  (lambda (inst)
    (caddr inst)
    ))


;check if the inst writes to register
(define write-inst?
  (lambda (inst)
    (if (null? (caddr inst))
        #f
        #t
        )
    ))


;;remove instructions with no writes
(define remove-no-writes
  (lambda (instLst newLst)
    (if (null? instLst)
        newLst
        (let ((curInst (car instLst)))
          (if (write-inst? curInst)
              (remove-no-writes (cdr instLst) (append newLst (list curInst)))
              (remove-no-writes (cdr instLst) newLst)
              )))
    ))


(define reg-in-write?
  (lambda (reg lst)
    (ormap (lambda (inst)
             (member reg (get-write-reg inst)))
           lst)))

(define reg-in-read?
  (lambda (reg lst)
    (ormap (lambda (inst)
             (member reg (get-read-reg inst)))
           lst)))


;;check if two lists have a common register
(define check-lst-members
  (lambda (lst1 lst2)
    (if (null? lst1)
        #f
        (if (member (car lst1) lst2)
            #t
            (check-lst-members (cdr lst1) lst2)
            ))
    ))


(define common-read-reg
  (lambda (inst write-tos)
    (let ((read-reg (get-read-reg inst)))
      (check-lst-members read-reg write-tos)
      )
    )
  )


(define common-write-reg
  (lambda (inst write-tos)
    (let ((write-reg (get-write-reg inst)))
      (check-lst-members write-reg write-tos)
      )
    )
  )


;;remove writes to orveridden registers
(define clean-overriden-writes
  (lambda (lst write-tos)
    (if (null? lst)
        lst
        (let* ((curInst (car lst))
               (tail (cdr lst))
               (write-regs (get-write-reg curInst))
               (read-regs (get-read-reg curInst)))
          (cond ((equal? write-regs write-tos)
                 (clean-overriden-writes tail write-tos))
                ((and (common-read-reg curInst write-tos) (not (common-write-reg curInst write-tos))) ;same read registers, not same write registers
                 lst)
                (else (cons curInst (clean-overriden-writes tail write-tos)))
                )
          )
        )
    ))


;;work on a reversed list
(define check-instruction-writes
  (lambda (lst)
    (if (null? lst)
        lst
        (let* ((curInst (car lst))
               (tail (cdr lst))
               (write-regs (get-write-reg curInst))
               (read-regs (get-read-reg curInst)))
          (if (check-lst-members read-regs write-regs) ;have same registers in read and write
              (append (list curInst) (check-instruction-writes tail)) ;don't remove if r/w have same registers
              (append (list curInst) (check-instruction-writes (clean-overriden-writes tail write-regs)))) ;
          ))))

(define need-to-remove 1)
(define register-was-read-from 0)

(set! need-to-remove 1)
(set! register-was-read-from 0)

(define check-if-need-to-remove
  (lambda (write-regs inst2)
    (if (check-lst-members write-regs (get-read-reg inst2))
        (set! register-was-read-from 1)
        (if (and (check-lst-members write-regs (get-write-reg inst2)) (= register-was-read-from 0))
            (set! need-to-remove 0)
            (set! need-to-remove 1))
        )
    ))

(define remove-overriden-writes2
  (lambda (lst1 lst2 result) ;lst2 = cdr lst1
    (cond ((null? lst1) result)
					((and (null? lst2) (null? (cdr lst1))) (remove-overriden-writes2 (cdr lst1) lst2 (append result (list (car lst1)))))
					((null? lst2) (begin (set! need-to-remove 1) (set! register-was-read-from 0) (remove-overriden-writes2 (cdr lst1) (cddr lst1) (append result (list (car lst1))))))
					(else
				        (let* ((curInst (car lst1))
				               (tail (cdr lst1))
				               (write-regs (get-write-reg curInst))
				               (read-regs (get-read-reg curInst))
				               (compare-instr (car lst2)))
				          (if (check-lst-members read-regs write-regs) ;have same registers in read and write
				              (remove-overriden-writes2 tail (cdr tail) (append result (list curInst))) ;don't remove if r/w have same registers
				              (if need-to-remove
				                  (cond ((null? (car lst2)) (remove-overriden-writes2 tail (cdr tail) (append result (list curInst))))
				                        (else (begin (check-if-need-to-remove write-regs compare-instr)
				                                     (if need-to-remove
				                                         (remove-overriden-writes2 lst1 (cdr lst2) result)
				                                         ((begin (set! need-to-remove 0) (set! register-was-read-from 0)
				                                                 (remove-overriden-writes2 tail (cdr tail) result)) ;remove the instruction)
				                                          )))));recursion, check if need to remove
				                    (begin (set! need-to-remove 0)
				                           (remove-overriden-writes2 tail (cdr tail) result)) ;remove the instruction
				             )
        ))))))


(define check-if-stay2
			(lambda (writes result)
				(if (null? result)
						#t
						(let* ((curInst (car result))
									 (tail (cdr result)))
						(cond ((check-lst-members (list writes) (get-read-reg curInst)) #t)
									((not (check-lst-members (list writes) (get-write-reg curInst))) (check-if-stay2 writes tail))
									(else #f)
						))
					)
			)
	)

(define check-if-stay (lambda (writes result)
		(ormap (lambda (instruction-writes) (check-if-stay2 instruction-writes result)) writes)))

(define until-nekudat-shevet
    (lambda (lst result)
          (if (null? lst)
              result
              (let ((curInst (car lst))
                    (tail (cdr lst)))
                    (if (not (check-if-stay (get-write-reg curInst) result))
                        (until-nekudat-shevet tail result)
                        (until-nekudat-shevet tail (append (list curInst) result)))
))))

(define remww
  (lambda (instLst)
    (if (null? instLst)
        '()
				(let ((after-check-instructions (reverse (check-instruction-writes (reverse (remove-no-writes instLst '()))))))
							(until-nekudat-shevet (reverse (remove-overriden-writes2 after-check-instructions (cdr after-check-instructions) '())) '())
        ))))
