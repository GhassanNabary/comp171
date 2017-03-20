(load "pattern-matcher.scm")
(load "pc.scm")
;--------------------help functions--------------

(define ^case-insensitive
  (lambda (var insensitive-val)
    (new
      (*parser (word-ci var))
      (*pack
        (lambda(_) insensitive-val))
      done)))


(define <digit-1-9>
  (range #\1 #\9))

(define <digit-0-9>
  (range #\0 #\9))

(define <digits0-9>
  (range #\0 #\9))

(define <digits1-9>
  (range #\1 #\9))


 (define <Space>
  (new (*parser (char #\space))
    done))


(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
   (new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
        done)))
    (new (*parser (char #\;))

   (*parser <any-char>)
   (*parser <end-of-line-comment>)
   *diff *star

   (*parser <end-of-line-comment>)
   (*caten 3)
   done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

(define <infix-comment>
   (new
       (*parser (word "#;"))
       (*parser (char #\ )) *star
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
      done))

(define <comment>
  (disj <line-comment>
  <sexpr-comment>))

(define <skip>
  (disj <comment>
  <whitespace>))

(define <commentInfix>
  (disj <line-comment>
  <infix-comment>))

(define <skipInfix>
  (disj <commentInfix>
  <whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
     (*parser <p>)
     (*parser <wrapper>)
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))
(define ^<skippedInfix*> (^^<wrapped> (star <skipInfix>)))
;;;

;--------------------bool------------
(define <boolean>
    (new
    (*parser (^case-insensitive "#f" '#f))
    (*parser (^case-insensitive "#t" '#t))
        (*disj 2)
    done))
;---------------chars-------------

(define <char-prefix>
  (new
    (*parser (word "#\\"))
  done))

(define <visible-simple-char>
  (new
    (*parser (range (integer->char 33) (integer->char 255)))
  done))


(define <named-char>
  (new
    (*parser (^case-insensitive "lambda" (integer->char 955)))
    (*parser (^case-insensitive "newline" #\newline))
    (*parser (^case-insensitive "nul" #\nul))
    (*parser (^case-insensitive "page" #\page))
    (*parser (^case-insensitive "return" #\return))
    (*parser (^case-insensitive "space" #\space))
    (*parser (^case-insensitive "tab" #\tab))
    (*disj 7)
  done))

  (define <hex-char>
    (new
      (*parser <digits0-9>)
      (*parser (range-ci #\a #\f))
      (*disj 2)
    done))

  (define <hex-unicode-char>
    (new
      (*parser (char-ci #\x))
      (*parser <hex-char>) *plus
      (*caten 2)
      ;(*pack-with (lambda(_ hex)
      ;       (integer->char
      ;         (string->number
      ;           (list->string hex) 16))))
      (*guard (lambda (x) (if (integer? x) (> 131071 x))))

      (*pack-with (lambda(_ hex)
        (let ((ans (string->number (list->string hex) 16)))
        (if (< ans 131071)
          (integer->char ans)
          #\x ))))
    done))

  (define fail
    <fail>
    )

  (define <char>
    (new
      (*parser <char-prefix>)
      (*parser <named-char>)
      (*parser <hex-unicode-char>)
      (*parser <visible-simple-char>)
      (*disj 3) ;pop 3 types of chars with disjoint
      (*caten 2)
      (*pack-with
        (lambda (pref char)
          char)) ;return the char w/o the prefix
    done))

  ;(*pack-with (lambda(_ hex)
  ;       (integer->char
  ;         (string->number
  ;           (list->string hex) 16))))

  ;(*pack-with (lambda(_ hex)
  ; (let ((ans (string->number (list->string hex) 16)))
  ; (if (< ans 131071)
  ;   (integer->char ans)
  ;   #\a ))))

(define fail
  <fail>
  )

(define <char>
  (new
    (*parser <char-prefix>)
    (*parser <hex-unicode-char>)
    (*parser <named-char>)
    (*parser <visible-simple-char>)
    (*disj 3) ;pop 3 types of chars with disjoint
    (*caten 2)
    (*pack-with
      (lambda (pref char)
        char)) ;return the char w/o the prefix
    ; (*parser <end-of-input>)
    ; (*caten 2)
    ; (*pack-with (lambda(a b)
    ;   a))
  done))

(define <symbol-char-no-calc>
    (new
    (*parser <digits0-9>)
    (*parser (char #\!))
    (*parser (char #\$))
    (*parser (char #\^))
    (*parser (char #\*))
    (*parser (char #\-))
    (*parser (char #\_))
    (*parser (char #\=))
    (*parser (char #\+))
    (*parser (char #\<))
    (*parser (char #\>))
    (*parser (char #\?))
    (*parser (char #\/))
    (*parser (range #\a #\z))
    (*parser (range #\A #\Z))
    (*pack (lambda (char)
      (integer->char (+ 97 (- (char->integer char) (char->integer #\A))))))
    (*disj 15)
    (*pack (lambda (x) x))
 done))

(define <symbol-char>
  (new

    (*parser (range #\0 #\9) )
    (*parser (char #\!))
    (*parser (char #\$))
    (*parser (char #\^))
    (*parser (char #\*))
    (*parser (char #\-))
    (*parser (char #\_))
    (*parser (char #\=))
    (*parser (char #\+))
    (*parser (char #\<))
    (*parser (char #\>))
    (*parser (char #\?))
    (*parser (char #\/))

    (*parser (range #\a #\z))

    (*parser (range #\A #\Z))
       (*pack
        (lambda (ch)
          (integer->char (+ 97 (- (char->integer ch) (char->integer #\A))))))
    (*disj 15)
    done))


; (define <symbol>
;   (new
;     (*parser <symbol-char>) *plus
;     (*pack
;       (lambda (lst)
;         (string->symbol
;           (list->string lst))))
;   done)
; )

;?Symbol?::=?SymbolChar? +
(define <symbol>
  (new
    (*parser <symbol-char>)
    *plus
    (*pack (lambda  (s)
      (let(( d (list->string s) ))
        (if (string->number d) (string->number d) (string->symbol `(,@d))))))
  done))



;-----------------------numbers------
(define <Natural>
  (new (*parser <digit-0-9>) *plus

    (*pack (lambda (n)
    (string->number
     (list->string
             n))))

done))


(define <Integer>
  (new (*parser (char #\+))
        (*parser <Natural>)
        (*caten 2)
        (*pack-with
            (lambda (++ n) n))


        (*parser (char #\-))
        (*parser <Natural>)
        (*caten 2)

        (*pack-with
            (lambda (-- n) (- n)))

        (*parser <Natural>)
        (*disj 3)
;(*parser <end-of-input>)
;(*caten 2)
;(*pack-with (lambda (n m) n))
  done))


(define <Fraction>
(new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
  (lambda (num div den)
    (/ num den)))
       done))

(define <Number>

    (new
        (*parser <Fraction>)
        (*parser <Integer>)
        (*disj 2)

       ; (*parser <end-of-input>)
       ; (*caten 2)
        ;(*pack-with (lambda (n m) n))
done))



(define <StringLiteralChar>
  (new
       (*parser <any-char>)
       (*parser (char #\"))
       *diff
      ;(*parser <end-of-input>)
      ;(*caten 2)

;*pack-with (lambda (n t) n))
      done))

(define <StringHexChar>
  (new
    (*parser (char #\\))
   ; (*parser (char #\x))
    (*parser <hex-unicode-char>)
    *star
    (*pack
    (lambda  (s) `(,@s)))
    (*parser (char #\;))
    (*caten 3)

    (*pack-with
      (lambda  (y x s )
        (car x)))
  done))

(define ^<meta-char>
    (lambda (str ch)
      (new
        (*parser (word str))
        (*pack (lambda (_) ch))
      done)))

(define <StringMetaChar>
    (new (*parser (^<meta-char> "\\\\" #\\))
         (*parser (^<meta-char> "\\\"" #\"))
         (*parser (^<meta-char> "\\t" #\tab))
         (*parser (^<meta-char> "\\f" #\page))
         (*parser (^<meta-char> "\\r" #\return))
         (*parser (^<meta-char> "\\n" #\newline))

       (*disj 6)

       done))


(define <StringChar>
 (new
  (*parser <StringHexChar> )
        (*parser <StringMetaChar>)
        (*parser <StringLiteralChar>)
        (*disj 3)

        done))

(define <String>
  (new
      (*parser (char #\"))
      (*parser <StringChar>)
      *star
      (*parser (char #\"))
      (*caten 3)

      (*pack-with
        (lambda (open-delim chars close-delim)
          (list->string chars)
        ))
  done))

;-----------------quoted------------------
(define <quoted>
  (new
    (*parser (char #\'))
    (*delayed (lambda () <sexpr> ))
        (*caten 2)

    (*pack-with (lambda (q chars)
      `',chars))
    done)
)




;--------------------------unqouted------------------------------

(define <unquoted>
  (new
    (*parser (char #\,))
    (*delayed (lambda () <sexpr> ))
        (*caten 2)
    (*pack-with (lambda (q chars)
      (list 'unquote chars)
      ))
    done)
)

;-----------------Properlist-----------

(define <ProperList>
  (new
    (*parser (char #\())

          ;(*parser (<Lang-sep> (delay (lambda () <sexpr>))))
          (*parser (delay (lambda () <sexpr>)))
          *star

        (*parser (char #\)))
        (*caten 3)
        (*pack-with (lambda (a chars c)
         chars
          ))

    done))

;-----------------improperlist-----------


(define <improper-list>
  (new
    (*parser (char #\())
        (*delayed (lambda () <sexpr>)) *plus
    (*parser (char #\.))
        (*delayed (lambda () <sexpr>))
    (*parser (char #\)))
    (*caten 5)
    (*pack-with
      (lambda(lbracket firstsexpr point tailExpr rbracket)
        `(,@firstsexpr ,@tailExpr)))
  done)
)
;-----------------Vector-----------

(define <Vector>
  (new
    (*parser (char #\#))

    (*parser (char #\())

    (*parser (delay (lambda () <sexpr>)))
    *star
    (*parser (char #\)))
    (*caten 4)

    (*pack-with (lambda (a b chars c)
    ;(list->string chars)
          (list->vector chars)
    ))

    done))

;-----------------QuasiQuoted-----------
(define <QuasiQuoted>
  (new
    (*parser (char #\`))
    (*parser (delay (lambda () <sexpr>)))
    (*caten 2)

   (*pack-with (lambda (q chars)
        (list 'quasiquote chars)
    ))

    done))

;-----------------UnquoteAndSpliced-----------
(define <UnquoteAndSpliced>
    (new
    (*parser (char #\,))
    (*parser (char #\@))
    (*parser (delay (lambda () <sexpr>)))
    (*caten 3)

   (*pack-with (lambda (a b chars)
        (list 'unquote-splicing chars)
    ))
    done))

;---------------infixsymbol------------------
(define <InfixPrefixExtensionPrefix>
  (new
    (*parser (^<skippedInfix*> (word "##")))
    (*parser (^<skippedInfix*> (word "#%")))
    (*disj 2)
  done)
)


(define <infix-symbol-char>
  (new
    (*parser <symbol-char-no-calc>)
    (*parser (char #\+))
    (*parser (char #\-))
    (*parser (char #\*))
    (*parser (word "**"))
    (*parser (char #\^))
    (*parser (char #\/))
    (*disj 6)
    *diff
  done)
)

(define <infix-symbol>
  (new
    (*parser <infix-symbol-char>)
    *plus
     (*pack (lambda (sym)
          (string->symbol
            (list->string sym))))
  done)
)


(define <power-symbol>
  (new
    (*parser (char #\^))
    (*parser (word "**"))
    (*disj 2)
  done))

(define <infix-sexpr-escp>
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda(pref sexpr)
      sexpr))
  done)
)




;-------------------------infix-arg-list---------------------------------------


(define <infix-arg-list>
  (new
    ;(*parser <skip>) *star

    (*delayed (lambda () <InfixExpression>))
    (*parser <Space>)
                 *star
                (*caten 2)
                (*pack-with (lambda (a b) a))

                (*parser (^<skippedInfix*> (char #\,)))
    (*parser <Space>)
                 *star
                (*caten 2)
                (*pack-with (lambda (a b) a))
    (*delayed (lambda () <InfixExpression>))
    (*parser <Space>)
                 *star
                (*caten 2)
                (*pack-with (lambda (a b) a))
    (*caten 2)
    (*pack-with
      (lambda (comma iExp)
        iExp))
    *star

    (*caten 2)
    (*pack-with
      (lambda (firstArg restArg)
        `(,firstArg ,@restArg)))


    (*parser <epsilon>)
    ;(*parser <infix-symbol>)
    (*disj 2)


  done)
)
;-------------------------infix-paren---------------------------------------

(define <infix-paren>
  (new
    ;(*parser <skip>) *star
    ;(*parser(^<skipped*>) (char #\())

    (*parser (^<skippedInfix*> (char #\()))
    (*delayed (lambda () <InfixExpression>))
    (*parser (^<skippedInfix*> (char #\))))
    (*caten 3)
    (*pack-with
      (lambda (leftBracket iExp rightBracket)
        iExp))



  done)
)


;-------------------------InfixSub-add---------------------------------------

(define <InfixSub-add>
  (new




      (*parser (delay (lambda() <InfixMul-Div> )))
        (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
        (*parser (^<skippedInfix*> (char #\+)))
      (*pack (lambda (a) '+))
      (*parser (^<skippedInfix*> (char #\-)))
       (*pack (lambda (a) '-))
       (*disj 2)
       (*parser <Space>)
      *star
      (*caten 2)
      (*pack-with (lambda (a b) a))
      (*parser (delay (lambda() <InfixMul-Div> )))
      (*caten 3)
      (*pack-with (lambda (a b c)
            `(,b ,a, c )

        ))





        (*parser (^<skippedInfix*> (char #\+)))
      (*pack (lambda (a) '+))
      (*parser (^<skippedInfix*> (char #\-)))
        (*pack (lambda (a) '-))
        (*disj 2)
         (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
       (*parser (delay (lambda() <InfixMul-Div> )))
      (*caten 2)
      (*pack-with (lambda (a b)
       `(,a ,b )))

      *star

      (*caten 2)

;; here think about letrec , a and b are list just do it there
       (*pack-with (lambda (c d)
         ;`(* ,a,@b )
         ;(caar d ) )) *
         ; (cadar d ) )) 3
          (letrec ((final-res (lambda (a b)
           (if ( null?  b)
             a
             (final-res `(,(caar b),a ,(cadar b)) (cdr b))
            )
          )

           ))
          (final-res c d)
         )))


(*parser (delay (lambda() <InfixMul-Div> )))
(*disj 2)


done))

;-------------------------PowerSymbol---------------------------------------

;?PowerSymbol?::= ^ | **
(define <PowerSymbol>
    (new
      (*parser (^<skippedInfix*> (word-ci "^")))
        (*pack (lambda (_) '^))

        (*parser (^<skippedInfix*> (word-ci "**")))
        (*pack (lambda (_) '**))
        (*disj 2)
    done))


;-------------------------InfixNeg---------------------------------------
;;check spaces!!!
(define <InfixNeg>
  (new

    (*parser (^<skippedInfix*> (char #\-)))

   (*parser (delay (lambda()  <infix-sexpr-escp>)))
    (*caten 2)

    (*pack-with (lambda(a b)
      `(-,b)))

      (*parser (^<skippedInfix*> (char #\-)))

   (*parser (delay (lambda()  <InfixFuncall>)))
    (*caten 2)

    (*pack-with (lambda(a b)
      `(-,b)))

      (*parser (^<skippedInfix*> (char #\-)))

   (*parser (delay (lambda()  <InfixArrayGet>)))
    (*caten 2)

    (*pack-with (lambda(a b)
      `(-,b)))


      (*parser (^<skippedInfix*> (char #\-)))
 (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
  (*parser (delay (lambda() <Number>)))
    (*caten 2)

    (*pack-with (lambda(a b)
      `(-,b)))


      (*parser (^<skippedInfix*> (char #\-)))
 (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
   (*parser (delay (lambda()  <infix-symbol>)))
    (*caten 2)

    (*pack-with (lambda(a b)
      `(-,b)))

      (*parser (^<skippedInfix*> (char #\-)))
 (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
   (*parser (delay (lambda()  <symbol>)))
    (*caten 2)

    (*pack-with (lambda(a b)
      `(-,b)))


      (*parser (^<skippedInfix*> (char #\-)))

   (*parser (delay (lambda()  <infix-paren>)))
    (*caten 2)

    (*pack-with (lambda(a b)
          `(-,b)))

    (*parser (^<skippedInfix*> (char #\-)))
    (*parser <Space>)
    *star
    (*caten 2)
    (*pack-with (lambda (a b) a))
    (*parser (delay (lambda()  <InfixExpression>)))
    (*caten 2)

    (*pack-with (lambda(a b)
        `(-,b)))

   (*disj 8)
    done))
;-----------------------motherfucker-test------------------
(define <InfixPow>
    (new

     (*parser (delay (lambda()  <InfixArrayGet>)))

  ;----------------------
       (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
 ;----------------------
        (*parser <PowerSymbol>)
 ;----------------------
         (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
  ;----------------------
 (*parser (delay (lambda()  <InfixArrayGet>)))
;----------------------
        (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
   ;----------------------
        (*caten 3)
        (*pack-with
          (lambda (a b c)
            `(,a ,c)
            ))

 ;----------------------
       (*parser <PowerSymbol>)

                (*parser <Space>)
                 *star
                (*caten 2)
                (*pack-with (lambda (a b) a))
  ;----------------------

  (*parser (delay (lambda()  <InfixArrayGet>)))
 ;----------------------
                  (*parser <Space>)
                 *star
                (*caten 2)
                (*pack-with (lambda (a b) a))
  ;----------------------
        (*caten 2)
        (*pack-with
          (lambda (b c)
            ;`(expt  ,c)
            c
            ))
*star
(*caten 2)
 ;----------------------
       (*pack-with (lambda (c d)

        (letrec ((final-res (lambda (a)
           (if (null? (cddr a))
             `(expt ,(car a) ,(cadr a))
              `(expt ,(car a) ,(final-res (cdr a)))
          )
           )))
          (final-res `(,@c ,@d))
         )))
       ; (*pack-with (lambda(a b)
       ;  `(,@a ,@b)
       ;  ))
 ;----------------------
    (*parser (delay (lambda()  <InfixArrayGet>)))
    (*disj 2)
    done))
;-------------------------InfixArrayGet---------------------------------------

(define <InfixArrayGet>
  (new
    (*parser (delay (lambda()  <InfixFuncall>)))

    (*parser (^<skippedInfix*> (char #\[)))
    (*delayed (lambda () <InfixExpression>))
    (*parser (^<skippedInfix*> (char #\])))
    (*caten 3)
    (*pack-with
      (lambda(leftBracket iExp rightBracket)
        iExp))
    *star
    (*caten 2)
    (*pack-with
      (lambda(array elements)
        (fold-left (lambda (exp next)
                `(vector-ref ,exp ,next))

            array elements)))

    (*parser (delay (lambda()  <InfixFuncall>)))
       (*disj 2)

  done)
)
;-------------------------InfixFuncall---------------------------------------



(define <InfixFuncall>
  (new
        ;(*parser (delay (lambda() <numberWithoutRemaining>)))
    (*parser (delay (lambda() <NumberNotSymbol>)))
    (*parser (delay (lambda()  <infix-symbol>)))
    (*parser (delay (lambda()  <InfixNeg>)))
    (*parser (delay (lambda()  <infix-paren>)))

    (*disj 4)
    (*parser <Space>)
      *star
    (*caten 2)
    (*pack-with (lambda (a b) a))

    (*parser (^<skippedInfix*> (char #\()))

    (*parser <skip>) *star
    (*delayed (lambda () <infix-arg-list>))
    (*parser <skip>) *star
    (*caten 3)
    (*pack-with
      (lambda (lspace args rspace)
        args))

    (*parser (^<skippedInfix*> (char #\))))
    (*caten 3)
    (*pack-with
    (lambda (lBracket argsList rBracket)
        argsList))
    *plus ;has to have atleast one args

    (*caten 2)
    (*pack-with
      (lambda (parenthesis args)
        (fold-left (lambda (p a) `(,parenthesis ,@(car args)))
         parenthesis args)))

        ;(*parser (delay (lambda() <numberWithoutRemaining>)))
    (*parser (delay (lambda() <NumberNotSymbol>)))
    (*parser (delay (lambda()  <infix-symbol>)))
    (*parser (delay (lambda()  <InfixNeg>)))
    (*parser (delay (lambda()  <infix-paren>)))
    (*parser <infix-sexpr-escp>)
    (*disj 6)

  done)
)


;-----------------------------------------------


(define <symbol-char-infix>
  (new
    (*parser (range #\a #\z))
    (*parser (range #\A #\Z))
    (*disj 2)
  done))



;?Symbol?::=?SymbolChar? +
(define <symbol-infix-use>
  (new
    (*parser <symbol-char-infix>)
    *plus
    (*pack (lambda  (s)
      (let(( d (list->string s) ))
         (string->symbol `(,@d)))))
  done))

;--------------------------NumberNotSymbol-------------------------------------

(define <NumberNotSymbol>
  (new
   (*delayed (lambda() <Number>))
   (*delayed (lambda() <symbol-infix-use>))
   *not-followed-by
  done))



;-------------------------InfixMul-Div---------------------------------------

(define <InfixMul-Div>
  (new
    (*parser <InfixPow>)
    (*parser <Space>)
    *star
    (*caten 2)
    (*pack-with (lambda (a b) a))

    (*parser (^<skippedInfix*> (char #\*)))
    (*pack (lambda (a) '*))

    (*parser <Space>)
    *star
    (*caten 2)
    (*pack-with (lambda (a b) a))

    (*parser <InfixPow>)
    (*caten 3)
    (*pack-with (lambda (a b c)
      `(,b ,a, c )))

    (*parser <Space>)
    *star
    (*caten 2)
    (*pack-with (lambda (a b) a))

      (*parser <InfixPow>)
      (*parser <Space>) *star

      (*parser (^<skippedInfix*> (char #\/)))
      (*pack (lambda (a) '/))

      (*parser <Space>) *star
      (*parser <InfixPow>)

      (*caten 5)

      (*pack-with (lambda (a e b d c)
            `(,b ,a, c )))

      (*disj 2)

      (*parser <Space>) *plus
      (*parser (^<skippedInfix*> (char #\/)))
      (*pack (lambda (a) '/))

      (*parser <Space>) *plus

      (*parser <InfixPow>)

      (*caten 4)
      (*pack-with (lambda (d a c b)
          `(,a ,b )))

      (*parser <Space>)
      *star
      (*caten 2)
      (*pack-with (lambda (a b) a))

    (*parser (^<skippedInfix*> (char #\*)))
    (*pack (lambda (a) '*))
    (*parser <Space>)
    *star
    (*caten 2)
    (*pack-with (lambda (a b) a))
    (*parser <InfixPow>)
    (*caten 2)
    (*pack-with (lambda (a b)
          `(,a ,b )))

    (*parser <Space>)
    *star
    (*caten 2)
    (*pack-with (lambda (a b) a))

    (*disj 2)

      *star

      (*caten 2)

;; here think about letrec , a and b are list just do it there
       (*pack-with (lambda (c d)

          (letrec ((final-res (lambda (a b)
           (if ( null?  b)
             a
             (final-res `(,(caar b),a ,(cadar b)) (cdr b))
            )
          )

           ))
          (final-res c d)
         )))


     (*parser <InfixPow>)
     (*disj 2)

done))



;-------------------------InfixExpression---------------------------------------

 (define  <InfixExpression>
   (new
    (*parser <InfixSub-add>)
    (*parser <infix-paren>)
    (*disj 2)
   done))

;-------------------------InfixExtension---------------------------------------

(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
     (*parser <Space>)
        *star
        (*caten 2)
        (*pack-with (lambda (a b) a))
        (*parser <InfixExpression>)
        (*caten 2)

    (*pack-with (lambda (a b )
      b))
done))
;-------------------------sexpr---------------------------------------



(define <sexpr>
  (^<skipped*>
    (new
    (*parser <boolean>)
    (*parser <char>)
    (*parser <symbol>)
    (*parser <Number>)
    ;(*parser <NumberNotSymbol>)
    (*parser <String>)

    (*parser <improper-list>)
    (*parser <quoted>)
    (*parser <unquoted>)
    (*parser <ProperList>)
    (*parser <Vector>)
    (*parser <QuasiQuoted>)
    (*parser <UnquoteAndSpliced>)
    (*parser <InfixExtension>)


    (*disj 13)
    done)))



;;-------------------------------------------------------ASSIGNMENT 2-------------------

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define void-obj
  (if #f #f))


  (define var?
   (lambda (x)
   (and (symbol? x)
   (not (member x *reserved-words*)))))

  (define simple-const?
      (let ((preds (list boolean? char? string? number? )))
        (lambda (e)
         (ormap (lambda (pred)
          (pred e)
          ) preds))))

  (define identify-lambda
    (lambda (argl ret-simple ret-opt ret-var)
      (cond
        ((null? argl) (ret-simple '()))
        ((var? argl) (ret-var argl))
        (else (identify-lambda (cdr argl)
            (lambda (s) (ret-simple `(,(car argl) ,@s)))
            (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
            (lambda (var) (ret-opt `(,(car argl)) var))))))) ;;ret-var and not ret-opt???

  (define size>2?
    (lambda (l)
      (if (not (list? l))
        (#f)
        (if (null?  l)
          (#f)
          (if (null? (cdr l))
            #f
            #t)
        ))))

  (define beginify
    (lambda (s)
      (cond
        ((null? s) void-obj)
        ((null? (cdr s)) (cdr s))
        (else `(begin ,@s)))))

  (define flatten
    (lambda (lst)
    (cond ((null? lst) '())
          ((list? lst)
              (cond ((and (list? (car lst)) (eq? 'begin (caar lst)))
                       (append (flatten (cdar lst)) (flatten (cdr lst))))
                    ((eq? 'begin (car lst))
                       (append '() (flatten (cdr lst))))
                    ((or (null? (car lst)) (not (eq? 'begin (car lst))))
                        (cons (car lst) (flatten (cdr lst))))))
          (else ('(lst))))))

;;=======QQ EXPANSION MEIR CODE;;=====

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
     (eq? (car e) tag)
     (pair? (cdr e))
     (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
   (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
     simple-sexprs-predicates)
    (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
      (pair? e)
      (symbol? e)
      (vector? e))
  `',e
  e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
  (cadr e)
  e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
   (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
      (lambda (e)
        (cond ((unquote? e) (cadr e))
        ((unquote-splicing? e)
         (error 'expand-qq
           "unquote-splicing here makes no sense!"))
        ((pair? e)
         (let ((a (car e))
         (b (cdr e)))
           (cond ((unquote-splicing? a)
            `(append ,(cadr a) ,(expand-qq b)))
           ((unquote-splicing? b)
            `(cons ,(expand-qq a) ,(cadr b)))
           (else `(cons ,(expand-qq a) ,(expand-qq b))))))
        ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
        ((or (null? e) (symbol? e)) `',e)
        (else e))))
     (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
     (optimizer
      (compose-patterns
       (pattern-rule
        `(append ,(? 'e) '())
        (lambda (e) (optimize-qq-expansion e)))
       (pattern-rule
        `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
        (lambda (c1 c2 e)
    (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
          (e (optimize-qq-expansion e)))
      (optimize-qq-expansion `(append ,c ,e)))))
       (pattern-rule
        `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
        (lambda (c1 c2)
    (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
      c)))
       (pattern-rule
        `(append ,(? 'e1) ,(? 'e2))
        (lambda (e1 e2)
    (let ((e1 (optimize-qq-expansion e1))
          (e2 (optimize-qq-expansion e2)))
      `(append ,e1 ,e2))))
       (pattern-rule
        `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
        (lambda (c1 c2 e)
    (let ((c (quotify (list (unquotify c1) (unquotify c2))))
          (e (optimize-qq-expansion e)))
      (optimize-qq-expansion `(append ,c ,e)))))
       (pattern-rule
        `(cons ,(? 'e1) ,(? 'e2))
        (lambda (e1 e2)
    (let ((e1 (optimize-qq-expansion e1))
          (e2 (optimize-qq-expansion e2)))
      (if (and (const? e1) (const? e2))
          (quotify (cons (unquotify e1) (unquotify e2)))
          `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))

       ;;=======QQ EXPANSION MEIR CODE;;=====


(define parse
  (let ((run
      (compose-patterns
        ;;const
        (pattern-rule
          (? 'c simple-const?)
          (lambda (c) `(const ,c)))
        (pattern-rule
          `(quote ,(? 'c))
          (lambda (c) `(const ,c)))

        ;;var
        (pattern-rule
          (? 'v var?)
          (lambda (v) `(var ,v)))

        ;;if
        (pattern-rule
          `(if ,(? 'test) ,(? 'dit))
          (lambda (test dit) `(if3 ,(parse test) ,(parse dit) (const ,void-obj))))
        (pattern-rule
          `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
          (lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))

      ;;disjunction
      (pattern-rule
        `(or )
       (lambda () (parse #f) ))

      (pattern-rule
        `(or  ,(? 'expr) . ,(? 'exprs))
         (lambda (expr exprs)
          (if (null? exprs)
            (parse expr)
           `(or ,(map parse `(,expr ,@ exprs)))

          )))


       ;;lambdasss -------------------------
       (pattern-rule
          `(lambda ,(? 'variables) ,(? 'rest) . ,(? 'body))
          (lambda (variables rest body)
            ;(display `(begin ,rest ,@body))
              `(,@(identify-lambda
                  variables
                  (lambda (vars) `(lambda-simple ,vars))
                  (lambda (vars opt) `(lambda-opt ,vars ,opt))
                  (lambda (vars) `(lambda-var ,vars))) ,(parse `(begin ,rest ,@body)))))


        ;;and
        ;;no arguements -> true
        (pattern-rule `(and)
          (lambda () `(const #t)))
        ;;1 or more args
        (pattern-rule `(and ,(? 'first) . ,(? 'rest))
          (lambda (term rest)
            (if (null? rest) ;;only 1 args
              (parse term)
              (parse `(if ,term (and ,@rest) #f))))) ;;more than 1 arg

        ;;QQ
        ;quasi-quote
          (pattern-rule `(quasiquote ,(? 'q) . ,(? 'rest))
             (lambda (q rest)
                 (parse (expand-qq q))))

        ;cond
        ;only else
        (pattern-rule
          `(cond ,(? 'first (lambda (x) (and (not (null? (cdr x))) (equal? (car x) 'else))))) ;cdr x is expr1 and car x is 'else
          (lambda (first)
            (parse `(begin ,@(cdr first)))))

        ;;full cond
        (pattern-rule
          `(cond ,(? 'first) . ,(? 'rest))
          (lambda (first rest)
            (if (null? rest)
              (parse `(if ,(car first) (begin ,@(cdr first)) #f))
              (parse `(if ,(car first) (begin ,@(cdr first)) (cond ,@rest))))))

        ;;no args -> ERROR
        (pattern-rule `(cond)
          (lambda () (parse void-obj)))

          ;;define -------------------------

          ;;MIT style define

         (pattern-rule
           `(define ,(? 'vars list?) . ,(? 'expr))
           (lambda (vars expr)
           (parse `(define ,(car vars) ,`(lambda ,(cdr vars) (begin ,@expr))))
        ))

        (pattern-rule
        `(define (,(? 'var) . ,(? 'vars)) ,(? 'expr) . ,(? 'exprs))
        (lambda (var vars expr exprs)
      ;  (display `(lambda ,vars  ,expr ))
          (parse `(define ,var ,`(lambda ,vars  ,expr .,exprs)))
          ))

        (pattern-rule
          `(define ,(? 'var)  ,(? 'expr) . ,(? 'exprs))
              (lambda (var expr exprs)
                (if (null? exprs)
                `(def ,(parse var) ,(parse expr))
                `(def ,(parse var) ,(parse (beginify (cons expr exprs)))))))

        ;;beginnn

        (pattern-rule `(begin)
          (lambda () `(const ,void-obj)))
        (pattern-rule
          `(begin ,(? 'expr). ,(? 'exprs))
          (lambda (expr exprs)
            ;display (flatten `(,expr . ,exprs)))
            (if (null? exprs) ;;added this in order to prevent "seq" of only 1 expression
              (parse expr)
              `(seq ,(map parse  `(,expr . ,exprs))))))

          ;;let

        (pattern-rule
          `(let () ,(? 'expr) . ,(? 'exprs))
          (lambda (expr exprs)

           (parse `(,`(lambda () ,expr . ,exprs)))))

        (pattern-rule
          `(let ((,(? 'var var?) ,(? 'val)) . ,(? 'rest
            (lambda (rest) (map (lambda (x) (var? (car x))) rest)))) ,(? 'expr) . ,(? 'exprs))
          (lambda (var val rest expr exprs)
            (let*((rest-vars (map (lambda(x)(car x)) rest))
                (rest-vals (map (lambda(x)(cadr x)) rest))
                (vars `(,var ,@rest-vars))
                (vals `(,val ,@rest-vals))
                )
             (parse `(,`(lambda ,vars ,expr . ,exprs)
              ,@vals))
             )))


       ;let*

        (pattern-rule
          `(let* () ,(? 'expr) . ,(? 'exprs list?))
          (lambda (expr exprs)
            (parse `(,`(lambda () ,expr . ,exprs))) ))

        ;val? check if it should be here
        (pattern-rule
          `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
          (lambda (var val rest exprs)
             (if (null? rest)
               (parse `(let ((,var ,val)) . ,exprs))
              (parse `(let ((,var ,val)) (let* ,rest . ,exprs)))
              )
             ))


        ;;letrec
        (pattern-rule
          `(letrec () ,(? 'expr) . ,(? 'exprs list?))
          (lambda (expr exprs)
            (parse `(let () ,`(let () ,expr . ,exprs)) )))

        (pattern-rule
          `(letrec ((,(? 'var var?) ,(? 'val)) . ,(? 'rest
            (lambda (rest) (map (lambda (x) (var? (car x))) rest)))) ,(? 'expr) . ,(? 'exprs))
          (lambda (var val rest expr exprs)
            (let* ((rest-vars (map (lambda(x)(car x)) rest))
                (rest-vals (map (lambda(x)(cadr x)) rest))
                (vars `(,var  ,@rest-vars))
                (vals `(,val  ,@rest-vals))
                (vars-unsigend (map (lambda (x) `(,x #f)) vars))
                )
            (letrec (
                (vars-sitting (lambda (vars vals) (if (null? vars)
                  '()
               (cons (list 'set! (caar vars) (car vals)) (vars-sitting (cdr vars) (cdr vals))))))

                )
            ;(display (vars-sitting vars-unsigend vals))
            (if (null? exprs)
              (parse `(let ,vars-unsigend ,(beginify `( ,@(vars-sitting vars-unsigend vals)
                   (let () ,expr)))))

                  (parse `(let ,vars-unsigend ,(beginify `( ,@(vars-sitting vars-unsigend vals)
                   (let () ,(beginify (cons expr exprs)) )))))
                )))))

        ;;set

        (pattern-rule `(set! ,(? 'var var?) ,(? 'val))
          (lambda(var val)
            `(set ,(parse var) ,(parse val))))


            ;;application
          (pattern-rule `(,(? 'func) . ,(? 'appliedto))
            (lambda (func appliedto)
              `(applic ,(parse func) ,(map parse appliedto))))


)))
      (lambda (e)
        (run e
            (lambda ()
              (error 'parse
                  (format "I can't recognize this: ~s" e))))))) ;;end parse function



 ;; add the fixed compiler for hw2
(define *test-expr* '(define my-even? (lambda (e) (define even? (lambda (n) (or (zero? n) (odd? (- n 1))))) (define odd? (lambda (n) (and (positive? n) (even? (- n 1))))) (even? e))))
(define *parsed-test-expr* (parse *test-expr*))


(define (deep-map f l)
  (define (deep x)
    (cond ((null? x) x)
          ((pair? x) (map deep (f x)))
          (else x)))
  (map deep l))

(define (deep-filter f lst)
  (cond
    ((null? lst) '())
    ((and (atom? lst) (f lst)) (list lst))
    ((atom? lst) '())
    (else (append (deep-filter f (car lst))
                  (deep-filter f (cdr lst))))))

  (define lambda-simple?
    (lambda (expr)
        (if (list? expr)
            (equal? (car expr) 'lambda-simple)
            #f
        )))

  (define lambda-var?
    (lambda (expr)
        (if (list? expr)
            (equal? (car expr) 'lambda-var)
            #f
        )))

  (define lambda-opt?
    (lambda (expr)
        (if (list? expr)
            (equal? (car expr) 'lambda-opt)
            #f
        )))

  (define is-lambda?
    (lambda (expr)
        (or (lambda-simple? expr)
            (lambda-var? expr)
            (lambda-opt? expr)
        )))

(define get-lambda-body
    (lambda (expr)
        (if (is-lambda? expr)
            (let ((body (cond ((lambda-simple? expr) (caddr expr))
                              ((lambda-var? expr) (cadddr expr))
                              ((lambda-opt? expr) (caddr expr)))))
                  (display body)
                  )
            (expr)))) ;end if

(define split
  (lambda (pes ret-def+exprs)
      (if (null? pes)
            (ret-def+exprs '() '())
            (split (cdr pes)
                  (lambda (ds es)
                        (cond ((eq? (caar pes) 'def) (ret-def+exprs (cons (car pes) ds) es))
                              ((eq? (caar pes) 'seq) (split (cadar pes)
                                                        (lambda (dss ess) (ret-def+exprs (append dss ds) (append ess es)))))
                              ((not (pair? (car pes))) (ret-def+exprs ds (cons (car pes) es)))
                              ((null? (car pes)) (ret-def+exprs ds (cons (car pes) es)))
                              (else (ret-def+exprs ds (cons (car pes) es)))))))
                        ))

;return a list of variables that are defined in inner define
(define get-ds
    (lambda (expr)
        (split expr (lambda (x y) x))))

;return a list of body without inner defines
(define get-es
    (lambda (expr)
        (split expr (lambda (x y) y))))

(define blah
  (parse '(lambda (x . y) 1)))

(define eliminate-nested-defines2
  (lambda (pes)
      (display 'x)


))


(define counter-val 0) ;count times going into e-n-d function

(define eliminate-nested-defines
    (lambda (pes)
    ;(display counter-val)
          (set! counter-val (+ counter-val 1))
    ;      (display counter-val)
          (letrec ((make-rest-body (lambda (rest-body)
                                      `(,@(map eliminate-nested-defines rest-body))))
                  (make-set (lambda (ds es)
                                `(seq (,@(map (lambda (x) `(set ,(cadr x) ,(eliminate-nested-defines (caddr x)))) ds)
                                       ,@(make-rest-body es)))))
                   (attach-false (lambda (x)
                        (map (lambda (y) `(const #f)) x)))
                   (check-first (lambda (expr)
                        (cond ((and (eq? (caar expr) 'var) (list? (car expr))) `(,(car expr) ,(check-first (cdr expr))))
                              (else (map eliminate-nested-defines (car expr)))
                        )
                      ))
                   (make-applic (lambda (body)
                          (let ((innerVars (map cadadr (get-ds body))))
                                  ;(display innerVars)
                                  (if (not (null? innerVars))
                                     `((applic (lambda-simple ,innerVars ,(make-set (get-ds body) (get-es body))) ,(attach-false (get-ds body))))
                                      (map eliminate-nested-defines body)) ;no inner define vars so get in deeper
                                  ))))
          (cond ;((and (= counter-val 1) (lambda-simple? pes) (eq? (caaddr pes) 'seq)) `(lambda-simple ,(cadr pes) ,(caddr pes) ,(map eliminate-nested-defines (cdaddr pes)))) ;179,173
                ;((and (= counter-val 1) (eq? (car pes) 'applic)) `(applic ,@(check-first (cdr pes))))
                ((lambda-simple? pes) `(lambda-simple ,(cadr pes) ,@(make-applic (cddr pes))))
                ((lambda-var? pes) `(lambda-var ,(cadr pes) ,@(make-applic (cddr pes))))
                ((lambda-opt? pes) `(lambda-opt ,(cadr pes) ,(caddr pes) ,@(make-applic (cdddr pes))))
                ((eq? (car pes) 'def) `(def ,(cadr pes) ,(eliminate-nested-defines (caddr pes))))
                ((eq? (car pes) 'seq) `(seq ,(map eliminate-nested-defines (cadr pes))))
                ((eq? (car pes) 'or) `(or ,(map eliminate-nested-defines (cadr pes))))
                ;((eq? (car pes) 'set) `(set ,(cadr pes) ,(eliminate-nested-defines (caddr pes))))
                ;((and (eq? (car pes) 'applic) (eq? (cadr pes) 'lambda-opt)) `(lambda-opt ,(caddr pes) ,(cadddr pes) ,@(make-applic (cddddr pes))))
                ;((eq? (car pes) 'applic) `(applic ,@(make-applic (cdr pes))))
                ;((list? (car pes)) `(,(car pes) ,(map eliminate-nested-defines (cadr pes))))
                (else pes)))))



(define redunant-applic?
    (lambda (expr)
        (and (list? expr)
             (not (null? expr))
             (equal? (car expr) 'applic)
             (is-lambda? (list (caar (cdr expr))))
             (equal? (car (cdr (car (cdr expr)))) '())
             )))

(define remove-redundancy
    (lambda(expr)
        (if (redunant-applic? expr)
            (car (cdr (cdr (car (cdr expr)))))
            expr)))

(define remove-applic-lambda-nil
    (lambda (lst)
        (letrec ((nest-map-if-list
                    (lambda (expr func)
                        (if (list? expr)
                            (func (map (lambda (exp) (nest-map-if-list exp func)) expr))
                            (func expr)))))
              (set! counter-val 0)
              (nest-map-if-list lst remove-redundancy))))


;; check what happen with variadic lambda .....!!!!!!!!!!!!
(define box-set (lambda (exp)
   (cond
    ; ((equal? (car exp) 'set)
    ;       `(box-set ,(cadr exp) ,(box-set (caddr exp)))
    ;       )
        ((or (equal? (car exp) 'lambda-simple) (equal? (car exp) 'lambda-opt) (equal? (car exp) 'lambda-var) )

          (let ((original-vars `(,(cadr exp)))
            (vars (cadr exp))
               (sitting-vars '())
               (body (caddr exp))
            )
          (if (equal? (car exp) 'lambda-opt)
            ;(display body)
            (begin (set! vars `(,@(cadr exp) ,(caddr exp))) `(set! sitting-vars ,(caddr exp))
              (set! body  (cadddr exp))
             (set! original-vars `(,(cadr exp) ,(caddr exp))) )
            )
         ; (display `(,(car exp) ,@original-vars ,body) )
          ; (display "\n")
          ;;applic in
          ; (if (and (list? body) (equal? 'applic (car body)))
          ; (set! body (box-set body))
          ; )

          (if (not (list? vars)) (set! vars (list vars)))
          ; (display 'vars)
          ; (display vars)
          ; (display "\n")
          ;;looking for set occurence in the exp
          (set! vars (deep-filter (lambda (x) (check-set x body))  vars))
          ;;looking for bound occurence vars
          (set! vars (deep-filter (lambda (x) (check-bound x body))  vars))
          ;;looking for get occurence
          (set! vars (deep-filter (lambda (x) (check-get x body))  vars))


            (if (not(null? vars))
              (set! sitting-vars (map (lambda(var)
                `(set ,(parse var) (box ,(parse var)))) (flatten vars)))
              )



            ;   (let ((manipulated-set-exp `(,(car exp) ,@original-vars ,(deep-map
            ; ;(lambda (x) (if (and (member x sitting-vars) (list? x)) (box-set x) x) ) (deep-map-get-box vars body)))
            ; ;(lambda (x) (if  (and (list? x) (not (null? vars))) (set-in-lambda x vars) x) ) (deep-map-get-box vars body)))
            ; (lambda (x)

            ;  (if (list? x)
            ;   (if (check-if-it-lambda x)
            ;     (let ((new-vars (deep-filter (lambda(var) (not (member var (cadr x)))) vars)))
            ;   (set-in-lambda x new-vars)
            ;   )
            ;   (set-in-lambda x vars))
            ;     x) )
            ; (deep-map-get-box vars body)))
            ;   )
            ;   )
             (let ((manipulated-set-exp `(,(car exp) ,@original-vars ,(deep-map-set-shit
           set-in-lambda
            (deep-map-get-box vars body) vars))
              )
              )

            (let ((body (caddr manipulated-set-exp)))
                (if (equal? (car exp) 'lambda-opt)
                  (set! body (cadddr manipulated-set-exp))
                )

             (if (null? vars)

              `(,(car manipulated-set-exp) ,@original-vars ,(deep-map
            ;(lambda (x) (if (and (member x sitting-vars) (list? x)) (box-set x) x) ) (deep-map-get-box vars body)))
            (lambda (x) (if (list? x) (box-set x) x) ) body))


                  ;;with seq
                  (let ((body (deep-map
            (lambda (x) (if (list? x) (box-set x) x) ) body)
                  ))
;                   (display 'sitting-vars)
;                  (display sitting-vars)
; (display "\n")
;                   (display '-vars)
;                   (display 'original-vars)
;                   (display original-vars)
;                   (display "\n")
;                   (display 'ssssssssss)
;                  ; (display `(,(car manipulated-set-exp) ,(cadr manipulated-set-exp) (seq ,sitting-vars  ,body)))
;                   (display '-------------------)
              (if (check-seq-in-body body)
              ;;with seq
          `(,(car manipulated-set-exp) ,@original-vars (seq (,@sitting-vars ,@(cadr body))))

          `(,(car manipulated-set-exp) ,@original-vars (seq  (,@sitting-vars  ,body)))

            )
          ;     ;;with seq
          ; `(,(car exp) ,(cadr exp) (seq ,sitting-vars ,(deep-map
          ;   (lambda (x) (if (list? x) (box-set x) x) ) (deep-map-get-box vars body))))
          )
          )
             )
          )



                        ))
        ((equal? (car exp) 'applic)
          ; (display 'applic-box-set)
          ; (display (cadr exp))
          ; (display "\n")
         `(,(car exp) ,(box-set (cadr exp)),@(cddr exp)))
        (else exp))))


;; Fuck this shit !!!!!!!!!!!!!!!!!!!!!!!!!!


  (define (deep-map-set-shit f l vars)
  (define (deep x vars)
    (cond ((null? x) x)
          ((and (list? x) (check-if-it-lambda x))

            (let ((new-vars (deep-filter (lambda(var) (not (member var (cadr x)))) vars))
              (original-vars `(,(cadr x)))
               (body (caddr x))
            )
          (if (equal? (car x) 'lambda-opt)
            ;(display body)
            (begin
              (set! body  (cadddr x))
             (set! original-vars `(,(cadr x) ,(caddr x))) )
            )
          (if (null? (cadr x)) (display body))
              `(,(car x) ,@original-vars ,(deep body new-vars))
              ))
          ((and (list? x) (equal? (car x) 'set)) (f x vars))
          ((pair? x) (map (lambda (z) (deep (f z vars) vars)) x))
          (else x)))
  (map (lambda (x) (deep x vars)) l))


(define check-if-it-lambda (lambda(exp)

              (if (or (equal? (car exp) 'lambda-simple) (equal? (car exp) 'lambda-opt) (equal? (car exp) 'lambda-var) )
                #t
                #f
              )))

(define check-seq-in-body (lambda(body)
  (if (and (list? body) (equal? 'seq (car body)))
      #t
      #f
      )))

(define set-in-lambda (lambda (exp vars)
   (cond ((and (list? exp)(equal? (car exp) 'set))
          (if (member (cadr(cadr exp)) vars)
          `(box-set ,(cadr exp) ,(box-set (caddr exp)))
          exp
          ))
   (else exp))))

(define test (lambda(exp)
  (deep-map (lambda (x) (if (list? x) (box-set x) x) )  exp)))


(define (deep-map-get-box vars l)
  (define (deep x vars)
    (cond ((null? x) x)
      ((and (list? x) (check-if-it-lambda x))
            (let ((new-vars (deep-filter (lambda(var) (not (member var (cadr x)))) vars))
              (original-vars `(,(cadr x)))
               (body (caddr x))
            )
          (if (equal? (car x) 'lambda-opt)
            ;(display body)
            (begin
              (set! body  (cadddr x))
             (set! original-vars `(,(cadr x) ,(caddr x))) )
            )
              `(,(car x) ,@original-vars ,(deep body new-vars))
              ))
          ((and (list? x) (or (equal? (car x) 'set) (equal? (car x) 'box-set))) `(,(car x) ,(cadr x)
          ,@(map (lambda (z) (deep z vars)) (cddr x))))
          ((and (list? x) (equal? (car x) 'var) (member (cadr x) vars)) `(box-get ,x))
          ((list? x) (map (lambda (z) (deep z vars)) x))

          (else  x )))
  ( deep l vars))

(define check-set (lambda(var exp)
  (let ((found #f))

  (define (deep x)
    (cond ((null? x) x)
          ((and (list? x) (equal? (car x) 'set)) (if (equal? var (cadadr x))
            (set! found #t)) (map deep x))
          ((list? x) (map deep x))
          (else x)))
  ( deep exp)

     ; (display 'check-set)
     ; (display var)
     ;  (display found)
     ;  (display "\n")

found

)

  ))
;; got body of lambda only
(define check-bound (lambda( var exp )
  (let ((found #f))

  (define (deep x)
    (cond ((null? x) x)
      ((and (list? x) (check-if-it-lambda x) (member var (cadr x))) x)
          ((and (not found) (list? x) (or (equal? (car x) 'lambda-simple) (equal? (car x) 'lambda-opt) (equal? (car x) 'lambda-var) )
) (set! found (check-var-in-lambda var x))
             (map deep x))

          ((list? x) (map deep x))
          (else x))
    )
  (deep exp)

; (display 'check-bound )
;      (display var)
;       (display found)
;       (display "\n")
found

)

  ))
(define check-var-in-lambda (lambda(var exp)
  (let ((found #f))
    (define (deep x)
    (cond ((null? x) x)
          ;((and (list? x) (equal? (car x) 'set)) (if (list? (caddr x)) (map deep (caddr x))))
          ((and (list? x)(equal? 'var (car x))) (if (equal? var (cadr x)) (set! found #t)))

          ((list? x) (map deep x))
          (else x)))

     (if (not (member var (cadr exp)))
    (deep exp)
    #f
    )

found
)))

(define check-get (lambda(var exp)
  (let ((found #f))
    (define (deep x)
    (cond ((null? x) x)

          ((and (list? x)(equal? 'var (car x))) (if (equal? var (cadr x)) (set! found #t)))
           ((and (list? x) (equal? (car x) 'set) (equal? 'var (car (caddr x)))) (if (list? (caddr x)) (deep (caddr x))))
           ((and (list? x) (equal? (car x) 'set)) (if (list? (caddr x)) (map deep (caddr x))))


          ((list? x) (map deep x))
          (else x)))

    ; (if (not (member var (cadr exp)))
    (deep exp)

    ; (display 'check-get )
   ;   (display var)
   ;    (display found)
   ;    (display "\n")
found
)))



(define *example*
'(let ((a 0))
(list
(lambda () a)
(lambda () (set! a (+ a 1)))
(lambda (b) (list (set! b 1) (set! a b))))))

(define get-occurrences
  (lambda (x lst)
    (cond
      ((memq x lst) => (lambda (lst)
                          (+ (get-occurrences x (cdr lst)) 1)))
      (else 0))))

(define test-scope '((1 2 3) (4 5)))

(define is-scope-member?
    (lambda (element lst)
          (if (list? (car lst))
              (let ((result (map (lambda (x) (get-occurrences element x)) lst)))
                    (if (member 1 result)
                        #t
                        #f))
              (member element lst)
)))


(define check-bound-var
    (lambda (var scope major minor)
          (if (member var (car scope))
              `(bvar ,var ,major ,(- (length (car scope)) (length (member var (car scope)))))
               (check-bound-var var (cdr scope) (+ 1 major) 0)
          )
))

(define check-var
    (lambda (var para scope)
    ;(and (null? para) (null? scope)) happens if there is no lambda and we didn't fill in the para list or scope list yet
          (cond ((or (and (null? para) (null? scope)) (and (not (member var para)) (not (is-scope-member? var scope))))
                      `(fvar ,var)) ;free var
                ((member var para) `(pvar ,var ,(- (length para) (length (member var para))))) ;parameter
                (else (check-bound-var var scope 0 0)) ;bound variable
)))


(define traverse
    (lambda (expr paraList scope)
        (cond ((null? expr) expr)
              ((lambda-simple? expr) `(lambda-simple ,(cadr expr) ,(traverse (caddr expr) (cadr expr) (cons paraList scope))))
              ((lambda-var? expr) `(lambda-var ,(cadr expr) ,(traverse (caddr expr) (list (cadr expr)) (cons paraList scope))))
              ((lambda-opt? expr) `(lambda-opt ,(cadr expr) ,(caddr expr) ,(traverse (cadddr expr) (append (cadr expr) (list (caddr expr))) (cons paraList scope))))
              ((list? (car expr)) (map (lambda (x) (traverse x paraList scope)) expr))
              ;((eq? (car expr) 'var) (cons (check-var (cadr expr) paraList scope) (traverse (cdr expr) paraList scope)))
              ((eq? (car expr) 'var) (check-var (cadr expr) paraList scope))
              ((not (list? expr)) expr)
              ;((eq? (car expr) 'def) `(def ,(cons (flatten (check-var (cdadr expr) paraList scope)) (traverse (cddr expr) paraList scope))))
              (else (cons (car expr) (traverse (cdr expr) paraList scope)))
    )))

(define pe->lex-pe
      (lambda (pes)
          (traverse pes '() '())
      ))

(define niv
    (parse '(lambda (a b) (a)))
)

(define niv2
    (parse '(lambda (a b) (lambda (c d) (lambda (e f) a))))
)

(define niv3
    (parse '(lambda (a b) (d)))
)

(define niv4 (parse '(define fact (lambda (n) (if (zero? n) 1 0)))))

(define niv5 (parse '(define n 5)))




(define (replace-item new old alist)
  (cond ((null? alist) '())
        ((equal? (car alist) old) (replace-item  new old (cons new (cdr alist))))
        (else (cons (car alist) (replace-item new old (cdr alist))))))

(define (replace-for-deep new old )
  (lambda(alist)
    (replace-item new old alist)
    ))

(define (deep-map-annotate f l)
  (define (deep x)
    (cond ((null? x) x)
          ((pair? x) (f x) (map deep x))
          (else  x)))
  (map deep l))

;; check what happen with variadic lambda .....!!!!!!!!!!!!
(define annotate-tc (lambda (exp)

(if (pair? exp)
  (cond
    ((or (equal? (car exp) 'lambda-simple) (equal? (car exp) 'lambda-opt) (equal? (car exp) 'lambda-var) )
      (let ((body (caddr exp))
            (vars `(,(cadr exp)))
        )
      (if (equal? (car exp) 'lambda-opt)
        (begin (set! vars `(,(cadr exp) ,(caddr exp))) (set! body (cadddr exp)))
        )
          `(,(car exp) ,@vars ,(annotate-tc-helper body))


        ))
    ((equal? (car exp) 'def)
      (let ((defined (caddr exp)))

          `(,(car exp) ,(cadr exp) ,(annotate-tc defined))
          ))

      ((or (equal? (car exp) 'or) (equal? (car exp) 'seq))

      `(,(car exp) ,(map (lambda(x) (annotate-tc x)) (cadr exp)))
      )

      ((equal? (car exp) 'applic)
        ;`(,(car exp) ,(annotate-tc (cadr exp)),(map (lambda(x) (annotate-tc x)) (caddr exp)))
        `(,(car exp) ,@(deep-map (lambda(x) (annotate-tc x)) (cdr exp)))

        )
        (else exp))
  exp
  )))

;; assume you got body of lambda

(define annotate-tc-helper (lambda (exp)
  (cond
  ((check-if-it-lambda exp)

      (annotate-tc exp))

    ((equal? (car exp) 'if3)
      `(,(car exp) ,(cadr exp) ,(annotate-tc-helper (caddr exp)) ,(annotate-tc-helper (cadddr exp))))

    ((or (equal? (car exp) 'or) (equal? (car exp) 'seq))
      ;(display (car (reverse (car (reverse exp)))))

      `(,(car exp) ,(reverse `(,(annotate-tc-helper (car (reverse (car (reverse exp))))) ,@(cdr (reverse (car (reverse exp)))))))
      )

    ((equal? (car exp) 'applic)
      `(tc-applic ,@(deep-map (lambda(x) (annotate-tc x)) (cdr exp))))
    (else exp))
  ))


(define full-cycle (lambda(exp)
  (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse exp))))))))

;;;;;;;;;;;;;;; final project


;defining constant variables
(define constList (list void-obj '() '#t '#f))
(define parsedConstList '())
(define constTable '())



(define topo-sort
      (lambda (expr)
            (cond ((or (number? expr) (string? expr) (eq? expr void-obj) (boolean? expr) (char? expr) (null? expr)) `(,expr))

                  ((pair? expr) `(,expr ,@(topo-sort (car expr)) ,@(topo-sort (cdr expr))))

                  ((vector? expr) `( ,expr ,@(apply append (map topo-sort (vector->list expr)))))

                  ((symbol? expr) `(,expr ,@(topo-sort (symbol->string expr))))

                  (else 'sorting-exception)
      )))

      (define (qsort e)
        (if (or (null? e) (<= (length e) 1)) e
            (let loop ((left '()) (right '())
                         (pivot (car e)) (rest (cdr e)))
                  (if (null? rest)
                      (append (append (qsort left) (list pivot)) (qsort right))
                     (cond ((not (list? (car rest))) (loop (append left (list (car rest))) right pivot (cdr rest)))
                           ((or (< (length (car rest))) (= (length (car rest)))) (loop (append left (list (car rest))) right pivot (cdr rest)))
                           (else (loop left (append right (list (car rest))) pivot (cdr rest))))))))


(define remove-dupli
      (lambda (origList newList)
          ;(display origList)
          ;(display '---------)
          ;(display newList)
          ;(display '---------)
            (cond ((null? origList) newList)
                  ((equal? '() newList) (remove-dupli (cdr origList) (list (car origList))))
                  ;((and (symbol? (car origList)) (member (symbol->string (car origList)) newList)) (remove-dupli (cdr origList) newList)) ;remove rewrite of string into symbols that for some annoying reason it does in topo-sort
                  ((member (car origList) newList) (remove-dupli (cdr origList) newList))
                  (else (remove-dupli (cdr origList) (append newList (list (car origList)))))
      )))

;(define order-parsed-const-list
;      (lambda (parsedLst oldLst newLst)
;            (cond ((null? oldLst) newLst)
;                  ((= 1 (length (car oldLst))) (order-parsed-const-list parsedLst (cdr oldLst) (append newLst (car oldLst))))
;                  (())
;
;      ))


(define ra-sort
  (lambda (numList)
    (begin
    (let
        ((elem-length-list (map (lambda (x) (length x)) numList)))
       ; (display elem-length-list)
          (cond
                ((null? numList) '())
                ((and (list? (car numList)) (= (car (length numList)) (apply max (map (lambda (x) (length x)) numList))))
                    (cons (car numList) (ra-sort (cdr numList))))
                (else (ra-sort (append (cdr numList) (list (car numList)))))))
      )))

  ;(define ra-sort
  ;  (lambda (numList)
  ;    (cond
  ;      ((null? numList) '())
  ;      ((and (list? (car numList)) (= (car (length numList)) (apply max (map (lambda (x) (length x)) numList))))
  ;          (cons (car numList) (ra-sort (cdr numList))))
  ;      (else (ra-sort (append (list-tail numList (- (length numList) 1))
  ;                             (reverse (list-tail (reverse numList) 1)))))
  ;      )))

;(define get-constants
;      (lambda (pes)
;          (if (or (null? pes) (not (pair? pes)))
;              parsedConstList
;              (if (eq? (car pes) 'const)
;                  (if (not (memq (cadr pes) constList))
;                      (begin (set! parsedConstList (append parsedConstList (cdr pes))) (get-constants (cdr pes)))
;                      (get-constants (cdr pes)))
;                  (begin (get-constants (car pes)) (get-constants (cdr pes)))))))


(define get-constants
      (lambda (pes)
          (if (or (null? pes) (not (pair? pes)))
              '()
              (if (eq? (car pes) 'const)
                  (reverse (topo-sort (cadr pes)))
                  `(,@(get-constants (car pes)) ,@(get-constants (cdr pes)))))))


;put into parsedConstList the finalized constants list of the parsed expression
(define set-constants
    (lambda (pes)
      (begin
            (set! parsedConstList '()) ;reset constants list
            (set! parsedConstList (get-constants pes))

            ;(set! parsedConstList (append constList parsedConstList)) ;add the parsed constant list to the default const list
            ;(set! parsedConstList (remove-dupli (append constList (reverse (topo-sort parsedConstList))) '())) ;remove any dups
             (set! parsedConstList (remove-dupli parsedConstList '()))

            (set! parsedConstList (reverse (topo-sort parsedConstList)))
          ; (display parsedConstList)
          ;     (display   nl)
          ;     (display   nl)
            (set! parsedConstList (remove-dupli parsedConstList '()))

            ;(set! parsedConstList (map (lambda (x) (if (list? x)
            ;                                              (remove-dupli x '())
            ;                                              x)) parsedConstList))
            (set! parsedConstList (remove-dupli parsedConstList '()))

              ; (display parsedConstList)
              ; (display   nl)

)))

;get nth element in list
(define nth
      (lambda (list n)
            (if (eq? n 1)
                (car list)
                (nth (cdr list) (- n 1))
        )))

;find memory pointer in const table for element
(define find-in-table
      (lambda (element lst n)

        (if (or (null? lst) (= (length parsedConstList) 4) )
              #f
              (if (or (equal? (nth (car lst) n) element))
                  (car lst)
                  (find-in-table element (cdr lst) n))
          )))

;find memory pointer in const table for element
(define find-in-table-cdr
      (lambda (element lst n)
      ;(display (newline))
      ;(display 'lst:)
      ;(display (cadar lst))
      ;(display 'elem)
      ;(display element)
      ;(display (newline))
        (if (or (null? lst) (= (length parsedConstList) 4) )
              #f
              (if (equal? (cadar lst) element)
                  (car lst)
                  (find-in-table-cdr element (cdr lst) n))
          )))

;;; changed!!!
(define find-last-sym-addr
    (lambda (const-table)
      (if (equal? const-table '())
          #f ;no previous symbols in table
          (let ((type (nth (nth (car const-table) 3) 1)))
            (if (equal? type '\T_SYMBOL)
                (caar const-table)
                (find-last-sym-addr (cdr const-table)))))))

(define find-in-table-sym
      (lambda (element lst)
          (let ((addr (caar lst)))
                (if (member element (car lst))
                    addr
                    (find-in-table-sym element (cdr lst)))
          )))


(define get-first-sym-addr
  (lambda (const-table)
    (if (null? const-table)
        -1
        (let ((type (nth (nth (car const-table) 3) 1)))
          (if (equal? type '\T_SYMBOL)
              (caar const-table)
              (get-first-sym-addr (cdr const-table)))))))

;; here it should starts from the adress we choose not 0
;insert into const table the first default values
(define create-default-const-table
      (set! constTable `((300 ,(car constList) (\T_VOID))
                        (301 ,(cadr constList) (\T_NIL))
                        (302 ,(caddr constList) (\T_BOOL 1))
                        (304 ,(cadddr constList) (\T_BOOL 0)))
      ))

(define create-constant-table
      (lambda (const-lst address sym-addr)
            (if (null? const-lst)
                constTable
                  (let ((cur-element (car const-lst))
                        (next-element (cdr const-lst)))

                        (begin
                        ;(display '--CurElementttttttt:       )
                        ;(display cur-element)
                        ;(display '--pair:::       )
                        ;(display (pair? cur-element))
                        ;;(display '--next-element:      )
                        ;(display next-element)
                    (cond
                        ((pair? cur-element)
                         ;   (display constTable)
                         ;    (display   nl)
                         ;  (display   (car cur-element))
                         ;  (display   nl)
                         ; (display  (find-in-table (car cur-element) constTable 2))
                         ; (display   nl)
                            (let ((car-addr (car (find-in-table (car cur-element) constTable 2)))
                                  (cdr-addr (car (find-in-table-cdr (cdr cur-element) constTable 2))))
                                    (begin
                                            ; (display '---cdrrrrrrrrrrrrr: )
                                            ; (display cur-element)
                                            ; (display nl)
                                            ; (display `((,address ,cur-element (\T_PAIR ,car-addr ,cdr-addr))))
                                            ; (display nl)
                                            (set! constTable (append constTable `((,address ,cur-element (\T_PAIR ,car-addr ,cdr-addr))))) ;exists in the table
                                            (create-constant-table next-element (+ address 3) sym-addr))
                                     ))

                       ((integer? cur-element)
                             (begin (set! constTable (append constTable `((,address ,cur-element (\T_INTEGER ,cur-element)))))
                                    (create-constant-table next-element (+ address 2) sym-addr)
                             )
                         )
                       ((rational? cur-element)
                             (begin (set! constTable (append constTable `((,address ,cur-element (\T_FRACTION ,(numerator cur-element) ,(denominator cur-element))))))
                                    (create-constant-table next-element (+ address 3) sym-addr)
                             )
                         )
                        ((string? cur-element)
                            (let ((ascii (map char->integer (string->list cur-element))))
                              (begin  (set! constTable (append constTable `((,address ,cur-element (\T_STRING ,(string-length cur-element) ,@ascii)))))
                                     (create-constant-table next-element (+ address (+ (string-length cur-element) 2)) sym-addr))))

                        ((char? cur-element)
                              (begin (set! constTable (append constTable `((,address ,cur-element (\T_CHAR ,(char->integer cur-element))))))
                                     (create-constant-table next-element (+ address 2) sym-addr)
                              )
                          )

                        ((vector? cur-element)
                        
                              (let ((elements (map (lambda (elem) (car (find-in-table elem constTable 2))) (vector->list cur-element))))
                                (begin  (set! constTable (append constTable `((,address ,cur-element (\T_VECTOR ,(length elements) ,@elements)))))
                                       (create-constant-table next-element (+ address (+ (length elements) 2)) sym-addr))))

                        ((symbol? cur-element)

                              (let ((str-addr (find-in-table-sym (symbol->string cur-element) constTable))
                                    (last-sym-addr (find-last-sym-addr (reverse constTable))))

                                    (if (not (eq? last-sym-addr #f))
                                        (begin (set! constTable (append constTable `((,address ,cur-element (\T_SYMBOL ,str-addr ,last-sym-addr)))))
                                                (create-constant-table next-element (+ address 3) sym-addr))
                                        (begin (set! constTable (append constTable `((,address ,cur-element (\T_SYMBOL ,str-addr ,(get-first-sym-addr constTable)))))) ;first symbol
                                                (create-constant-table next-element (+ address 3) sym-addr)))))

                        (else (create-constant-table next-element address sym-addr)))
                    )))))



;(define *example* '(let ((a 0)) (list (lambda () a) (lambda () (set! a (+ a 1))) (lambda (b) (set! a b)))))
;(define parsed-example (parse *example*))
(define ex2 (parse '(+ 1 2))) ;test char
;(define ex2 (parse '(if #t 'abc))) ;test string
;(define ex2 (parse '('#() 7))) ;test vector
;(define ex2 (parse '(list (1 2 3 4)))) ;test pair
;(define ex2 (parse '(#f 1 '() 'fuck 'my 'life '(2) 2 list(1 2 3 #\a) '#() ))) ;test fb expre

;(set-constants ex2) ;this creates the parsedConstList
;(create-constant-table parsedConstList 306 0)
;; sep bloody strings with motherfucker comma
(define sep-with-comma
  (lambda (list)
    (fold-left (lambda (a b) (string-append a ", " b))`,(car list) (cdr list))))

;; turn list elements to strings
(define list->strings-list
  (lambda (lst)
    (map (lambda (elem)
           (cond ((symbol? elem) (symbol->string elem))
                 ((number? elem) (number->string elem)))) lst)))

(define constTable->consts-string-arr
  (lambda (consts)
    (sep-with-comma (list->strings-list (apply append (map caddr consts))))))

(define get-consts-length (lambda()
(length (list->strings-list (apply append (map caddr constTable))))
  ))

;;; check if the length is right !!!!!!!!!!!!!
(define alloc-const-table (lambda()
  (let*((length (length constTable))

    )
    (string-append
      "NOP" nl
      "long const_mem["(number->string (get-consts-length ))"]={" (constTable->consts-string-arr constTable)"};" nl
      "memcpy((void*) &IND(300),(void*) &const_mem,sizeof(const_mem));" nl

      )

  )))


;;; check if the length is right !!!!!!!!!!!!!
(define alloc-global-table (lambda()
  (let*((length (+ 300 (get-consts-length ) (length Global-table) 100 ))
    )
    (string-append
      ;;"NOP" nl
      ;;"long global_mem["(number->string (get-consts-length ))"]={" (constTable->consts-string-arr constTable)"};" nl
      ;;"memcpy((void*) &IND("(number->string (+ (get-consts-length ) 350))"),(void*) &global_mem,sizeof(global_mem));" nl
      "MOV(IND(0),"(number->string length)");" nl
      )

  )))



(define (deep-map f l)
  (define (deep x)
    (cond ((null? x) x)
          ((list? x) (map deep (f x)))
          (else x)))
  (map deep l))

(define check-free-var (lambda(exp)
	(if(and (list? exp) (equal? (car exp) 'fvar))
		#t
		#f)))

(define (append-element lst elem)
  (append lst (list elem)))
;; returns fvars in pe might be duplicate
(define get-fvars (lambda(exp)
	(let* ((fvar-list '())
		(add-var (lambda(sub-exp)
  	(if (check-free-var sub-exp)
  		(set! fvar-list (append-element fvar-list (cdr sub-exp)))
          ) sub-exp)))
	(add-var  exp)
  (deep-map add-var  exp)
;;(display fvar-list)
fvar-list
  )))

(define (remove-dup e)
  (if (null? e) '()
      (cons (car e) (remove-dup (filter (lambda (x) (not (equal? x (car e))))
                                    (cdr e))))))
(define Global-table '())
;;returns list with the symbol and addr no duplicate
(define create-global-table (lambda(fvars-list addr Global-table)
;(display addr)
   (if (null? fvars-list)
   	   (reverse! Global-table)
   	   (create-global-table (cdr fvars-list) (+ addr 1) (cons  `(,@(car fvars-list) ,addr ) Global-table))
   	   )))
;;get address of var in global table , should change if null lst
(define find-in-global-table (lambda(var lst)

  (if (null? lst)
  #f
      (if (and (not (null? lst)) (equal? var (car (car lst))))
        (cadr (car lst) )
        (find-in-global-table  var (cdr lst))
        )
      )
  ))

;;Mayer's code
(define file->string
(lambda (in-file)
(let ((in-port (open-input-file in-file)))
(letrec ((run
(lambda ()
(let ((ch (read-char in-port)))
(if (eof-object? ch)
(begin
(close-input-port in-port)
'())
(cons ch (run)))))))
(list->string
(run))))))

;;Gets parsed exp and after appling the reuqired proc
(define code-gen (lambda(pe)
	(code-gen-helper pe 0)))

(define code-gen-helper (lambda(pe env-size)
	(cond
		((equal? (car pe) 'if3 ) (code-gen-if3 pe env-size))
		((equal? (car pe) 'seq) (code-gen-seq pe env-size))
		((equal? (car pe) 'or) (code-gen-or pe env-size))
		((equal? (car pe) 'applic) (code-gen-applic pe env-size))
    ((equal? (car pe) 'tc-applic) (code-gen-tc-applic pe env-size))
    ((equal? (car pe) 'const) (code-gen-const pe env-size))
    ((equal? (car pe) 'fvar) (code-gen-fvar pe env-size))
    ((equal? (car pe) 'def) (code-gen-define pe env-size))
    ((equal? (car pe) 'set) (code-gen-set pe env-size))
    ((equal? (car pe) 'pvar) (code-gen-pvar pe env-size))
    ((equal? (car pe) 'bvar) (code-gen-bvar pe env-size))
    ((equal? (car pe) 'box) (code-gen-box pe env-size))
    ((equal? (car pe) 'box-get) (code-gen-box-get pe env-size))
    ((equal? (car pe) 'box-set) (code-gen-box-set pe env-size))
    ((equal? (car pe) 'lambda-simple) (code-gen-lambda-simple pe env-size))
    ((equal? (car pe) 'lambda-opt) (code-gen-lambda-opt pe env-size))
    ((equal? (car pe) 'lambda-var) (code-gen-lambda-var pe env-size))
		(else (error "Compilation error ,For God's sake check the input we've worked hard here! ==> " pe))
		)))

;;apply but in opposite order for smpilcity's sake
(define with (lambda (s f) (apply f s)))

;;copied from 151 instructions
(define ^^label (lambda (name) (let ((n 0)) (lambda () (set! n (+ n 1)) (string-append name (number->string n))))))
 (define ^label-if3else (^^label "L_if3_else"))
 (define ^label-if3exit (^^label "L_if3_exit"))
(define nl (list->string (list #\newline)))


;;added by me
(define ^label-or-exit (^^label "L_or_exit"))
(define ^label-applic-exit (^^label "L_applic_exit"))
(define ^label-non-closure (^^label "L_error_not_closure"))
(define ^label-override (^^label "L_proc_override"))
(define ^lambda-body (^^label "L_lambda_simple_body"))
(define ^lambda-exit (^^label "L_lambda_simple_exit"))
(define ^lambda-error (^^label "L_lambda_error"))
(define ^loop1 (^^label "L_loop_1"))
(define ^loop2 (^^label "L_loop_2"))
(define ^loop-exit1 (^^label "L_loop_exit_1"))
(define ^loop-exit2 (^^label "L_loop_exit_2"))
(define ^lambda-correction-begin (^^label "L_lambda_correction_begin"))
(define ^lambda-correction-end (^^label "L_lambda_correction_end"))
(define ^label-print-content (^^label "L_print_content"))
(define ^label-print-exit (^^label "L_print_exit"))



(define ^label-lambdaCont (^^label "LlambdaCont"))
(define ^label-lambdaExit (^^label "LlambdaExit"))
(define ^label-lambdaEnvBegin (^^label "L_lambda_env_Begin"))
(define ^label-lambdaEnvEnd (^^label "L_lambda_env_End"))
(define ^label-lambdaArgsBegin (^^label "L_lambda_args_Begin"))
(define ^label-lambdaArgsEnd (^^label "L_lambda_args_End"))
(define code-gen-lambda-simple
  (lambda (e  env-size)
    (with e
      (lambda (lambda-simple params body-exp)
        (let ((label-cont (^label-lambdaCont))
          (label-exit (^label-lambdaExit))
          (label-EnvBegin (^label-lambdaEnvBegin))
          (label-ArgsBegin (^label-lambdaArgsBegin))
          (label-EnvEnd (^label-lambdaEnvEnd))
          (label-ArgsEnd (^label-lambdaArgsEnd))
          (args-length (length params))

                     (body (^lambda-body))
                  (exit (^lambda-exit))
                  (error (^lambda-error))
                  (new-env-size-str (number->string (+ 1 env-size)))
                  (loop1 (^loop1))
                  (loop-exit1(^loop-exit1))
                  (loop2 (^loop2))
                  (loop-exit2(^loop-exit2))
        )
          (string-append



            "PUSH(IMM(" (number->string (+ env-size 1)) ")); "nl
            "CALL(MALLOC);" nl
            "DROP(1);" nl
            "// R1 will hold new increased size env " nl
            "MOV(R1,R0);" nl
            "MOV(R4, IMM(" (number->string env-size) "));" nl
            "CMP(R4, IMM(0));" nl
            "JUMP_EQ(" loop-exit1 "); " nl
            "MOV(R2, FPARG(0)); //env" nl
            "MOV(R5, IMM(0));" nl
            "MOV(R6, IMM(1));" nl
            loop1 ":" nl
             "MOV(INDD(R1,R6), INDD(R2,R5));" nl
             "DECR(R4);" nl
             "INCR(R5);" nl
             "INCR(R6);" nl
             "CMP(R4, IMM(0));" nl
             "JUMP_NE(" loop1 ");" nl
            loop-exit1":" nl

            "// handling with params " nl
            "MOV(R4, IMM(" (number->string args-length) "));" nl
            "CMP(R4, IMM(0));" nl
            "JUMP_EQ(" loop-exit2  "); " nl
            "PUSH(IMM(" (number->string args-length) "));" nl
            "CALL(MALLOC);" nl
            "DROP(1);" nl
            "MOV(R3,R0);" nl
            "MOV(R5, IMM(0));" nl
            loop2 ":" nl
             "MOV(R6,R5);" nl
             "ADD(R6, IMM(2));" nl
             "MOV(INDD(R3,R5), FPARG(R6));" nl
             "DECR(R4);" nl
             "INCR(R5);" nl
             "CMP(R4, IMM(0));" nl
             "JUMP_NE(" loop2");" nl
           loop-exit2 ":" nl

            "MOV(INDD(R1,0), R3); //now R1 holds the environment" nl
            "// done handling with params " nl nl

            "// build the closure" nl
            "PUSH(LABEL(" body"));" nl
            "PUSH(R1);" nl
            "CALL(MAKE_SOB_CLOSURE);" nl
            "DROP(IMM(2));" nl
            "JUMP(" exit ");" nl
            ;; Body of the lambda, will be referenced only on application
            body ":" nl
            "PUSH(FP);" nl
            "MOV(FP,SP);" nl

            ; code-gen the body
            "//body code-gen" nl
             (code-gen-helper body-exp (+ 1 env-size))
            "//end body code-gen" nl
            "POP(FP);" nl
            "RETURN;" nl
            exit ":" nl
            nl


          )
        )

      )
    )
  )
)

; (define code-gen-lambda-simple (lambda(pe env-size)
;   (with pe (lambda (simple args body-exp)

;           (let* ( (args-length (length args))
;                   (body (^lambda-body))
;                   (exit (^lambda-exit))
;                   (error (^lambda-error))
;                   (new-env-size-str (number->string (+ 1 env-size)))
;                   (loop1 (^loop1))
;                   (loop-exit1(^loop-exit1))
;                   (loop2 (^loop2))
;                   (loop-exit2(^loop-exit2))
;                   )

;       (string-append
;         "/* start of lambda simple code gen */" nl
;         "MOV(R1,FPARG(0));//previous env" nl
;         "/* allocating memory for the new env */"nl
;         "PUSH("new-env-size-str");" nl
;         "CALL(MALLOC);" nl
;         "DROP(1);" nl
;         "MOV(R2,R0);" nl
;         "/* end of allocation */" nl

;         ;; here must many thing but now im just doing the loop
;         "MOV(R3,0);//i"nl
;         loop1 ":" nl
;         "MOV(R4,1);//new env index j" nl
;         "CMP(R3,"(number->string env-size)");" nl
;         "JUMP_EQ("loop-exit1");" nl
;         "MOV(INDD(R2,R4), INDD(R1,R3));//R2[j] = R1[i];" nl
;         "ADD(R3,1);" nl
;         "ADD(R4,1);" nl
;         loop-exit1 ":" nl
;         "MOV(R3,FPARG(1));//prevois lambda args num " nl
;         "PUSH(R3);" nl
;         "CALL(MALLOC);"nl
;         "DROP(1);" nl
;         "MOV(INDD(R2,0),R0);// R2[0]=MALLOC(R3)" nl
;         "MOV(R3,0); //i=0" nl
;         loop2 ":" nl
;         "MOV(R4,2); //j=2" nl
;         "CMP(R3,FPARG(1)); // if reach args num" nl
;         "JUMP_EQ("loop-exit2");" nl
;         "INCR(R4);" nl
;         "MOV(INDD(R0,R3),FPARG(R4));// R2[0][i] = FPARG[j];" nl
;         "INCR(R3);" nl
;         "JUMP("loop2");" nl
;         loop-exit2 ":" nl
;         "PUSH(IMM(3));" nl
;         "CALL(MALLOC);" nl
;         "DROP(1);" nl
;         "MOV(IND(R0),IMM(T_CLOSURE));" nl
;         "MOV(INDD(R0,1),R2);" nl
;         "MOV(INDD(R0,2), LABEL(" body ")) ;" nl
;         "JUMP("exit");" nl
;         body ":" nl
;         "PUSH(FP);" nl
;         "MOV (FP,SP);" nl
;         "CMP(FPARG(1),"(number->string (+ 1 args-length))");" nl
;         ;;"CMP(FPARG(1),IMM(3));" nl
;         "JUMP_NE("error");" nl
;         (code-gen-helper body-exp (+ 1 env-size))
;         "POP(FP);" nl
;         "RETURN;" nl
;         error ":" nl

;         "SHOW(Exception: Error occured on lambda simple !,R0);" nl
;          "STOP_MACHINE;" nl

;         exit ":" nl

;          "/* end of lambda simple code gen */" nl

;         )

; )))))
;;check labels here
(define code-gen-lambda-opt (lambda(pe env-size)
  (with pe (lambda (opt args rest body-exp)
          (let* ( (args-length (length args))
                  (body (^lambda-body))
                  (exit (^lambda-exit))
                  (error (^lambda-error))
                  (new-env-size-str (number->string (+ 1 env-size)))
                  (loop1 (^loop1))
                  (loop-exit1(^loop-exit1))
                  (loop2 (^loop2))
                  (loop-exit2(^loop-exit2))
                  (lambda-correction-begin (^lambda-correction-begin))
                  (lambda-correction-end (^lambda-correction-end))
                  )
      (string-append
        "MOV(R1,FPARG(0));//previous env" nl
        "/* allocating memory for the new env */"nl
        "PUSH("new-env-size-str");" nl
        "CALL(MALLOC);" nl
        "DROP(1);" nl
        "MOV(R2,R0);" nl
        "/* end of allocation */" nl

        ;; here must many thing but now im just doing the loop
        "MOV(R3,0);//i"nl
        loop1 ":" nl
        "MOV(R4,1);//new env index j" nl
        "CMP(R3,"(number->string env-size)");" nl
        "JUMP_EQ("loop-exit1");" nl
        "MOV(INDD(R2,R4), INDD(R1,R3));//R2[j] = R1[i];" nl
        "ADD(R3,1);" nl
        "ADD(R4,1);" nl
        loop-exit1 ":" nl
        "MOV(R3,FPARG(1));//prevois lambda args num " nl
        "PUSH(R3);" nl
        "CALL(MALLOC);"nl
        "DROP(1);" nl
        "MOV(INDD(R2,0),R0);// R2[0]=MALLOC(R3)" nl
        "MOV(R3,0); //i=0" nl
        loop2 ":" nl
        "MOV(R4,2); //j=2" nl
        "CMP(R3,FPARG(1)); // if reach args num" nl
        "JUMP_EQ("loop-exit2");" nl
        "INCR(R4);" nl
        "MOV(INDD(R0,R3),FPARG(R4));// R2[0][i] = FPARG[j];" nl
        "INCR(R3);" nl
        "JUMP("loop2");" nl
        loop-exit2 ":" nl
        "PUSH(IMM(3));" nl
        "CALL(MALLOC);" nl
        "DROP(1);" nl
        "MOV(IND(R0),IMM(T_CLOSURE));" nl
        "MOV(INDD(R0,1),R2);" nl
        "MOV(INDD(R0,2), LABEL(" body ")) ;" nl
        "JUMP("exit");" nl
        body ":" nl
        "PUSH(FP);" nl
        "MOV (FP,SP);" nl
        "/* begining of lambda opt correction */" nl
        "MOV(R1,FPARG(1)); //args num" nl
        "SUB(R1,IMM("(number->string (+ 1 (length args)))")); //sub params + magic box" nl
        "MOV(R0, IMM(SOB_NIL)); //last mem in the list" nl
        "CMP(R1, IMM(0));" nl
        "JUMP_EQ("lambda-correction-end");" nl
        "MOV(R2, FPARG(1)); //args num "nl
        lambda-correction-begin ":" nl
        "PUSH(R0); //second arg on pair" nl
        "PUSH(FPARG(R2)); //first arg on pair" nl
        "CALL(MAKE_SOB_PAIR);" nl
        "DROP(2);" nl
        "DECR(R2);" nl
        "DECR(R1);" nl
        "CMP(R1, IMM(0));" nl
        "JUMP_NE("lambda-correction-begin");" nl
        lambda-correction-end ":" nl
        "MOV(FPARG(" (number->string (+ 2 (length args))) "), R0);// moving motherfucker list to the right pos" nl
        "/* end of lambda opt correction */" nl


        (code-gen-helper body-exp (+ 1 env-size))
        "POP(FP);" nl
        "RETURN;" nl
        exit ":" nl

        )
      )))))

  (define code-gen-lambda-var (lambda(pe env-size)

  (with pe (lambda (var args body-exp)

          (let* ( ;(args-length (length args))
                  (body (^lambda-body))
                  (exit (^lambda-exit))
                  (error (^lambda-error))
                  (new-env-size-str (number->string (+ 1 env-size)))
                  (loop1 (^loop1))
                  (loop-exit1(^loop-exit1))
                  (loop2 (^loop2))
                  (loop-exit2(^loop-exit2))
                  (lambda-correction-begin (^lambda-correction-begin))
                  (lambda-correction-end (^lambda-correction-end))
                  )
         ;    (display 'fuck)
      (string-append
        "MOV(R1,FPARG(0));//previous env" nl
        "/* allocating memory for the new env */"nl
        "PUSH("new-env-size-str");" nl
        "CALL(MALLOC);" nl
        "DROP(1);" nl
        "MOV(R2,R0);" nl
        "/* end of allocation */" nl

        ;; here must many thing but now im just doing the loop
        "MOV(R3,0);//i"nl
        loop1 ":" nl
        "MOV(R4,1);//new env index j" nl
        "CMP(R3,"(number->string env-size)");" nl
        "JUMP_EQ("loop-exit1");" nl
        "MOV(INDD(R2,R4), INDD(R1,R3));//R2[j] = R1[i];" nl
        "ADD(R3,1);" nl
        "ADD(R4,1);" nl
        loop-exit1 ":" nl
        "MOV(R3,FPARG(1));//prevois lambda args num " nl
        "PUSH(R3);" nl
        "CALL(MALLOC);"nl
        "DROP(1);" nl
        "MOV(INDD(R2,0),R0);// R2[0]=MALLOC(R3)" nl
        "MOV(R3,0); //i=0" nl
        loop2 ":" nl
        "MOV(R4,2); //j=2" nl
        "CMP(R3,FPARG(1)); // if reach args num" nl
        "JUMP_EQ("loop-exit2");" nl
        "INCR(R4);" nl
        "MOV(INDD(R0,R3),FPARG(R4));// R2[0][i] = FPARG[j];" nl
        "INCR(R3);" nl
        "JUMP("loop2");" nl
        loop-exit2 ":" nl
        "PUSH(IMM(3));" nl
        "CALL(MALLOC);" nl
        "DROP(1);" nl
        "MOV(IND(R0),IMM(T_CLOSURE));" nl
        "MOV(INDD(R0,1),R2);" nl
        "MOV(INDD(R0,2), LABEL(" body ")) ;" nl
        "JUMP("exit");" nl
        body ":" nl
        "PUSH(FP);" nl
        "MOV (FP,SP);" nl
        "/* begining of lambda var correction */" nl
        "MOV(R1,FPARG(1)); //args num" nl
        "SUB(R1,IMM(1)); //sub magic box" nl
        "MOV(R0, IMM(SOB_NIL)); //last mem in the list" nl
        "CMP(R1, IMM(0));" nl
        "JUMP_EQ("lambda-correction-end");" nl
        "MOV(R2, FPARG(1)); //args num "nl
        lambda-correction-begin ":" nl
        "PUSH(R0); //second arg on pair" nl
        "PUSH(FPARG(R2)); //first arg on pair" nl
        "CALL(MAKE_SOB_PAIR);" nl
        "DROP(2);" nl
        "DECR(R2);" nl
        "DECR(R1);" nl
        "CMP(R1, IMM(0));" nl
        "JUMP_NE("lambda-correction-begin");" nl
        lambda-correction-end ":" nl
        "MOV(FPARG(2), R0);// moving motherfucker list to the right pos" nl
        "//end of lambda var correction" nl


        (code-gen-helper body-exp (+ 1 env-size))
        "POP(FP);" nl
        "RETURN;" nl
        exit ":" nl

        )
      )))))

(define code-gen-const (lambda(pe env-size)
  (with pe (lambda (const exp)
          (let* ((addr (number->string (car (find-in-table exp constTable 2)))))
            (string-append
            " /*==> start of code gen for const*/  " nl
             "  MOV(R0,"addr");" nl
             "/*==>  end of code gen for const*/ " nl)
            )))))

(define code-gen-set (lambda(pe env-size)
  (with pe (lambda (set var val)
    (if (equal? 'pvar (car var))

          (let* ((minor (number->string (caddr var))))
            (string-append
              (code-gen-helper val env-size)
            " /*start of code gen for set*/  " nl
            "MOV(FPARG(2 + "minor"), R0);"
             ;"  MOV(IND("addr"),R0);" nl
              "MOV(R0,IMM(SOB_VOID));" nl
             "/*end of code gen for set*/ " nl)
            )

          (let* ((addr (number->string (find-in-global-table  (cadr var)  Global-table))))
            (string-append
              (code-gen-helper val env-size)
            " /*start of code gen for set*/  " nl
             "  MOV(IND("addr"),R0);" nl
              "MOV(R0,IMM(SOB_VOID));" nl
             "/*end of code gen for set*/ " nl)
            )

          )
    ))))

(define code-gen-box-set (lambda(pe env-size)
   (with pe (lambda (box-set var val)

         (string-append
          "//code-gen-box-set" nl
                      (code-gen-helper val env-size) nl
                        "MOV(R1,R0);" nl
                         (code-gen-helper var env-size) nl
                        "MOV(IND(R0),R1);" nl
                        "MOV(R0,IMM(SOB_VOID));" nl
                        ))
    )))

(define code-gen-box (lambda(pe env-size)
   (with pe (lambda (box var)

         (string-append
           "//code-gen-box" nl
          (code-gen-helper var env-size) nl
                        "MOV(R1,R0);" nl
                        "PUSH(IMM(1));"nl
                        "CALL(MALLOC);"nl
                        "DROP(1);"
                        "MOV(IND(R0),R1);" nl
                        ))
    )))


(define code-gen-box-get (lambda(pe env-size)
  (with pe (lambda (box-get var )
     (string-append
       "//code-gen-box-get" nl
      (code-gen-helper var env-size) nl
      "MOV(R0,IND(R0));" nl
      )
    ))))


(define code-gen-pvar (lambda(pe env-size)
  (with pe (lambda (pvar var min)
          (let* ((min-stack (number->string (+ 2 min))))
            (string-append
             ; (code-gen-helper val env-size)
            " /*start of code gen for pvar*/  " nl
             "  MOV(R0, FPARG("min-stack "));" nl
             "/*end of code gen for pvar*/ " nl)
            )))))

(define code-gen-bvar (lambda(pe env-size)
  (with pe (lambda (bvar var maj min)

            (string-append
          ;    (code-gen-helper val env-size)
            " /*start of code gen for bvar*/  " nl
             "  MOV(R0, FPARG(0)); //env"nl
             "   MOV(R0,INDD(R0, "(number->string maj)"));"nl
             "  MOV(R0,INDD(R0, "(number->string  min)"));" nl
             "/*end of code gen for bvar*/ " nl)
            ))))

(define code-gen-define (lambda(pe env-size)
  (with pe (lambda (define var val)
          (let* ((addr (number->string (find-in-global-table  (cadr var)  Global-table))))
            (string-append
              (code-gen-helper val env-size)
            " /*start of code gen for define*/  " nl
             "  MOV(IND("addr"),R0);" nl
              "MOV(R0,IMM(SOB_VOID));" nl
             "/*end of code gen for define*/ " nl)
            )))))

(define code-gen-fvar (lambda(pe env-size)
  (with pe (lambda (fvar exp)
          (let* ((addr (number->string (find-in-global-table  exp Global-table))))
            (string-append
            " /*start of code gen for fvar*/  " nl
             "  MOV(R0,IND("addr"));" nl
             "/*end of code gen for fvar*/ " nl)
            )))))

(define code-gen-if3 (lambda(pe env-size)
	(with pe (lambda (if3 test dit dif)
		(let*(
			(test-code (code-gen-helper test env-size))
			(dit-code  (code-gen-helper dit env-size))
			(dif-code  (code-gen-helper dif env-size))
			(label-else (^label-if3else))
            (label-exit (^label-if3exit))
			)
		(string-append
      "/*start of code gen for if3 */" nl
			test-code nl; the result should be in R0
			"CMP(R0,IMM(SOB_FALSE));" nl ;;Here SOB_FALSE is a the addres in the const table
			"JUMP_EQ("label-else");" nl
			dit-code nl
			"JUMP("label-exit");" nl
			label-else":" nl
			dif-code
			label-exit ":" nl
      "/* end of code gen for if3*/ " nl))))))


(define code-gen-seq (lambda(pe env-size)
	(with pe (lambda (seq exprs)
			(let* (
				(exprs-code (apply string-append (map (lambda(expr)
				(code-gen-helper expr env-size)) exprs)))
				)
			exprs-code)))))

(define get-all-without-last
  (lambda (lst)
    (reverse (cdr (reverse lst)))))

(define get-last
  (lambda (lst)
    (car (reverse lst))))

(define code-gen-or (lambda(pe env-size)
  (with pe (lambda (or exprs)
       (let*((all-pes-without-last (get-all-without-last exprs))
            (last-pe (get-last exprs))
            (label_exit (^label-or-exit))
            (all-pes-without-last-code (apply string-append (map (lambda(exp)
                                            (string-append
                                            (code-gen-helper exp env-size)
                                            "CMP(R0,IMM(SOB_FALSE));" nl
                                             "JUMP_NE("label_exit"); " nl
                                             ) ) all-pes-without-last)))
            (last-pe-code (code-gen-helper last-pe env-size))
            )
       (string-append
         "/*start of code gen for or */" nl
         all-pes-without-last-code
         last-pe-code
         label_exit ":" nl
         "/*end of code gen for or */" nl
        ))))))

; (define code-gen-or (lambda(pe env-size)
; 	(with pe (lambda (or exprs)
; 		(if (null? exprs)
; 		    (string-append

;           (^label-or-exit) ":" nl
;          )

; 			(let* (
; 				(exp1-code (code-gen-helper (car exprs) env-size))
;         (label_exit (^label-or-exit))
;         (rest (code-gen-or `(or ,(cdr exprs)) env-size))
; 				)
;     ;  (if (string? rest) (display 'rest))

; 			(string-append
;          "/*start of code gen for or /*" nl
; 			exp1-code
; 			"CMP(Ro,IMM(SOB_FALSE));" nl
; 			"JUMP_NE("label_exit"); " nl
; 			rest
; 			label_exit ":" nl
;       "/*end of code gen for or /*" nl)
; 			))))))

; (define code-gen-applic (lambda(pe env-size)
; 	(with pe (lambda(applic proc args)
;    ; (display proc)
; 	(let* (
; 			(arguments (reverse args))
; 			(args-num (+ 1 (length arguments)));;check if we need + 1!!!!!!!!!
; 			(args-code (apply string-append (map (lambda(arg)
; 							(string-append
; 							 	(code-gen-helper arg env-size) "PUSH(R0);" nl)) arguments)))
; 			(proc-code (code-gen-helper proc env-size))
; 			(label-non-closure-exit (^label-non-closure))
; 			(label-exit (^label-applic-exit))
; 			)
;   (string-append
;     "/*start of code gen for applic */" nl
; 	"PUSH(IMM(SOB_NIL));" nl ;;fix for lambda opt ....
; 	args-code
; 	"PUSH("(number->string args-num)");" nl
; 	proc-code
; 	"CMP(IND(R0),IMM(T_CLOSURE));" nl
; 	"JUMP_NE("label-non-closure-exit");" nl
; 	"PUSH(INDD(R0,1));" nl ;;pushs the env
; 	"CALLA(INDD(R0,2));" nl
; 	"DROP(1);"nl ;;drops the env
; 	;;"POP(R0);" nl ;; gots args num changed !!!!!!!!!!!!!!!!!!!!!!!!
; 	"DROP("(number->string args-num)");"nl ;; drops m+1 args
;  ;  "MOV(R1, STARG(0));" nl
;  ; "ADD(R1, IMM(2));"nl
;  ;  "DROP (R1);" nl
; 	"JUMP("label-exit");" nl
; 	label-non-closure-exit ": "nl
; 	"SHOW(Exception: Trying to apply non-proc !,R0);" nl
; 	label-exit ":" nl
;   "/*end of code gen for applic */" nl)
; 	)))))

(define code-gen-applic
  (lambda (pe  env-size)
    (with pe
      (lambda (applic proc args)
        (let* ((arguments (reverse args))
          (label-non-closure-exit (^label-non-closure))
           (label-exit (^label-applic-exit))
           (proc-code (code-gen-helper proc env-size))
          (args-code (apply string-append
                            (map
                              (lambda (exp) (string-append
                                      (code-gen-helper  exp  env-size)
                                      "PUSH(R0);" nl))
                              arguments)))
        )
          (string-append
           "/*start of code gen for applic */" nl
            "PUSH(IMM(SOB_NIL));" nl
            args-code
            "PUSH(IMM(" (number->string (+ 1 (length arguments))) ")); //m+1" nl

             proc-code
            "CMP(INDD(R0,IMM(0)) , T_CLOSURE);" nl
            "JUMP_NE("  label-non-closure-exit  ");" nl
            "PUSH(INDD(R0,1));  // env" nl
            "CALLA(INDD(R0,2));  //code" nl
            "MOV(R1, STARG(0));" nl
            "ADD(R1, IMM(2));"nl
            "DROP (R1);" nl
            "JUMP(" label-exit ");" nl
            label-non-closure-exit  ":" nl
           "SHOW(Exception: Trying to apply non-proc !,R0);" nl
            label-exit ":" nl
           "/*end of code gen for applic */" nl
          )

        )
      )
    )
  )
)

; (define code-gen-tc-applic
;   (lambda (e env-size)
;     (with e
;       (lambda (tc-applic proc args)
;         (let* ((arguments (reverse args))
;            (label-non-closure-exit (^label-non-closure))
;      ;; (label-exit (^label-applic-exit)
;       (label-override (^label-override))
;           (applic-code (apply string-append (map

;                               (lambda (x) (string-append
;                                       (code-gen-helper x env-size)
;                                       "PUSH(R0);" nl))
;                               arguments)))
;         )
;           (string-append
;             (format "//begin expr: ~a" e) nl
;             "PUSH(IMM(SOB_NIL)); //MAGIC BOX" nl
;             applic-code
;             "PUSH(IMM(" (number->string (+ 1 (length arguments))) ")); //pushing args size to stack +1 for magic box" nl
;             "// done pushing args, now handling proc" nl
;            (code-gen-helper proc  env-size)
;             "CMP(INDD(R0,IMM(0)) , T_CLOSURE);" nl
;             "JUMP_NE(" label-non-closure-exit ");" nl
;             "PUSH(INDD(R0,1));  // env" nl
;             "PUSH(FPARG(-1)); // return address from current frame" nl
;             "MOV(R1,FPARG(-2)); // save FP" nl
;             "//start overriding old frame" nl
;             "MOV(R2, IMM(" (number->string (+ (length arguments) 4)) ")); //R2 holds loop size" nl
;             "MOV(R3, FPARG(1)); //number of old arguments" nl
;             "MOV(R6, FPARG(1)); //save old args for DROP later" nl
;             "ADD(R3,IMM(1)); //R3 points to first old param from FPARG point of view" nl
;             "MOV(R4, STARG(1)); //number of new arguments" nl
;             "ADD(R4, IMM(1)); //R4 points to first new param from STARG point of view" nl
;             label-override ":" nl
;              "MOV(R5, STARG(R4));" nl
;              "MOV(FPARG(R3),R5); //overriding" nl
;              "SUB(R2,1);" nl
;              "SUB(R3,1); //next old param" nl
;              "SUB(R4,1); //next new param" nl
;              "CMP(R2, IMM(0));" nl
;              "JUMP_NE(" label-override ");" nl
;             "//end overriding" nl
;             ;; we can determine the DROP value at compile time
;             "//complete the override by dropping unnecessary items from stack" nl
;             "ADD(R6, IMM(4));" nl
;             "DROP(R6);" nl
;             "MOV(FP,R1); //Restore old FP in preparation of JUMP" nl
;             "JUMPA(INDD(R0,2));  //code" nl

;             label-non-closure-exit ":" nl
;             "SHOW(\"Exception: attempt to apply non-procedure \", R0);" nl
;             (format "//end expr: ~a" e) nl
;           )

;         )
;       )
;     )
;   )
; )

(define code-gen-tc-applic (lambda(pe env-size)
  (with pe (lambda(applic proc args)
  (let* (
      (arguments (reverse args))
      (args-num (+ 1 (length arguments)));;check if we need + 1!!!!!!!!!
      (args-code (apply string-append (map (lambda(arg)
              (string-append
                (code-gen-helper arg env-size) "PUSH(R0);" nl)) arguments)))
      (proc-code (code-gen-helper proc env-size))
      (label-non-closure-exit (^label-non-closure))
     ;; (label-exit (^label-applic-exit)
      (label-override (^label-override))
      )
  (string-append
    "/*start of code gen for tc-applic */" nl
  "PUSH(IMM(SOB_NIL));" nl ;;fix for lambda opt ....
  args-code
  "PUSH("(number->string args-num)");//args num includes nil" nl ;; args num includes nil
  proc-code
  "CMP(IND(R0),IMM(T_CLOSURE));" nl
  "JUMP_NE("label-non-closure-exit");" nl
  "PUSH(INDD(R0,1));//env" nl ;;pushs the env
  "PUSH(FPARG(-1));//pushes the ret address of curr frame" nl
  "MOV(R1,FPARG(-2)); //save old fp in R1" nl
  "//==> begining of overriding "nl
  "MOV(R2, IMM("(number->string (+ args-num 3))"));" nl ;; "was 4" added 1 here
  "MOV(R3, FPARG(1)); //old args num" nl
  "ADD(R3,IMM(1));//points on the first arg"
  "MOV(R6, FPARG(1)); //for drop later " nl
  "MOV(R4, STARG(1)); //num of new args"nl
  "ADD(R4, IMM(1));// pointer to first arg" nl
  label-override ":" nl
  "MOV(R5, STARG(R4));//move new param to old param's place" nl
  "MOV(FPARG(R3),R5); //overriding" nl
  "SUB(R2,1);//decreasing num of old args" nl
  "SUB(R3,1); //next old param" nl
  "SUB(R4,1); //next new param" nl
  "CMP(R2, IMM(0));" nl
  "JUMP_NE(" label-override ");" nl
  "//==> end of overriding "nl
  "//drop unnecessary items from the stack" nl
  ;;"ADD(R6, IMM(4));" nl
  ;;"DROP(R6);" nl
  "MOV(FP,R1);//restore old fp" nl
  "JUMPA(INDD(R0,2));  //body" nl
  label-non-closure-exit ": "nl
  "SHOW(Exception: Trying to apply non-proc !,R0);" nl

  "/*end of code gen for tc-applic */" nl)
  )))))


  ;;PRIMITIVESSSSSSSSSSSSSSSSS;;
  (define prim-apply-code
  (lambda ()
    (string-append
      "/* primitive apply */" nl
      "JUMP(L_apply_cont);" nl
      "L_prim_apply:" nl
       "PUSH(FP);" nl
       "MOV(FP, SP);" nl

       "MOV(R1,IMM(0)); //R1 will hold list size" nl
       "MOV(R0, FPARG(3)); //list" nl
       "L_apply_size_Begin:" nl
        "CMP(IND(R0), T_PAIR);" nl
        "JUMP_NE(L_apply_size_End);" nl
        "INCR(R1);" nl
        "MOV(R0, INDD(R0,2));" nl
        "JUMP(L_apply_size_Begin);" nl
       "L_apply_size_End:" nl
      "/* done calculating list size, R1 holds it */" nl
       "MOV(R2, R1); //save the size for later use" nl
       "INCR(R1); //include the magic box" nl
       "/* start pushing the list args to stack (last first) */" nl
       "PUSH(IMM(SOB_NIL)); //MAGIC BOX" nl
       "L_apply_begin_push:" nl
        "CMP(R2, IMM(0));" nl
        "JUMP_EQ(L_apply_end_push);" nl
        "//loop that finds the current arg to push" nl
        "MOV(R3, R2);" nl
        "DECR(R3); //R3 holds number of inner iterations" nl
        "MOV(R0, FPARG(3)); //first pair in list" nl
        "L_apply_begin_search:" nl
         "CMP(R3, IMM(0));" nl
         "JUMP_EQ(L_apply_end_search);" nl
         "MOV(R0, INDD(R0,2)); //next pair" nl
         "DECR(R3);" nl
         "JUMP(L_apply_begin_search);" nl
        "L_apply_end_search:" nl
        "PUSH(INDD(R0,1));" nl
        "DECR(R2);" nl
        "JUMP(L_apply_begin_push);" nl
       "L_apply_end_push:" nl
       "PUSH(R1); //pushing args size to stack" nl

       "MOV(R0, FPARG(2)); //proc" nl
       "CMP(IND(R0) , T_CLOSURE);" nl
       "JUMP_NE(L_apply_not_proc);" nl
       "PUSH(INDD(R0,1));  // env" nl
       "PUSH(FPARG(-1)); // return address from current frame" nl
       "MOV(R2,FPARG(-2)); // save FP" nl
       "//start overriding old frame" nl
       "MOV(R3, R1);" nl
       "ADD(R3, IMM(3)); //R3 holds loop size" nl
       "MOV(R4, FPARG(1)); //number of old arguments" nl
       "ADD(R4,IMM(1)); //R4 points to first old param from FPARG point of view" nl
       "MOV(R5, STARG(1)); //number of new arguments" nl
       "ADD(R5, IMM(1)); //R5 points to first new param from STARG point of view" nl
       "L_apply_override:" nl
        "MOV(R6, STARG(R5));" nl
        "MOV(FPARG(R4),R6); //overriding" nl
        "SUB(R3,1);" nl
        "SUB(R4,1); //next old param" nl
        "SUB(R5,1); //next new param" nl
        "CMP(R3, IMM(0));" nl
        "JUMP_NE(L_apply_override);" nl
       "//end overriding" nl

       "//complete the override by dropping unnecessary items from stack" nl
       "DROP(IMM(7)); //apply had 7 values on stack before" nl
       "MOV(FP,R2); //Restore old FP in preparation of JUMP" nl
       "JUMPA(INDD(R0,2));  //code" nl
       "L_apply_not_proc:" nl
       "SHOW(\"Exception: attempt to apply non-procedure \", R0);" nl
       "POP(FP);" nl
       "RETURN;" nl
      "L_apply_cont:" nl

      "#define SOB_PRIM_APPLY 76" nl

    )
  )
)
  (define prim-number?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE NUMBER? --- " nl
          "JUMP(L_number_def);" nl
          "L_prim_number:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, IND(R1));" nl
          "CMP(R1, IMM(" "T_INTEGER" "));" nl
          "JUMP_EQ(L_number_true);" nl
          "CMP(R1, IMM(" "T_FRACTION" "));" nl
          "JUMP_EQ(L_number_true);" nl
          "MOV(R0, SOB_FALSE);" nl
          "JUMP(L_number_finish);" nl

          "L_number_true:" nl
          "MOV(R0, SOB_TRUE);" nl

           "L_number_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_number_def:" nl
        )
      )
    )

  (define prim-integer?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE INTEGER? --- " nl
          "JUMP(L_integer_def);" nl
          "L_prim_integer:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "PUSH(R1);" nl
          "CALL(IS_SOB_INTEGER);" nl
          "DROP(1);" nl
          "CMP(R0, IMM(0));" nl
          "JUMP_EQ(L_integer_false);" nl

          "L_integer_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_integer_finish);"
          "L_integer_false:"
          "MOV(R0, SOB_FALSE);"

           "L_integer_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_integer_def:" nl
        )
      )
    )

  (define prim-char?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE CHAR? --- " nl
          "JUMP(L_char_def);" nl
          "L_prim_char:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "PUSH(R1);" nl
          "CALL(IS_SOB_CHAR);" nl
          "DROP(1);" nl
          "CMP(R0, IMM(0));" nl
          "JUMP_EQ(L_char_false);" nl

          "L_char_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_char_finish);"
          "L_char_false:"
          "MOV(R0, SOB_FALSE);"

           "L_char_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_char_def:" nl
        )
      )
    )

  (define prim-symbol?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE SYMBOL? --- " nl
          "JUMP(L_symbol_def);" nl
          "L_prim_symbol:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, IND(R1));" nl
          "CMP(R1,IMM(" "T_SYMBOL" "));" nl
          "JUMP_NE(L_symbol_false);" nl

          "L_symbol_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_symbol_finish);"
          "L_symbol_false:"
          "MOV(R0, SOB_FALSE);"

           "L_symbol_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_symbol_def:" nl
        )
      )
    )

  (define prim-string?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE STRING? --- " nl
          "JUMP(L_string_def);" nl
          "L_prim_string:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, IND(R1));" nl
          "CMP(R1, IMM(" "T_STRING" "));" nl
          "JUMP_NE(L_string_false);" nl

          "L_string_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_string_finish);"
          "L_string_false:"
          "MOV(R0, SOB_FALSE);"

           "L_string_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_string_def:" nl
        )
      )
    )

  (define prim-vector?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE VECTOR? --- " nl
          "JUMP(L_vector_def);" nl
          "L_prim_vector:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, IND(R1));" nl
          "CMP(R1, IMM(" "T_VECTOR" "));" nl
          "JUMP_NE(L_vector_false);" nl

          "L_vector_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_vector_finish);"
          "L_vector_false:"
          "MOV(R0, SOB_FALSE);"

           "L_vector_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_vector_def:" nl
        )
      )
    )

  (define prim-null?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE NULL? --- " nl
          "JUMP(L_null_def);" nl
          "L_prim_null:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, IND(R1));" nl
          "CMP(R1, IMM(" "T_NIL" "));" nl
          "JUMP_NE(L_null_false);" nl

          "L_null_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_null_finish);"
          "L_null_false:"
          "MOV(R0, SOB_FALSE);"

           "L_null_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_null_def:" nl
        )
      )
    )

  (define prim-boolean?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE BOOLEAN? --- " nl
          "JUMP(L_boolean_def);" nl
          "L_prim_boolean:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, IND(R1));" nl
          "CMP(R1, IMM(" "T_BOOL" "));" nl
          "JUMP_NE(L_boolean_false);" nl

          "L_boolean_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_boolean_finish);"
          "L_boolean_false:"
          "MOV(R0, SOB_FALSE);"

           "L_boolean_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_boolean_def:" nl
        )
      )
    )

  (define prim-procedure?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE PROCEDURE? --- " nl
          "JUMP(L_procedure_def);" nl
          "L_prim_procedure:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, IND(R1));" nl
          "CMP(R1, IMM(" "T_CLOSURE" "));" nl
          "JUMP_NE(L_procedure_false);" nl

          "L_procedure_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_procedure_finish);"
          "L_procedure_false:"
          "MOV(R0, SOB_FALSE);"

           "L_procedure_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_procedure_def:" nl
        )
      )
    )


      (define prim-rational?-code
        (lambda ()
            (string-append
              "// --- PRIMITIVE RATIONAL? --- " nl
              "JUMP(L_rational_def);" nl
              "L_prim_rational:" nl
              "PUSH(FP);" nl
              "MOV(FP, SP);" nl
              "MOV(R1, FPARG(2)); // R1=params" nl
              "MOV(R1, IND(R1));" nl
              "CMP(R1, IMM(" "T_INTEGER" "));" nl
              "JUMP_EQ(L_rational_true);" nl
              "CMP(R1, IMM(" "T_FRACTION" "));" nl
              "JUMP_EQ(L_rational_true);" nl
              "MOV(R0, SOB_FALSE);" nl
              "JUMP(L_rational_finish);" nl

              "L_rational_true:" nl
              "MOV(R0, SOB_TRUE);" nl

               "L_rational_finish:" nl
               "POP(FP);" nl
               "RETURN;" nl

               "L_rational_def:" nl
            )
          )
        )


  (define prim-zero?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE ZERO? --- " nl
          "JUMP(L_zero_def);" nl
          "L_prim_zero:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, INDD(R1,1));" nl
          "CMP(R1, IMM(0));" nl
          "JUMP_NE(L_zero_false);" nl

          "L_zero_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_zero_finish);"
          "L_zero_false:"
          "MOV(R0, SOB_FALSE);"

           "L_zero_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_zero_def:" nl
        )
      )
    )

  (define prim-pair?-code
    (lambda ()
        (string-append
          "// --- PRIMITIVE PAIR? --- " nl
          "JUMP(L_pair_def);" nl
          "L_prim_pair:" nl
          "PUSH(FP);" nl
          "MOV(FP, SP);" nl
          "MOV(R1, FPARG(2)); // R1=params" nl
          "MOV(R1, IND(R1));" nl
          "CMP(R1, IMM(" "T_PAIR" "));" nl
          "JUMP_NE(L_pair_false);" nl

          "L_pair_true:"
          "MOV(R0, SOB_TRUE);"
          "JUMP(L_pair_finish);"
          "L_pair_false:"
          "MOV(R0, SOB_FALSE);"

           "L_pair_finish:" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_pair_def:" nl
        )
      )
    )


(define prim-eq?-code
  (lambda ()
    (string-append
      "// --- PRIMITIVE EQ?--- " nl
      "JUMP(L_eq_def);" nl
      "L_prim_eq:" nl
       "PUSH(FP);" nl
       "MOV(FP,SP);" nl
   		 "MOV(R0, FPARG(2));" nl
   		 "MOV(R1, FPARG(3));" nl
       "MOV (R0, IND(R0));" nl
       "MOV (R1, IND(R1));" nl
   		 "CMP(R0,R1);" nl
   		 "JUMP_NE(L_eq_false);" nl
   		 "CMP(R0, T_VOID);" nl
   		 "JUMP_EQ(L_eq_check_data);" nl
   		 "CMP(R0, T_NIL);" nl
   		 "JUMP_EQ(L_eq_check_data);" nl
   		 "CMP(R0, T_BOOL);" nl
   		 "JUMP_EQ(L_eq_check_data);" nl
   		 "CMP(R0, T_CHAR);" nl
   		 "JUMP_EQ(L_eq_check_data);" nl
   		 "CMP(R0, T_INTEGER);" nl
   		 "JUMP_EQ(L_eq_check_data);" nl
   		 "CMP(R0, T_CLOSURE);" nl
   		 "JUMP_EQ(L_eq_check_address);" nl
   		 "CMP(R0, T_STRING);" nl
   		 "JUMP_EQ(L_eq_check_address);" nl
   		 "CMP(R0, T_PAIR);" nl
   		 "JUMP_EQ(L_eq_check_address);" nl
   		 "CMP(R0, T_VECTOR);" nl
   		 "JUMP_EQ(L_eq_check_address);" nl
   		 "CMP(R0, T_SYMBOL);" nl
   		 "JUMP_EQ(L_eq_check_address);" nl

       "L_eq_check_address:" nl
       "MOV(R0, FPARG(2));" nl
       "MOV(R1, FPARG(3));" nl
   		 "CMP(R0,R1);" nl
   		 "JUMP_NE(L_eq_false);" nl
   	   "MOV(R0, SOB_TRUE);" nl
   	   "JUMP(L_eq_finish);" nl

   			"L_eq_check_data:" nl
        "MOV(R0, FPARG(2));" nl
        "MOV(R1, FPARG(3));" nl
   		 "CMP(INDD(R0,1), INDD(R1,1));" nl
   		 "JUMP_NE(L_eq_false);" nl
   		 "MOV(R0, SOB_TRUE);" nl
   		 "JUMP(L_eq_finish);" nl

   			"L_eq_check_symbol:" nl
   			"L_eq_false:" nl
   		  "MOV(R0, SOB_FALSE);" nl
   			"L_eq_finish:" nl

       "POP(FP);" nl
       "RETURN;" nl

       "L_eq_def:" nl
    )
  )
)



    ;;PAIR;;

    (define prim-car-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE CAR--- " nl
          "JUMP(L_car_def);" nl
          "L_prim_car:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R0, INDD(R0, 1))" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_car_def:" nl
        )
      )
    )

    (define prim-cdr-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE CDR--- " nl
          "JUMP(L_cdr_def);" nl
          "L_prim_cdr:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R0, INDD(R0, 2))" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_cdr_def:" nl
        )
      )
    )

    (define prim-cons-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE CONS--- " nl
          "JUMP(L_cons_def);" nl
          "L_prim_cons:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R1, FPARG(3));" nl
           "PUSH(R1);" nl
           "PUSH(R0);" nl
           "CALL(MAKE_SOB_PAIR);" nl
           "DROP(2);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_cons_def:" nl
        )
      )
    )


    (define prim-set-cdr-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE SET-CDR--- " nl
          "JUMP(L_set_cdr_def);" nl
          "L_prim_set_cdr:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R1, FPARG(3));" nl
           "MOV(INDD(R0, 2), R1)" nl
           "MOV(R0, IMM(SOB_VOID));" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_set_cdr_def:" nl
        )
      )
    )

    (define prim-set-car-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE SET-CAR--- " nl
          "JUMP(L_set_car_def);" nl
          "L_prim_set_car:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R1, FPARG(3));" nl
           "MOV(INDD(R0, 1), R1)" nl
           "MOV(R0, IMM(SOB_VOID));" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_set_car_def:" nl
        )
      )
    )



    ;;CHAR;;

    (define prim-char-to-integer-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE CHAR->INTEGER --- " nl
          "JUMP(L_char_to_int_def);" nl
          "L_prim_char_to_int:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R0, INDD(R0, 1));" nl
           "PUSH(R0);" nl
           "CALL(MAKE_SOB_INTEGER);" nl
           "DROP(1);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_char_to_int_def:" nl
        )
      )
    )


    (define prim-integer-to-char-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE INTEGER->CHAR --- " nl
          "JUMP(L_int_to_char_def);" nl
          "L_prim_int_to_char:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R0, INDD(R0, 1));" nl
           "PUSH(R0);" nl
           "CALL(MAKE_SOB_CHAR);" nl
           "DROP(1);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_int_to_char_def:" nl
        )
      )
    )



    ;;STRING;;

    (define prim-make-string-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE MAKE-STRING --- " nl
          "JUMP(L_make_string_def);" nl
          "L_prim_make_string:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(1));" nl
           "MOV(R1, FPARG(2));" nl
           "MOV(R1, INDD(R1, 1));" nl
           "CMP(R0, IMM(1));" nl
           "JUMP_EQ(L_make_string_1_arg);" nl
           "MOV(R2, FPARG(3));" nl
           "MOV(R2, INDD(R2, 1));" nl
           "CMP(R1, IMM(0));" nl
           "JUMP_EQ(L_make_string_finish);" nl

           "L_make_string_2_args:" nl
           "PUSH(R2);" nl
           "DECR(R1);" nl
           "CMP(R1, IMM(0));" nl
           "JUMP_NE(L_make_string_2_args);" nl
     			 "JUMP(L_make_string_finish);" nl

     			 "L_make_string_1_arg:" nl
     			 "CMP(R1, IMM(0));" nl
     			 "JUMP_EQ(L_make_string_finish);" nl

     			 "L_make_string_1_arg_push:" nl
     			 "PUSH(IMM(0));" nl
     			 "DECR(R1);" nl
     			 "CMP(R1, IMM(0));" nl
     			 "JUMP_NE(L_make_string_1_arg_push);" nl

           "L_make_string_finish:" nl
           "MOV(R1, FPARG(2));" nl
           "MOV(R2, INDD(R1, 1));" nl
           "PUSH(R2); //R2=SIZE OF STRING" nl
           "CALL(MAKE_SOB_STRING);" nl
           "DROP(INDD(FPARG(2), 1) + 1);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_make_string_def:" nl
        )
      )
    )


    (define prim-string-length-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE STRING LENGTH--- " nl
          "JUMP(L_string_length_def);" nl
          "L_prim_string_length:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R0, INDD(R0, 1));" nl
           "PUSH(R0);" nl
           "CALL(MAKE_SOB_INTEGER);" nl
           "DROP(1);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_string_length_def:" nl
        )
      )
    )

    (define prim-string-ref-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE STRING REF--- " nl
          "JUMP(L_string_ref_def);" nl
          "L_prim_string_ref:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R1, FPARG(3));" nl
           "MOV(R1, INDD(R1, 1));" nl
           "ADD(R1, IMM(2));" nl
           "MOV(R2, INDD(R0, R1));" nl
           "PUSH(R2);" nl
           "CALL(MAKE_SOB_CHAR);" nl
           "DROP(1);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_string_ref_def:" nl
        )
      )
    )


    (define prim-string-set-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE STRING SET--- " nl
          "JUMP(L_string_set_def);" nl
          "L_prim_string_set:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(3));" nl
           "MOV(R0, INDD(R0, 1));" nl
           "ADD(R0, IMM(2));" nl
           "MOV(R1, FPARG(4));" nl
           "MOV(R1, INDD(R1, 1));"
           "MOV(INDD(FPARG(2), R0), R1);" nl
           "MOV(R0, SOB_VOID);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_string_set_def:" nl
        )
      )
    )

    ; (define prim-string-to-sym-code
    ;   (lambda ()
    ;     (string-append
    ;       "// --- PRIMITIVE STRING-TO-SYM --- " nl
    ;       "JUMP(L_string_to_symbol_cont);" nl
    ; 			"L_string_to_symbol:" nl
    ;   		 "PUSH(FP); " nl
    ;   		 "MOV(FP, SP); "  nl
    ;   		 "MOV(R1,FPARG(2));" nl
    ;   		 "MOV(R2,0);" nl
    ; 		    "L_str_to_sym_loop:" nl
    ; 		   "CMP(INDD(506,R2),T_SYMBOL);" nl
    ; 		   "JUMP_NE(L_str_to_sym_loop_end);" nl
    ; 		   "CMP(INDD(R2,507),R1);"nl
    ; 		   "JUMP_EQ(L_str_to_sym_find);" nl
    ; 			"L_str_to_sym_loop_end:"
    ; 		   "CMP(R2,IND(3));" nl
    ; 		   "ADD(R2,IMM(1));" nl
    ; 		   "JUMP_LT(L_str_to_sym_loop);" nl
    ; 			 "MOV(R2,IND(4))" nl
    ; 			 "LOOP_LINKED_LIST:" nl
    ; 		   "CMP(INDD(R2,2),0);" nl
    ; 		   "JUMP_EQ(L_str_to_sym_mall);" nl
    ; 		   "CMP(INDD(R2,2),R1);" nl
    ; 		   "JUMP_NE(L_str_to_sym_lend);" nl
    ; 		   "MOV(R0,R2);" nl
    ; 		   "ADD(R0,1);" nl
    ; 		   "JUMP(string_to_symbol_exit_label);" nl
    ; 			 "L_str_to_sym_lend:" nl
    ; 		   "MOV(R2,IND(R2));" nl
    ; 		   "JUMP(LOOP_LINKED_LIST);" nl
    ; 			"L_str_to_sym_mall:" nl
    ; 		   "PUSH(IMM(3));" nl
    ; 		   "CALL(MALLOC);" nl
    ; 		   "DROP(1);" nl
    ; 		   "MOV(INDD(R2,0),R0);" nl
    ; 		   "MOV(INDD(R2,1),T_SYMBOL);" nl
    ; 		   "MOV(INDD(R2,0),R1);" nl
    ; 		   "MOV(R0,R1);" nl
    ; 		   "ADD(R0,1);" nl
    ; 		   "JUMP(string_to_symbol_exit_label);" nl
    ; 			"L_str_to_sym_find:" nl
    ; 		   "MOV(R0,R2);" nl
    ; 		   "ADD(R0,506);" nl
    ; 			"string_to_symbol_exit_label:" nl
    ; 			 "MOV(R0, IND(R0));" nl
    ; 		 	 "POP(FP);" nl
    ; 		   "RETURN; " nl

    ;        "L_string_to_symbol_cont:" nl


    ;     )
    ;   )
    ; )
    ;;(define label-str-to-sym-code "L_Prim_str_to_sym_code")
(define label-str-to-sym-loop "L_Prim_str_to_sym_loop")
(define label-str-to-sym-done "L_Prim_str_to_sym_done")
(define label-str-to-sym-new-sym "L_Prim_str_to_sym_new_sym")

       (define prim-string-to-sym-code
      (lambda ()
        (string-append
"  /* string->symbol code  */" nl
     "  /* Magic symbol definition */" nl
       "  MOV(IND(7), T_SYMBOL);" nl
       "  MOV(IND(8), -1);" nl
       "  MOV(IND(9),"(number->string (get-first-sym-addr constTable))");" nl
       "  #define MAGIC_SYMBOL 7" nl
       " JUMP(L_string_to_symbol_cont);" nl
       "L_string_to_symbol:" nl
       "  PUSH(FP);" nl
       "  MOV(FP,SP);" nl
       "  PUSH(R1);" nl
       "  PUSH(R2);" nl
       "  PUSH(R3);" nl
       "  PUSH(R4);" nl
       "  MOV(R4, MAGIC_SYMBOL); //R4 now points to the magic symbol" nl
       "  MOV(R1, INDD(R4,2)); //R1 now points to the first real symbol" nl
       "  MOV(R2, FPARG(2)); //R2 now holds the address of the parameter string" nl
       label-str-to-sym-loop":" nl
       "  CMP(R1,IMM(-1)); //Check if we are not at the last symbol" nl
       "  JUMP_EQ("label-str-to-sym-new-sym"); //If we are - there are no more symbols in the list, so create a new symbol." nl
       "  MOV(R3,INDD(R1,1)); //If the symbol exists - point R3 to its string" nl
       "  MOV(R0,R1); //And point R0 to the symbol itself (in preperation for returning)" nl
       "  CMP(R3,R2); //compare the string addresses of the current symbol and the parameter" nl
       "  JUMP_EQ("label-str-to-sym-done"); //If they are the same - we have found what we were looking for, so finish" nl
       "  MOV(R4, R1); //Otherwise, save the current symbol address in R4" nl
       "  MOV(R1, INDD(R1,2)); //and move R1 to the next symbol" nl
       "  JUMP("label-str-to-sym-loop");" nl
       label-str-to-sym-new-sym":" nl
       "  PUSH(IMM(3)); //Prepare to allocate memory for a new symbol" nl
       "  CALL(MALLOC); //Allocating memory, the address is in R0" nl
       "  DROP(1);" nl
       "  MOV(INDD(R4,2),IMM(R0)); //Update the pointer of the previous symbol to point to the new symbol" nl
       "  MOV(IND(R0), T_SYMBOL) //type" nl
       "  MOV(INDD(R0,1), IMM(R2)); //pointer to the string" nl
       "  MOV(INDD(R0,2), IMM(-1)); //pointer to the \"next\" symbol - but there is no next symbol, so it's -1" nl
       label-str-to-sym-done":" nl
       "  POP(R4);" nl
       "  POP(R3);" nl
       "  POP(R2);" nl
       "  POP(R1);" nl
       "  POP(FP);" nl
       "  RETURN;" nl
       "  /* end of string->symbol code */" nl
       "L_string_to_symbol_cont:" nl
       nl
       )))


       (define prim-sym-to-string-code
         (lambda ()
           (string-append

             "// --- PRIMITIVE SYM-TO-STRING --- " nl
           "JUMP(L_symbol_to_string_cont);" nl
           "L_symbol_to_string:" nl
            "  PUSH(FP);" nl
              "  MOV(FP,SP);" nl
              "  PUSH(R1);" nl
              "  MOV(R1, FPARG(2));" nl
              "  MOV(R0,INDD(R1,1));" nl
              "  POP(R1);" nl
              "  POP(FP);" nl
              "  RETURN;" nl
              "L_symbol_to_string_cont:" nl
           )
         )
       )


(define prim-vector-code
    (lambda()

           (string-append
                          " /* vector mofo implemntation */ " nl
                         "JUMP(L_vector_cont);" nl
                          "L_prim_my_vector:" nl
                           "PUSH (FP);" nl
                           "MOV(FP, SP);" nl
                           "MOV(R1,FPARG(1));" nl ;
                           "DECR(R1);" nl
                           "MOV(R2,0);" nl
                           "MOV(R10,2);" nl
                           "L_vector_mofo_loop:"  nl
                           "CMP(R2,R1);" nl
                           "JUMP_EQ(L_vector_mofo_loop_end );"nl
                           "MOV(R3,FPARG(R10));" nl
                           "PUSH(R3);"nl
                           "ADD(R10,1);"nl
                           "ADD(R2,1);"nl
                           "JUMP(L_vector_mofo_loop );"nl
                           "L_vector_mofo_loop_end:"nl
                           "PUSH(R1);"nl
                           "CALL(MAKE_SOB_VECTOR);"nl
                           "POP(R1);"nl
                           "DROP(R1);" nl
                           "POP(FP);"nl
                           "RETURN;"nl

                           "L_vector_cont:" nl
                          )
    ))
    ;VECTOR;;
; (define prim-vector-code
;   (lambda ()
;     (string-append
;       "/* primitive vector?  */" nl
;       "JUMP(L_vector_cont);" nl
;       "L_prim_my_vector:" nl
;        "PUSH(FP);" nl
;        "MOV(FP, SP);" nl
;        "PUSH(FPARG(2)); //param" nl
;        "CALL(IS_SOB_VECTOR);" nl
;        "DROP(1);" nl
;        "CMP(R0, IMM(1));" nl
;        "JUMP_EQ(L_vector_True);" nl
;        "MOV(R0, SOB_FALSE);" nl
;        "JUMP(L_vector_Exit);" nl
;        "L_vector_True:" nl
;        "MOV(R0, SOB_TRUE);" nl
;        "L_vector_Exit:" nl
;        "POP(FP);" nl
;        "RETURN;" nl
;       "L_vector_cont:" nl
;        ; "MOV(IND(28), IMM(T_CLOSURE));" nl
;        ; "MOV(IND(29), IMM(615181));" nl
;        ; "MOV(IND(30), LABEL(L_prim_my_vector));" nl
;       "#define SOB_PRIM_VECTOR 28" nl
;     )
;   )
; )

    (define prim-make-vector-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE MAKE-VECTOR --- Iluhim eshmor  " nl
          "JUMP(L_make_vector_def);" nl
          "L_prim_make_vector:" nl
          "PUSH(FP);" nl
       "MOV(FP, SP);" nl
       "MOV(R0, FPARG(1)); " nl
       "DECR(R0); " nl
       "MOV(R1, INDD(FPARG(2),1)); //size of new vector" nl
       "CMP(R0, IMM(1));" nl
       "JUMP_EQ(L_makeVec_oneArg);" nl
      "/* didn't jump, fill the vector with the second arg */" nl
       "/* Mayer ill kill you */" nl
       "MOV(R2, FPARG(3)); //the value" nl
       "CMP(R1, IMM(0));" nl
       "JUMP_EQ(L_makeVec_Exit);" nl
      "L_makeVec_twoArgs_BeginPush:" nl
       "PUSH(R2);" nl
       "DECR(R1);" nl
       "CMP(R1, IMM(0));" nl
       "JUMP_NE(L_makeVec_twoArgs_BeginPush);" nl
       "JUMP(L_makeVec_Exit);" nl
      "L_makeVec_oneArg:" nl
       "CMP(R1, IMM(0));" nl
       "JUMP_EQ(L_makeVec_Exit);" nl
      "L_makeVec_oneArg_BeginPush:" nl
       "PUSH(IMM(0));" nl
       "CALL(MAKE_SOB_INTEGER);" nl
       "DROP(1);" nl
       "PUSH(R0)" nl
       "DECR(R1);" nl
       "CMP(R1, IMM(0));" nl
       "JUMP_NE(L_makeVec_oneArg_BeginPush);" nl
      "L_makeVec_Exit:" nl

       "MOV(R1, FPARG(2));" nl
       "PUSH(INDD(R1,1)); //size of vector" nl
       "CALL(MAKE_SOB_VECTOR);" nl
       "DROP(INDD(FPARG(2),1) + 1);" nl
       "POP(FP);" nl
       "RETURN;" nl
      "L_makeVec_cont:" nl
    ;   "MOV(IND(70), IMM(T_CLOSURE));" nl
     ;  "MOV(IND(71), IMM(400183));" nl
      ; "MOV(IND(72), LABEL(L_prim_make_vector));" nl
      "#define SOB_PRIM_MAKE_VECTOR 70" nl

           "L_make_vector_def:" nl
        )
      )
    )


    ; (define prim-make-vector-code
    ;   (lambda ()
    ;     (string-append
    ;       "// --- PRIMITIVE MAKE-VECTOR --- " nl
    ;       "JUMP(L_make_vector_def);" nl
    ;       "L_prim_make_vector:" nl
    ;        "PUSH(FP);" nl
    ;        "MOV(FP,SP);" nl
    ;        "MOV(R0, FPARG(1));" nl
    ;        "CMP(R0,IMM(1));" nl
    ;        "JUMP_EQ(L_make_vector_1_arg);" nl

    ;        "MOV(R2,FPARG(3));" nl
    ;        "JUMP(L_make_vector_cont);" nl

    ;        "L_make_vector_1_arg:"
    ;        "PUSH(0);" nl
    ;        "CALL(MAKE_SOB_INTEGER);" nl
    ;        "DROP(1);" nl
    ;        "MOV(R2, R0);" nl

    ;        "L_make_vector_cont:"
    ;        "MOV(R0, FPARG(2));"
    ;        "MOV(R3, INDD(R0, 1));" nl
    ;        "MOV(R4,R3);" nl

    ;        "L_make_vector_loop:"
    ;        "CMP(R3, IMM(0));" nl
    ;        "JUMP_EQ(L_make_vector_finish);" nl
    ;        "PUSH(IMM(R2));" nl
    ;        "DECR(R3);" nl
    ;        "JUMP(L_make_vector_loop);" nl

    ;        "L_make_vector_finish:"
    ;        "PUSH(R4);" nl
    ;        "CALL(MAKE_SOB_VECTOR);" nl
    ;        "POP(R1);" nl
    ;        "DROP(R1);" nl
    ;        "POP(FP);" nl
    ;        "RETURN;" nl

    ;        "L_make_vector_def:" nl
    ;     )
    ;   )
    ; )


    (define prim-vector-length-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE VECTOR LENGTH--- " nl
          "JUMP(L_vector_length_def);" nl
          "L_prim_vector_length:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R0, INDD(R0, 1));" nl
           "PUSH(R0);" nl
           "CALL(MAKE_SOB_INTEGER);" nl
           "DROP(1);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_vector_length_def:" nl
        )
      )
    )

    (define prim-vector-ref-code
      (lambda ()
        (string-append
           "// --- PRIMITIVE VECTOR REF--- " nl
           "JUMP(L_vector_ref_def);" nl
           "L_prim_vector_ref:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(2));" nl
           "MOV(R1, FPARG(3));" nl
           "MOV(R1, INDD(R1, 1));" nl
           "ADD(R1, IMM(2));" nl
           "MOV(R0, INDD(R0, R1));" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_vector_ref_def:" nl
        )
      )
    )


    (define prim-vector-set-code
      (lambda ()
        (string-append
          "// --- PRIMITIVE VECTOR SET--- " nl
          "JUMP(L_vector_set_def);" nl
          "L_prim_vector_set:" nl
           "PUSH(FP);" nl
           "MOV(FP,SP);" nl
           "MOV(R0, FPARG(3));" nl
           "MOV(R0, INDD(R0, 1));" nl
           "ADD(R0, IMM(2));" nl
           "MOV(R1, FPARG(4));" nl
           "MOV(INDD(FPARG(2), R0), R1);" nl
           "MOV(R0, SOB_VOID);" nl
           "POP(FP);" nl
           "RETURN;" nl

           "L_vector_set_def:" nl
        )
      )
    )

    (define gcdFunc
        (lambda (func)
        (string-append

            "L_gcd_loop_" func ":" nl
            "MOV(R3, R1);" nl
            "REM(R3, R2);" nl
            "CMP(R3, IMM(0));" nl
            "JUMP_EQ(L_gcd_" func "_finish);" nl
            "MOV(R1, R2);" nl
            "MOV(R2, R3);" nl
            "JUMP(L_gcd_loop_" func ");" nl

            "L_gcd_" func "_finish:" nl
            )))


      ;;MATH OPS;;




      (define prim-plus-code
        (lambda ()
          (string-append
            "// --- PRIMITIVE PLUS + --- " nl
             "JUMP(L_plus_def);" nl
             "L_prim_plus:" nl
             "PUSH(FP);" nl
             "MOV(FP,SP);" nl
             "MOV(R10,FPARG(1)); //number of elements" nl
             "DECR(R10); //exclude magic box " nl
              "CMP(R10,0);" nl
              "PUSH(IMM(0));"nl
              "CALL(MAKE_SOB_INTEGER);"nl
              "DROP(1);"nl
              "JUMP_EQ(prim_plus_finish);" nl
              "DECR(R10); // R10 holds number of loops " nl
              "MOV(R11,IMM(3));" nl
            "MOV(R0,FPARG(2)); //first element" nl

             "L_plus_begin_loop:" nl

            "CMP(R10,IMM(0)); " nl
            "JUMP_EQ(prim_plus_finish);" nl

             "CMP(IND(R0), T_INTEGER);" nl
             "JUMP_EQ(prim_plus_1st_arg_int);" nl

             "prim_plus_1st_arg_frac:" nl
             "MOV (R1,INDD(R0,1));" nl
             "MOV (R2,INDD(R0,2));" nl
             "JUMP(prim_plus_check_2nd_arg);" nl

             "prim_plus_1st_arg_int:" nl
             "MOV (R1, INDD(R0, 1));" nl
             "MOV (R2, 1);" nl



             "prim_plus_check_2nd_arg:" nl
             "MOV(R6, FPARG(R11));" nl
             "CMP(IND(R6), T_INTEGER);" nl
             "JUMP_EQ(prim_plus_2nd_arg_int);" nl

             "prim_plus_2nd_arg_frac:" nl
             "MOV (R3,INDD(R6,1));" nl
             "MOV (R4,INDD(R6,2));" nl
             "JUMP(L_plus_common_don);" nl
             "prim_plus_2nd_arg_int:" nl
             "MOV (R3, INDD(R6, 1));" nl
             "MOV (R4, 1);" nl

            "L_plus_common_don:" nl
             "MUL (R1,R4);" nl
             "MUL (R3,R2);" nl
             "ADD (R1,R3);" nl
             "MUL (R2,R4);" nl

             "MOV (R8,R1);" nl
             "MOV (R9,R2);" nl
             (gcdFunc "plus")
             "DIV (R8,R2);" nl
             "DIV (R9,R2);" nl
             "CMP (R9,IMM(1));" nl
             "JUMP_EQ(prim_plus_make_int);" nl

              "//MAKE_SOB_FRACTION" nl
             "PUSH (IMM(3));" nl
             "CALL (MALLOC);" nl
             "DROP (1);" nl
             "MOV (IND(R0), T_FRACTION);" nl
             "MOV (INDD(R0,1),R8);" nl
             "MOV (INDD(R0,2),R9);" nl
             "JUMP(prim_plus_after_int);" nl

             "prim_plus_make_int:" nl
             "PUSH (R8);" nl
             "CALL (MAKE_SOB_INTEGER);" nl
             "DROP (1);" nl

             "prim_plus_after_int:"

             "INCR(R11);" nl
             "DECR(R10);" nl
             "JUMP(L_plus_begin_loop);" nl

             "prim_plus_finish:" nl
             "POP(FP);" nl
             "RETURN;" nl

             "L_plus_def:" nl
          )
        )
      )

      (define prim-minus-code
        (lambda ()
          (string-append
            "// --- PRIMITIVE MINUS + --- " nl
             "JUMP(L_minus_def);" nl
             "L_prim_minus:" nl
             "PUSH(FP);" nl
             "MOV(FP,SP);" nl
                "/* chheck if there's one param */" nl
             "MOV(R10,FPARG(1));" nl
              "DECR(R10); //exclude magic box " nl
             "CMP(R10,1);" nl
             "JUMP_NE(L_minus_continue);" nl
             "PUSH(0);"nl
             "CALL(MAKE_SOB_INTEGER);"nl
             "DROP(1);" nl
             "MOV (R1, INDD(R0, 1));" nl
             "MOV (R2, 1);" nl
             "MOV(R6,FPARG(2));" nl
             "JUMP(prim_for_one_param_minus);"nl
            ; "MOV(R0,FPARG(2));" nl
            ; "MOV(R0,INDD(R0, 1));"
            ; "MUL (R0 ,IMM(-1));"nl
            ; "PUSH(R0);" nl
            ; "CALL(MAKE_SOB_INTEGER);"
            ; "DROP(1);"
            ; "JUMP(prim_minus_finish);"nl

            "L_minus_continue:" nl
              "MOV(R10,FPARG(1)); //number of elements" nl
             "DECR(R10); //exclude magic box " nl
              "CMP(R10,0);" nl
              "JUMP_EQ(prim_minus_finish);" nl
              "DECR(R10); // R10 holds number of loops " nl
              "MOV(R11,IMM(3));" nl
               "MOV(R0,FPARG(2));" nl

               "L_minus_begin_loop:" nl

             "CMP(R10,IMM(0)); " nl
            "JUMP_EQ(prim_minus_finish);" nl

             "CMP(IND(R0), T_INTEGER);" nl
             "JUMP_EQ(prim_minus_1st_arg_int);" nl

             "prim_minus_1st_arg_frac:" nl
             "MOV (R1,INDD(R0,1));" nl
             "MOV (R2,INDD(R0,2));" nl
             "JUMP(prim_minus_check_2nd_arg);" nl

             "prim_minus_1st_arg_int:" nl
             "MOV (R1, INDD(R0, 1));" nl
             "MOV (R2, 1);" nl


             "prim_minus_check_2nd_arg:" nl
             "MOV(R6, FPARG(R11));" nl
             "prim_for_one_param_minus: //fuck u Niv"  nl
             "CMP(IND(R6), T_INTEGER);" nl
             "JUMP_EQ(prim_minus_2nd_arg_int);" nl

             "prim_minus_2nd_arg_frac:" nl
             "MOV (R3,INDD(R6,1));" nl
             "MOV (R4,INDD(R6,2));" nl
             "JUMP(L_minus_common_don);" nl

             "prim_minus_2nd_arg_int:" nl
             "MOV (R3, INDD(R6, 1));" nl
             "MOV (R4, 1);" nl

          "L_minus_common_don:" nl

             "MUL (R1,R4);" nl
             "MUL (R3,R2);" nl
             "SUB (R1,R3);" nl
             "MUL (R2,R4);" nl

             "MOV (R8,R1);" nl
             "MOV (R9,R2);" nl
             (gcdFunc "minus")
             "DIV (R8,R2);" nl
             "DIV (R9,R2);" nl
             "CMP (R9,IMM(1));" nl
             "JUMP_EQ(prim_minus_make_int);" nl

              "//MAKE_SOB_FRACTION" nl
             "PUSH (IMM(3));" nl
             "CALL (MALLOC);" nl
             "DROP (1);" nl
             "MOV (IND(R0), T_FRACTION);" nl
             "CMP(R9,0);" nl
             "JUMP_GE(prim_minus_nth);"nl
             "MUL (R8,IMM(-1)); // God defend !"nl
              "MUL (R9,IMM(-1));"nl
             "prim_minus_nth: " nl
             "MOV (INDD(R0,1),R8);" nl
             "MOV (INDD(R0,2),R9);" nl
             "JUMP(prim_minus_after_int);" nl

             "prim_minus_make_int:" nl
             "PUSH (R8);" nl
             "CALL (MAKE_SOB_INTEGER);" nl
             "DROP (1);" nl

          "prim_minus_after_int:" nl

            "INCR(R11);" nl
             "DECR(R10);" nl

             "JUMP(L_minus_begin_loop);" nl
             "prim_minus_finish:" nl
             "POP(FP);" nl
             "RETURN;" nl

             "L_minus_def:" nl
          )
        )
      )


      (define prim-mult-code
        (lambda ()
          (string-append
            "// --- PRIMITIVE MULT + --- " nl
             "JUMP(L_mult_def);" nl
             "L_prim_mult:" nl
             "PUSH(FP);" nl
             "MOV(FP,SP);" nl
              "MOV(R10,FPARG(1)); //number of elements" nl
             "DECR(R10); //exclude magic box " nl
              "CMP(R10,0);" nl
              "PUSH(IMM(1));"nl
              "CALL(MAKE_SOB_INTEGER);"nl
              "DROP(1);"nl
              "JUMP_EQ(prim_mult_finish);" nl
              "DECR(R10); // R10 holds number of loops " nl
              "MOV(R11,IMM(3));" nl
             "MOV(R0,FPARG(2));" nl
              "L_mult_begin_loop:" nl

             "CMP(R10,IMM(0)); " nl
              "JUMP_EQ(prim_mult_finish);" nl

             "CMP(IND(R0), T_INTEGER);" nl
             "JUMP_EQ(prim_mult_1st_arg_int);" nl

             "prim_mult_1st_arg_frac:" nl
             "MOV (R1,INDD(R0,1));" nl
             "MOV (R2,INDD(R0,2));" nl
             "JUMP(prim_mult_check_2nd_arg);" nl

             "prim_mult_1st_arg_int:" nl
             "MOV (R1, INDD(R0, 1));" nl
             "MOV (R2, 1);" nl


             "prim_mult_check_2nd_arg:" nl
             "MOV(R6, FPARG(R11));" nl
             "CMP(IND(R6), T_INTEGER);" nl
             "JUMP_EQ(prim_mult_2nd_arg_int);" nl

             "prim_mult_2nd_arg_frac:" nl
             "MOV (R3,INDD(R6,1));" nl
             "MOV (R4,INDD(R6,2));" nl
            "JUMP(L_mult_common_don);" nl

             "prim_mult_2nd_arg_int:" nl
             "MOV (R3, INDD(R6, 1));" nl
             "MOV (R4, 1);" nl

            "L_mult_common_don:" nl
             "MUL (R1,R3);" nl
             "MUL (R2,R4);" nl

             "MOV (R8,R1);" nl
             "MOV (R9,R2);" nl
             (gcdFunc "mult")
             "DIV (R8,R2);" nl
             "DIV (R9,R2);" nl
             "CMP (R9,IMM(1));" nl
             "JUMP_EQ(prim_mult_make_int);" nl

              "//MAKE_SOB_FRACTION" nl
             "PUSH (IMM(3));" nl
             "CALL (MALLOC);" nl
             "DROP (1);" nl
             "MOV (IND(R0), T_FRACTION);" nl
              "CMP(R9,0);" nl
             "JUMP_GE(prim_mult_nth);"nl
             "MUL (R8,IMM(-1)); // God defend !"nl
              "MUL (R9,IMM(-1));"nl
             "prim_mult_nth: " nl
             "MOV (INDD(R0,1),R8);" nl
             "MOV (INDD(R0,2),R9);" nl
             "JUMP(prim_mult_after_int);" nl

             "prim_mult_make_int:" nl
             "PUSH (R8);" nl
             "CALL (MAKE_SOB_INTEGER);" nl
             "DROP (1);" nl


               "prim_mult_after_int:" nl

               "INCR(R11);" nl
             "DECR(R10);" nl

             "JUMP(L_mult_begin_loop);" nl

             "prim_mult_finish:" nl
             "POP(FP);" nl
             "RETURN;" nl

             "L_mult_def:" nl
          )
        )
      )

      (define prim-div-code
        (lambda ()
          (string-append
            "// --- PRIMITIVE DIV + --- " nl
             "JUMP(L_div_def);" nl
             "L_prim_div:" nl
             "PUSH(FP);" nl
             "MOV(FP,SP);" nl
              "MOV(R10,FPARG(1)); //number of elements" nl
             "DECR(R10); //exclude magic box " nl
              "CMP(R10,0);" nl
              "JUMP_EQ(prim_div_finish);" nl
              "DECR(R10); // R10 holds number of loops " nl
              "MOV(R11,IMM(3));" nl
             "MOV(R0,FPARG(2));" nl
              "L_div_begin_loop:" nl

             "CMP(R10,IMM(0)); " nl
              "JUMP_EQ(prim_div_finish);" nl

             "CMP(IND(R0), T_INTEGER);" nl
             "JUMP_EQ(prim_div_1st_arg_int);" nl

             "prim_div_1st_arg_frac:" nl
             "MOV (R1,INDD(R0,1));" nl
             "MOV (R2,INDD(R0,2));" nl
             "JUMP(prim_div_check_2nd_arg);" nl

             "prim_div_1st_arg_int:" nl
             "MOV (R1, INDD(R0, 1));" nl
             "MOV (R2, 1);" nl


             "prim_div_check_2nd_arg:" nl
             "MOV(R6, FPARG(R11));" nl
             "CMP(IND(R6), T_INTEGER);" nl
             "JUMP_EQ(prim_div_2nd_arg_int);" nl

             "prim_div_2nd_arg_frac:" nl
             "MOV (R3,INDD(R6,1));" nl
             "MOV (R4,INDD(R6,2));" nl
              "JUMP(L_div_common_don);" nl

             "prim_div_2nd_arg_int:" nl
             "MOV (R3, INDD(R6, 1));" nl
             "MOV (R4, 1);" nl


            "L_div_common_don:" nl

             "MUL (R1,R4);" nl
             "MUL (R2,R3);" nl

             "MOV (R8,R1);" nl
             "MOV (R9,R2);" nl
             (gcdFunc "div")
             "DIV (R8,R2);" nl
             "DIV (R9,R2);" nl
             "CMP (R9,IMM(1));" nl
             "JUMP_EQ(prim_div_make_int);" nl

              "//MAKE_SOB_FRACTION" nl
             "PUSH (IMM(3));" nl
             "CALL (MALLOC);" nl
             "DROP (1);" nl
             "MOV (IND(R0), T_FRACTION);" nl
                  "CMP(R9,0);" nl
             "JUMP_GE(prim_div_nth);"nl
             "MUL (R8,IMM(-1)); // God defend !"nl
              "MUL (R9,IMM(-1));"nl
             "prim_div_nth: " nl
             "MOV (INDD(R0,1),R8);" nl
             "MOV (INDD(R0,2),R9);" nl
             "JUMP(prim_div_after_int);" nl

             "prim_div_make_int:" nl
             "PUSH (R8);" nl
             "CALL (MAKE_SOB_INTEGER);" nl
             "DROP (1);" nl

          "prim_div_after_int:" nl

               "INCR(R11);" nl
             "DECR(R10);" nl

             "JUMP(L_div_begin_loop);" nl
             "prim_div_finish:" nl
             "POP(FP);" nl
             "RETURN;" nl

             "L_div_def:" nl
          )
        )
      )

  ;
  ;
  ;
  ;  ;;MATH OPS;; numerator, denominator, not, remainder
  ;  (define prim-plus-code
  ;    (lambda ()
  ;      (string-append
  ;        "// --- PRIMITIVE PLUS + --- " nl
  ;        "JUMP(L_plus_def);" nl
  ;        "L_prim_plus:" nl
  ;         "PUSH(FP);" nl
  ;         "MOV(FP,SP);" nl
  ;         "MOV(R0,IMM(0));" nl
  ;         "MOV(R1,FPARG(1)); //R1=NUM OF ELEM" nl
  ;         "DECR(R1);" nl
  ;         "CMP(R1,0);" nl
  ;         "JUMP_EQ(L_plus_finish); //no elements, finish and return R0" nl
  ;         "DECR(R1); // R1=NUM LOOPS " nl
  ;         "MOV(R2,IMM(3));" nl
  ;         "MOV(R0,FPARG(2)); //R0=FIRST ELEM" nl
  ;         "MOV(R0,INDD(R0,1));" nl
  ;
  ;        "L_plus_loop:" nl
  ;         "CMP(R1,IMM(0)); " nl
  ;         "JUMP_EQ(L_plus_finish);" nl
  ;         "MOV(R3,FPARG(R2));//R3=NEXT ELEM" nl
  ;         "MOV(R3,INDD(R3,1));" nl
  ;         "ADD(R0,R3);" nl
  ;         "DECR(R1);" nl
  ;         "INCR(R2);" nl
  ;         "JUMP(L_plus_loop);" nl
  ;
  ;         "L_plus_finish:" nl
  ;         "PUSH(R0);" nl
  ;         "CALL(MAKE_SOB_INTEGER); //create number " nl
  ;         "DROP(1); " nl
  ;         "POP(FP);" nl
  ;         "RETURN;" nl
  ;
  ;         "L_plus_def:" nl
  ;      )
  ;    )
  ;  )
  ;
  ;  (define prim-minus-code
  ;    (lambda ()
  ;      (string-append
  ;        "// --- PRIMITIVE MINUS - --- " nl
  ;        "JUMP(L_minus_def);" nl
  ;        "L_prim_minus:" nl
  ;         "PUSH(FP);" nl
  ;         "MOV(FP,SP);" nl
  ;         "MOV(R0,IMM(0));" nl
  ;         "MOV(R1,FPARG(1)); //R1=NUM OF ELEM" nl
  ;         "DECR(R1);" nl
  ;         "CMP(R1,0);" nl
  ;         "JUMP_EQ(L_minus_finish); //no elements, finish and return R0" nl
  ;         "DECR(R1); // R1=NUM LOOPS " nl
  ;         "MOV(R2,IMM(3));" nl
  ;         "MOV(R0,FPARG(2)); //R0=FIRST ELEM" nl
  ;         "MOV(R0,INDD(R0,1));" nl
  ;
  ;        "L_minus_loop:" nl
  ;         "CMP(R1,IMM(0)); " nl
  ;         "JUMP_EQ(L_minus_finish);" nl
  ;         "MOV(R3,FPARG(R2));//R3=NEXT ELEM" nl
  ;         "MOV(R3,INDD(R3,1));" nl
  ;         "SUB(R0,R3);" nl
  ;         "DECR(R1);" nl
  ;         "INCR(R2);" nl
  ;         "JUMP(L_minus_loop);" nl
  ;
  ;         "L_minus_finish:" nl
  ;         "PUSH(R0);" nl
  ;         "CALL(MAKE_SOB_INTEGER); //create number " nl
  ;         "DROP(1); " nl
  ;         "POP(FP);" nl
  ;         "RETURN;" nl
  ;         "L_minus_def:" nl
  ;      )
  ;    )
  ;  )
  ;
  ;(define prim-div-code
  ;(lambda ()
  ;  (string-append
  ;    "// --- PRIMITIVE DIVIDE / ---" nl
  ;    "JUMP(L_div_def);" nl
  ;    "L_prim_div:" nl
  ;     "PUSH(FP);" nl
  ;     "MOV(FP,SP);" nl
  ;     "MOV(R0,IMM(0));" nl
  ;     "MOV(R1,FPARG(1)); //R1=NUM OF ELEM" nl
  ;     "DECR(R1);" nl
  ;     "CMP(R1,0);" nl
  ;     "JUMP_EQ(L_div_finish); //no elements, finish and return R0" nl
  ;     "DECR(R1); // R1=NUM LOOPS " nl
  ;     "MOV(R2,IMM(3));" nl
  ;     "MOV(R0,FPARG(2)); //R0=FIRST ELEM" nl
  ;     "MOV(R0,INDD(R0,1));" nl
  ;
  ;    "L_div_loop:" nl
  ;     "CMP(R1,IMM(0)); " nl
  ;     "JUMP_EQ(L_div_finish);" nl
  ;     "MOV(R3,FPARG(R2));//R3=NEXT ELEM" nl
  ;     "MOV(R3,INDD(R3,1));" nl
  ;     "DIV(R0,R3);" nl
  ;     "DECR(R1);" nl
  ;     "INCR(R2);" nl
  ;     "JUMP(L_div_loop);" nl
  ;
  ;     "L_div_finish:" nl
  ;     "PUSH(R0);" nl
  ;     "CALL(MAKE_SOB_INTEGER); //create number " nl
  ;     "DROP(1); " nl
  ;     "POP(FP);" nl
  ;     "RETURN;" nl
  ;     "L_div_def:" nl
  ;     )
  ;  )
  ;)
  ;
  ;(define prim-mult-code
  ;(lambda ()
  ;  (string-append
  ;    "// --- PRIMITIVE MULT * ---" nl
  ;    "JUMP(L_mult_def);" nl
  ;    "L_prim_mult:" nl
  ;     "PUSH(FP);" nl
  ;     "MOV(FP,SP);" nl
  ;     "MOV(R0,IMM(0));" nl
  ;     "MOV(R1,FPARG(1)); //R1=NUM OF ELEM" nl
  ;     "DECR(R1);" nl
  ;     "CMP(R1,0);" nl
  ;     "JUMP_EQ(L_mult_finish); //no elements, finish and return R0" nl
  ;     "DECR(R1); // R1=NUM LOOPS " nl
  ;     "MOV(R2,IMM(3));" nl
  ;     "MOV(R0,FPARG(2)); //R0=FIRST ELEM" nl
  ;     "MOV(R0,INDD(R0,1));" nl
  ;
  ;    "L_mult_loop:" nl
  ;     "CMP(R1,IMM(0)); " nl
  ;     "JUMP_EQ(L_mult_finish);" nl
  ;     "MOV(R3,FPARG(R2));//R3=NEXT ELEM" nl
  ;     "MOV(R3,INDD(R3,1));" nl
  ;     "MUL(R0,R3);" nl
  ;     "DECR(R1);" nl
  ;     "INCR(R2);" nl
  ;     "JUMP(L_mult_loop);" nl
  ;
  ;     "L_mult_finish:" nl
  ;     "PUSH(R0);" nl
  ;     "CALL(MAKE_SOB_INTEGER); //create number " nl
  ;     "DROP(1); " nl
  ;     "POP(FP);" nl
  ;     "RETURN;" nl
  ;     "L_mult_def:" nl
  ;     )
  ;  )
  ;)


    (define prim-less-than-code
      (lambda ()
            (string-append
                "//PRIMITIVE LESS-THAN <" nl
                "JUMP(L_less_than_def);" nl
                "L_prim_less_than:" nl
                "PUSH(FP);" nl
          			"MOV(FP,SP);" nl
          			"MOV(R1,FPARG(1)); //R1=NUM OR ARGS" nl
          			"DECR(R1);" nl
          			"MOV(R5,R1); //FOR DROP" nl
          			"DECR(R5);" nl
          			"MOV(R2, FPARG(2)); //R2=FIRST ARG" nl
          			"MOV(R3, FPARG(3)); //R3=SECOND ARG" nl
          			"MOV(R4, IMM(2)); //R4=FPARG INDEX" nl

                "L_less_than_loop:" nl
                "CMP(R5,IMM(0));" nl
                "JUMP_NE(L_smaller_than_next_elem);" nl
                "L_less_than_true:" nl
                "MOV(R0,IMM(SOB_TRUE)); //R0 = T" nl
          			"JUMP(L_less_than_exit);" nl
                "L_less_than_false:" nl
                "MOV(R0,IMM(SOB_FALSE)); //R0 = F" nl
                "L_less_than_exit:" nl
                "POP(FP);" nl
                "RETURN;" nl

                "//CHECK NEXT ELEM://" nl
                "L_smaller_than_next_elem:" nl
                "MOV(R2,FPARG(R4)); // R2=LEFT ELEM" nl
          			"MOV(R2,INDD(R2,IMM(1))); " nl
          			"INCR(R4);" nl
          			"MOV(R3,FPARG(R4)); // R3=RIGHT ELEM" nl
          			"MOV(R3,INDD(R3,IMM(1))); " nl
          			"CMP(R2,R3); " nl
          			"JUMP_GE(L_less_than_false);" nl
          			"DECR(R5);" nl
          			"JUMP(L_less_than_loop);" nl

                ;"//define closure" nl
                "L_less_than_def:" nl
          )))


  (define prim-greater-than-code
    (lambda ()
          (string-append
              "//PRIMITIVE GREATER-THAN >" nl
              "JUMP(L_greater_than_def);" nl
              "L_prim_greater_than:" nl
              "PUSH(FP);" nl
        			"MOV(FP,SP);" nl
        			"MOV(R1,FPARG(1)); //R1=NUM OR ARGS" nl
        			"DECR(R1);" nl
        			"MOV(R5,R1); //FOR DROP" nl
        			"DECR(R5);" nl
        			"MOV(R2, FPARG(2)); //R2=FIRST ARG" nl
        			"MOV(R3, FPARG(3)); //R3=SECOND ARG" nl
        			"MOV(R4, IMM(2)); //R4=FPARG INDEX" nl

              "L_greater_than_loop:" nl
              "CMP(R5,IMM(0));" nl
              "JUMP_NE(L_greater_than_next_elem);" nl
              "L_greater_than_true:" nl
              "MOV(R0,IMM(SOB_TRUE)); //R0 = T" nl
        			"JUMP(L_greater_than_exit);" nl
              "L_greater_than_false:" nl
              "MOV(R0,IMM(SOB_FALSE)); //R0 = F" nl
              "L_greater_than_exit:" nl
              "POP(FP);" nl
              "RETURN;" nl

              "//CHECK NEXT ELEM://" nl
              "L_greater_than_next_elem:" nl
              "MOV(R2,FPARG(R4)); // R2=LEFT ELEM" nl
        			"MOV(R2,INDD(R2,IMM(1))); " nl
        			"INCR(R4);" nl
        			"MOV(R3,FPARG(R4)); // R3=RIGHT ELEM" nl
        			"MOV(R3,INDD(R3,IMM(1))); " nl
        			"CMP(R2,R3); " nl
        			"JUMP_LE(L_greater_than_false);" nl
        			"DECR(R5);" nl
        			"JUMP(L_greater_than_loop);" nl

              ;"//define closure" nl
              "L_greater_than_def:" nl
        )))


  (define prim-equal-to-code
    (lambda ()
          (string-append
              "//PRIMITIVE EQUAL-TO =" nl
              "JUMP(L_equal_to_def);" nl
              "L_prim_equal_to:" nl
              "PUSH(FP);" nl
        			"MOV(FP,SP);" nl
        			"MOV(R0,FPARG(2));" nl
              "MOV(R1, FPARG(3));" nl
              ;"PUSH(R0);" nl
              ;"PUSH(R1);" nl
              ;"CALL(L_prim_minus);" nl
              ;"DROP(2);" nl
              "MOV(R0,INDD(R0,1));"  nl
              "MOV(R1,INDD(R1,1));"  nl
              "SUB(R0, R1);" nl
              ;"MOV(R0, INDD(R0, 1));" nl
              "CMP(R0, IMM(0));" nl
              "JUMP_NE(L_equal_to_false);" nl
              "L_equal_to_true:" nl
              "MOV(R0, IMM(SOB_TRUE));" nl
              "JUMP(L_equal_to_finish);" nl

              "L_equal_to_false:" nl
              "MOV(R0, IMM(SOB_FALSE));" nl

              "L_equal_to_finish:" nl
              "POP(FP);" nl
              "RETURN;" nl

              "L_equal_to_def:" nl
        )))

  (define prim-numerator-code
    (lambda ()
      (string-append
        "// --- PRIMITIVE NUMERATOR --- " nl
        "JUMP(L_numerator_def);" nl
        "L_prim_numerator:" nl
         "PUSH(FP);" nl
         "MOV(FP,SP);" nl
         "MOV(R0, FPARG(2));" nl
         "MOV(R0, INDD(R0, 1));" nl
         "PUSH(R0);" nl
         "CALL(MAKE_SOB_INTEGER);" nl
         "DROP(1);" nl
         "POP(FP);" nl
         "RETURN;" nl

         "L_numerator_def:" nl
      )
    )
  )

  (define prim-denominator-code
    (lambda ()
      (string-append
        "// --- PRIMITIVE DENOMINATOR --- " nl
        "JUMP(L_denominator_def);" nl
        "L_prim_denominator:" nl
         "PUSH(FP);" nl
         "MOV(FP,SP);" nl
         "MOV(R0, FPARG(2));" nl
         "MOV(R0, IND(R0));" nl
         "CMP(R0, IMM(T_INTEGER)); //check if the number is not a fraction" nl
         "JUMP_EQ(L_denominator_whole_number);" nl

         "L_denominator_not_whole_number:" nl
         "MOV(R0, FPARG(2));" nl
         "MOV(R0, INDD(R0, 2));" nl
         "PUSH(R0);" nl
         "JUMP(L_denominator_finish);" nl

         "L_denominator_whole_number:" nl
         "PUSH(IMM(1));" nl

         "L_denominator_finish:" nl
         "CALL(MAKE_SOB_INTEGER);" nl
         "DROP(1);" nl
         "POP(FP);" nl
         "RETURN;" nl

         "L_denominator_def:" nl
      )
    )
  )

  (define prim-remainder-code
    (lambda ()
      (string-append
        "// --- PRIMITIVE REMAINDER --- " nl
        "JUMP(L_remainder_def);" nl
        "L_prim_remainder:" nl
         "PUSH(FP);" nl
         "MOV(FP,SP);" nl
         "MOV(R0, FPARG(2));" nl
         "MOV(R0, INDD(R0, 1));" nl
         "MOV(R1, FPARG(3));" nl
         "MOV(R1, INDD(R1, 1));" nl
         "REM(R0, R1);" nl
         "PUSH(R0);" nl
         "CALL(MAKE_SOB_INTEGER);" nl
         "DROP(1);" nl
         "POP(FP);" nl
         "RETURN;" nl

         "L_remainder_def:" nl
      )
    )
  )

  (define prim-not-code
    (lambda ()
      (string-append
        "// --- PRIMITIVE NOT --- " nl
        "JUMP(L_not_def);" nl
        "L_prim_not:" nl
         "PUSH(FP);" nl
         "MOV(FP,SP);" nl
         "MOV(R0, FPARG(2));" nl
         "MOV(R0, INDD(R0, 1));" nl
         "CMP(R0, IMM(0));" nl
         "JUMP_EQ(L_not_make_true);" nl

         "L_not_make_false:"
         "MOV(R0, IMM(0));" nl
         "PUSH(R0);"
         "CALL(MAKE_SOB_BOOL);"
         "JUMP(L_not_finish);"

         "L_not_make_true:"
         "MOV(R0, IMM(1));" nl
         "PUSH(R0);" nl
         "CALL(MAKE_SOB_BOOL);"

         "L_not_finish:" nl
         "DROP(1);" nl
         "POP(FP);" nl
         "RETURN;" nl

         "L_not_def:" nl
      )
    )
  )


;;Mayer's code
(define file->string
  (lambda (in-file)
	(let ((in-port (open-input-file in-file)))
	 (letrec ((run
			(lambda ()
				(let ((ch (read-char in-port)))
					(if (eof-object? ch)
						(begin
							(close-input-port in-port)
								'())
					(cons ch (run)))))))
			(list->string
(run))))))

;;enhanced proc changed read-char to char
;; returns a lst of exprs
(define file->exprs-lst-orig
  (lambda (in-file)
	(let ((in-port (open-input-file in-file)))
	 (letrec ((run
			(lambda ()
				(let ((ch (read in-port)))
					(if (eof-object? ch)
						(begin
							(close-input-port in-port)
								'())
					(cons ch (run)))))))
			;(list->string
(run)))));)

(define file->exprs-lst
  (lambda (filename)
    (let ((input (open-input-file filename))
          (support (open-input-file "scheme-support.scm")))
      (letrec ((run-input
          (lambda ()
            (let ((e (read input)))
            (if (eof-object? e)
              (begin (close-input-port input)
                '())
              (cons e (run-input))
              )
            ))
          )
        (run-support
          (lambda ()
            (let ((e (read support)))
            (if (eof-object? e)
              (begin (close-input-port support)
                '())
              (cons e (run-support))
              )
            ))
          )
      )
      (append (run-support) (run-input)))
    )
  )
)


(define write-to-file
  (lambda (output-str target-file)
    (if (file-exists? target-file) (delete-file target-file))
    (let  ((p (open-output-file target-file)))
      (display output-str p)
      (close-output-port p)
    )))

    (define prologue (lambda()
      (string-append
          "#include <stdio.h>" nl
          "#include <stdlib.h>" nl
          "#include <string.h>" nl

          "#include \"arch/cisc.h\"" nl

          "int main()" nl
          "{" nl
            "START_MACHINE;" nl
            "JUMP(CONTINUE);" nl

            "#include \"arch/char.lib\"" nl
            "#include \"arch/io.lib\"" nl
            "#include \"arch/math.lib\"" nl
            "#include \"arch/string.lib\"" nl
            "#include \"arch/system.lib\"" nl
            "#include \"arch/scheme.lib\"" nl
            "#include <string.h>" nl ;this is for memcpy

             "CONTINUE:" nl

             "  #define SOB_FALSE "(number->string (car (find-in-table  #f constTable 2))) nl
             "  #define SOB_VOID "(number->string (car (find-in-table  void-obj constTable 2))) nl
             "  #define SOB_NIL "(number->string (car (find-in-table  '() constTable 2))) nl
             "  #define SOB_TRUE "(number->string (car (find-in-table  #t constTable 2))) nl

             ;; should be more shits here
             (alloc-const-table)
             (alloc-global-table)


             ;predicates
             (prim-number?-code)
             (closure-gen-prim 'number? "L_prim_number" Global-table)
             (prim-integer?-code)
             (closure-gen-prim 'integer? "L_prim_integer" Global-table)
             (prim-procedure?-code)
             (closure-gen-prim 'procedure? "L_prim_procedure" Global-table)
             (prim-rational?-code)
             (closure-gen-prim 'rational? "L_prim_rational" Global-table)
             (prim-char?-code)
             (closure-gen-prim 'char? "L_prim_char" Global-table)
             (prim-symbol?-code)
             (closure-gen-prim 'symbol? "L_prim_symbol" Global-table)
             (prim-string?-code)
             (closure-gen-prim 'string? "L_prim_string" Global-table)
             (prim-vector?-code)
             (closure-gen-prim 'vector? "L_prim_vector" Global-table)
             (prim-null?-code)
             (closure-gen-prim 'null? "L_prim_null" Global-table)
             (prim-boolean?-code)
             (closure-gen-prim 'boolean? "L_prim_boolean" Global-table)
             (prim-zero?-code)
             (closure-gen-prim 'zero? "L_prim_zero" Global-table)
             (prim-pair?-code)
             (closure-gen-prim 'pair? "L_prim_pair" Global-table)
             (prim-eq?-code)
             (closure-gen-prim 'eq? "L_prim_eq" Global-table)


             ;;pair
             (prim-car-code)
             (closure-gen-prim 'car "L_prim_car" Global-table)
             (prim-cdr-code)
             (closure-gen-prim 'cdr "L_prim_cdr" Global-table)
             (prim-cons-code)
             (closure-gen-prim 'cons "L_prim_cons" Global-table)
             (prim-set-car-code)
             (closure-gen-prim 'set-car! "L_prim_set_car" Global-table)
             (prim-set-cdr-code)
             (closure-gen-prim 'set-cdr! "L_prim_set_cdr" Global-table)


             ;;char
             (prim-char-to-integer-code)
             (closure-gen-prim 'char->integer "L_prim_char_to_int" Global-table)
             (prim-integer-to-char-code)
             (closure-gen-prim 'integer->char "L_prim_int_to_char" Global-table)


             ;;string
             (prim-make-string-code)
             (closure-gen-prim 'make-string "L_prim_make_string" Global-table)
             (prim-string-length-code)
             (closure-gen-prim 'string-length "L_prim_string_length" Global-table)
             (prim-string-ref-code)
             (closure-gen-prim 'string-ref "L_prim_string_ref" Global-table)
             (prim-string-set-code)
             (closure-gen-prim 'string-set! "L_prim_string_set" Global-table)
             (prim-string-to-sym-code)
             (closure-gen-prim 'string->symbol "L_string_to_symbol" Global-table)
             (prim-sym-to-string-code)
             (closure-gen-prim 'symbol->string "L_symbol_to_string" Global-table)


             ;;vector
             (prim-make-vector-code)
             (closure-gen-prim 'make-vector "L_prim_make_vector" Global-table)
              (prim-vector-code)
              (closure-gen-prim 'vector "L_prim_my_vector" Global-table)
             (prim-vector-length-code)
             (closure-gen-prim 'vector-length "L_prim_vector_length" Global-table)
             (prim-vector-ref-code)
             (closure-gen-prim 'vector-ref "L_prim_vector_ref" Global-table)
             (prim-vector-set-code)
             (closure-gen-prim 'vector-set! "L_prim_vector_set" Global-table)


             ;mathematic operations
             (prim-plus-code)
             (closure-gen-prim  '+ "L_prim_plus" Global-table)
             (prim-minus-code)
             (closure-gen-prim  '- "L_prim_minus" Global-table)
             (prim-div-code)
             (closure-gen-prim  '/ "L_prim_div" Global-table)
             (prim-mult-code)
             (closure-gen-prim  '* "L_prim_mult" Global-table)
             (prim-not-code)
             (closure-gen-prim  'not "L_prim_not" Global-table)
             (prim-less-than-code)
             (closure-gen-prim '< "L_prim_less_than" Global-table)
             (prim-greater-than-code)
             (closure-gen-prim '> "L_prim_greater_than" Global-table)
             (prim-equal-to-code)
             (closure-gen-prim '= "L_prim_equal_to" Global-table)
             (prim-numerator-code)
             (closure-gen-prim  'numerator "L_prim_numerator" Global-table)
             (prim-denominator-code)
             (closure-gen-prim  'denominator "L_prim_denominator" Global-table)
             (prim-remainder-code)
             (closure-gen-prim  'remainder "L_prim_remainder" Global-table)


            (prim-apply-code)
            (closure-gen-prim  'apply "L_prim_apply" Global-table)


             )))

    (define place-prim-ptr
      (lambda (prim-name prim-addr global-table)
        (if (find-in-global-table prim-name global-table)
        (let ((fvar-addr-str (number->string (find-in-global-table prim-name global-table))))
         ; (display prim-addr)
          (string-append
           "  MOV(IND("fvar-addr-str"),IMM("(number->string prim-addr)"));" nl
           ))
           ""
           )))


    ;;check it again!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
    (define closure-gen-prim
            (lambda (prim-name code-label global-table)
            (cond ((null? global-table) "")
                  ;;((eq? (assoc-i prim-name fvar-table 2) #f) "")
                  (else
                    (let* ((addr (get-next-addr))
                          (env-str "0502207047")
                          )

            (string-append
              "/* closure defintion of "(symbol->string prim-name)"*/" nl
              "  MOV(IND("(number->string addr)"), T_CLOSURE); //type" nl
              "  MOV(IND("(number->string (+ addr 1))"), "env-str"); //env" nl
              "  MOV(IND("(number->string (+ addr 2))"), LABEL("code-label")); //code-address" nl
              (place-prim-ptr prim-name addr global-table)
               "/* end of closure defintion of "(symbol->string prim-name)"*/" nl
               )
            )))))

(define ^next+3
  (lambda (first)
    (let ((n first))
      (lambda ()
        (set! n (+ n 3))
        n))))

(define get-next-addr (^next+3 50))

(define printing-func
  (lambda (assm-fun-str)
    (let ((label-content (^label-print-content))
        (label-exit (^label-print-exit )))
      (string-append
        assm-fun-str
        "CMP(IND(R0), T_VOID);" nl
        "JUMP_NE(" label-content ");" nl
        "NOP;" nl
        "JUMP(" label-exit ");" nl
        label-content ":" nl
        "PUSH(R0);" nl
          "CALL(WRITE_SOB);" nl
          "CALL(NEWLINE);" nl
          "DROP(1);" nl
          label-exit ":" nl
      )
    )
  )
)

(define epilogue
  (lambda ()
    (string-append
       ;   "  /* printing the content of R0 */" nl
       ; "  CMP(R0, SOB_VOID);" nl
       ; "  JUMP_EQ(dont_print);" nl
       ; "  PUSH(R0);" nl
       ; "  CALL(WRITE_SOB);" nl
       ; "  CALL(NEWLINE);" nl
       ; "  DROP(1);" nl
       ; "dont_print:" nl
       ; "  /* done printing the content of R0 */" nl
       "STOP_MACHINE;" nl
        "return 0;" nl
      "}" nl

    )
  )
)



(define compile-scheme-file(lambda(source-file target-file)
	(let* (
		(exprs (file->exprs-lst source-file))
		(pes (map (lambda(sub-exp) (annotate-tc (pe->lex-pe (box-set
		(remove-applic-lambda-nil (eliminate-nested-defines(parse sub-exp)))))))
		 exprs))
    ; (pes (map (lambda(sub-exp) (pe->lex-pe (box-set
    ; (remove-applic-lambda-nil (eliminate-nested-defines(parse sub-exp))))))
    ;  exprs))
    ;(start-addr-global (+ 300 (get-consts-length) 1))
		;(Global-table (create-global-table (remove-dup (get-fvars pes)) start-addr-global '()))
		)

  ;;; change it !!!!!!!!!


(set-constants pes)
(create-constant-table parsedConstList 306 -1)
(set! Global-table (create-global-table (remove-dup (get-fvars pes)) (+ 300 (get-consts-length) 1) '()))
(write-to-file (string-append (prologue)
  (apply string-append (map (lambda(sub-exp) ;(display sub-exp)
 (printing-func (code-gen sub-exp))) pes))
  (epilogue)
  )
  target-file)

;constTable

)))


(define check-with-all (lambda(exp)

(annotate-tc (pe->lex-pe (box-set
    (remove-applic-lambda-nil (eliminate-nested-defines(parse exp))))))
  ))
