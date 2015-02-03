;;; anagma.scm
;;; 2014/8/19
;;; yagi hiroki

;; ローターはとりあえず３つ
;; I	EKMFLGDQVZNTOWYHXUSPAIBRCJ	1930	Enigma I
;; II	AJDKSIRUXBLHWTMCQGZNPYFVOE	1930	Enigma I
;; III	BDFHJLCPRTXVZNYEIWGAKMUSQO	1930	Enigma I
;; ローターの持つべき属性は上に示した変換規則と針の位置
;; 針のが上に来る度に次のローターが一文字分回転する
;; リフレクターがどうなってるのか
;; Contacts    = ABCDEFGHIJKLMNOPQRSTUVWXYZ                
;;               ||||||||||||||||||||||||||
;; Reflector B = YRUHQSLDPXNGOKMIEBFZCWVJAT
;; Reflector C = FVPJIAOYEDRZXWGCTKUQSBNMHL
;;; connectsは変換規則
;;; needleは針の位置(数字1文字 0ならばAとBの間、ZならばZとAのあいだ)
;;; headingは数字1文字 入力された文字数にどれだけずれて対応するか
;;; もうこれでいい

(use coops srfi-13 regex easy-args extras utils)

;;; ローターの変換規則

(define upper-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define *length* (string-length upper-set))
(define 1st-set "EKMFLGDQVZNTOWYHXUSPAIBRCJ")
(define 2nd-set "AJDKSIRUXBLHWTMCQGZNPYFVOE")
(define 3rd-set "BDFHJLCPRTXVZNYEIWGAKMUSQO")
;;; リフレクターの変換規則
(define refB-set "YRUHQSLDPXNGOKMIEBFZCWVJAT")
(define refC-set "FVPJIAOYEDRZXWGCTKUQSBNMHL")

;;; クラス定義
(define-class <converter> ()
  ((connects initform: upper-set)))
(define-class <rotor> (<converter>)
  ((needle initform: 0
           accessor: get-needle)
   (heading initform: 0
            accessor: get-heading)))
(define-class <enigma> ()
  ((plag-board initform: plag-board
               reader: plag-of)
   (fast initform: 1st-rotor
         reader: fast-of)
   (middle initform: 2nd-rotor
           reader: middle-of)
   (slow initform: 3rd-rotor
         reader: slow-of)
   (reflector initform: reflector
              reader: reflector-of)))

(define-generic (input a <converter>))
(define-generic (input a <rotor>))
(define-generic (input a <enigma>))
(define-generic (output a <rotor>))

(define-method (get-heading (r <rotor>)) (slot-value r 'heading))
(define-method ((setter get-heading) (r <rotor>) val) (set! (slot-value r 'heading) val))
(define-method (get-needle (r <rotor>)) (slot-value r 'needle))
(define-method ((setter get-needle) (r <rotor>) val) (set! (slot-value r 'needle) val))


(define-method (input (a #t) (c <converter>))
  (let ((set (slot-value c 'connects))
        (num (string-index upper-set a)))
    (string-ref set num)))

(define-method (input (a #t) (r <rotor>))
  (let ((set  (slot-value r 'connects))
        (head (slot-value r 'heading))
        (num  (string-index upper-set a)))
    (string-ref set (modulo (- num head) *length*))))


(define-method (output (a #t) (r <rotor>))
  (let ((num (string-index (slot-value r 'connects) a)))
    (string-ref upper-set (modulo (+ num (get-heading r)) *length*))))


(define-method (plag-of (e <enigma>)) (slot-value e 'plag-board))
(define-method (fast-of (e <enigma>)) (slot-value e 'fast))
(define-method (middle-of (e <enigma>)) (slot-value e 'middle))
(define-method (slow-of (e <enigma>)) (slot-value e 'slow))
(define-method (reflector-of (e <enigma>)) (slot-value e 'reflector))

(define-method (input (a #t) (e <enigma>))
  (let  ((a1 (input a (plag-of e)))
         (f (fast-of e))
         (m (middle-of e))
         (s (slow-of e))
         (flag1 #t)
         (flag2 #t))
    (set! (get-heading f) (modulo (add1 (get-heading f)) *length*))
    (let ((a2 (input a1 f)))
      (cond ((= (get-heading f) (get-needle f))
             (when flag1
               (set! (get-heading m) (modulo (add1 (get-heading m)) *length*))
               (set! flag1 #f)))
            (else (set! flag1 #t)))
      (let ((a3 (input a2 m)))
        (cond ((= (get-heading m) (get-needle m))
             (when flag2
               (set! (get-heading s) (modulo (add1 (get-heading s)) *length*))
               (set! flag2 #f)))
            (else (set! flag2 #t)))
        (output (output (output (input (input a3 s) (reflector-of e))
                                s)
                        m)
                f)))))

;;; dirty

(define-syntax initialize-enigma
  (syntax-rules ()
    ((_ lst)
     (begin
       (define heading-list lst)
       (define plag-board (make <converter> 'connects upper-set))
       (define 1st-rotor  (make <rotor>
                            'connects 1st-set
                            'heading (first heading-list)))
       (define 2nd-rotor  (make <rotor>
                            'connects 2nd-set
                            'heading (second heading-list)))
       (define 3rd-rotor  (make <rotor>
                            'connects 3rd-set
                            'heading (third heading-list)))
       (define reflector  (make <converter> 'connects refB-set))
       (define enigma (make <enigma>))))
    ((_)
     (initialize-enigma (list (random *length*) (random *length*) (random *length*))))))

(define-syntax reset-enigma
  (syntax-rules ()
    ((_)
     (begin
       (set! (get-heading (fast-of enigma)) (first heading-list))
       (set! (get-heading (middle-of enigma)) (second heading-list))
       (set! (get-heading (slow-of enigma)) (third heading-list))))))

(define (input-string str)
  (display
   (string-map (lambda (c) (input c enigma))
               (string-upcase (string-substitute* str '(("[^a-zA-Z]" . ""))))))
  (newline)
  (reset-enigma))

(define (enigma-loop)
  (display "anagma> ")
  (input-string (read-line))
  (enigma-loop))

(define (usage)
  (printf "anagma, version 1.1\n")
  (printf "usage: anagma [OPTION ...] [FILENAME | STRING]\n")
  (printf "-e, --exec string  : string passed in\n")
  (printf "-f, --file file    : filename passed in\n")
  (printf "-h, --help         : print this help message\n")
  (printf "-k, --keys keys    : keys is init string of 3 rotors\n")
  (printf "-r, --repl         : start anagma repl\n")
  (printf "-v, --version      : print the anagma version\n"))

;;; main exec----------------------------------------------
(define-arguments
  ((exec e) "")
  ((file f) "")
  ((help h))
  ((keys k) "")  
  ((repl r))  
  ((version v)))

(let ((lst (map string->number (string-split (keys)))))
  (if (null? lst) (initialize-enigma)
      (initialize-enigma lst)))
(cond ((help) (usage))
      ((repl) (enigma-loop))
      ((not (zero? (string-length (file))))
       (input-string (read-all (file))))
      ((not (zero? (string-length (exec))))
       (input-string (exec)))
      ((version)
       (printf "anagma, version 1.0\n"))
      (else (usage)))

