(define (item->string item)
  (cond ((null? item) "'()")
        ((eq? #f item) "#f")
        ((eq? #t item) "#t")
        ((char? item) (string-append "#\\" (string item)))
        ((string? item) (string-append "\"" item "\""))
        ((number? item) (number->string item))
        ((symbol? item) (string-append
                         "'"
                         (symbol->string item)))
        ((vector? item) (string-append
                         "#("
                         (unbreakupstr (map item->string (vector->list item)) " ")
                         ")"))
        ((list? item) (string-append
                       "("
                       (unbreakupstr (map item->string item) " ")
                       ")"))
        ((pair? item) (string-append
                       "("
                       (item->string (car item))
                       " . "
                       (item->string (cdr item))
                       ")"))
        ((closure? item) "#<CLOSURE>")
        ((procedure? item) "#<PROCEDURE>")
        (else "<?>")))

(define arg-type-list
  '("PDB-INT32"
    "PDB-INT16"
    "PDB-INT8"
    "PDB-FLOAT"
    "PDB-STRING"
    "PDB-INT32ARRAY"
    "PDB-INT16ARRAY"
    "PDB-INT8ARRAY"
    "PDB-FLOATARRAY"
    "PDB-STRINGARRAY"
    "PDB-COLOR"
    "PDB-REGION"
    "PDB-DISPLAY"
    "PDB-IMAGE"
    "PDB-LAYER"
    "PDB-CHANNEL"
    "PDB-DRAWABLE"
    "PDB-SELECTION"
    "PDB-COLORARRAY"
    "PDB-VECTORS"
    "PDB-PARASITE"
    "PDB-STATUS"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (function-args-values:main proc func-name which target-index)
  (define (seq n)
    (letrec
        ((seq-sub
          (lambda (n)
            (if (= n 0)
                '(0)
                (cons n
                      (seq-sub (- n 1)))))))
      (reverse (seq-sub (- n 1)))))

  (define (arg-num func-name)
    (nth target-index (gimp-procedural-db-proc-info func-name)))

  (unbreakupstr
   (mapcar
    (lambda (n)
      (if (= which 0) ;; when type
          (nth (nth which (proc func-name n)) arg-type-list)
          (nth which (proc func-name n))))
    (seq (arg-num func-name)))
   "\n"))

(define (function-args-values type func-name which)
  (cond ((eq? type 'arg)
         (function-args-values:main
          gimp-procedural-db-proc-arg func-name which 6))
        ((eq? type 'value)
         (function-args-values:main
          gimp-procedural-db-proc-val func-name which 7))))

(define (function-args-type func-name)
  (function-args-values 'arg func-name 0))
(define (function-args-name func-name)
  (function-args-values 'arg func-name 1))
(define (function-args-desc func-name)
  (function-args-values 'arg func-name 2))

(define (function-values-type func-name)
  (function-args-values 'value func-name 0))
(define (function-values-name func-name)
  (function-args-values 'value func-name 1))
(define (function-values-desc func-name)
  (function-args-values 'value func-name 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (function-blurb func-name)
  (nth 0 (gimp-procedural-db-proc-info func-name)))
(define (function-help func-name)
  (nth 1 (gimp-procedural-db-proc-info func-name)))

(define (sfs:list-all-functions)
   (cadr (gimp-procedural-db-query "" "" "" "" "" "" "")))
(define (sfs:list-all-functions:lines)
  (unbreakupstr
   (sfs:list-all-functions)
   "\n"))
