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
  '("INT32"
    "INT16"
    "INT8"
    "FLOAT"
    "STRING"
    "INT32ARRAY"
    "INT16ARRAY"
    "INT8ARRAY"
    "FLOATARRAY"
    "STRINGARRAY"
    "COLOR"
    "REGION"
    "DISPLAY"
    "IMAGE"
    "LAYER"
    "CHANNEL"
    "DRAWABLE"
    "SELECTION"
    "COLORARRAY"
    "VECTORS"
    "PARASITE"
    "STATUS"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (seq n)
    (letrec
        ((seq-sub
          (lambda (n)
            (if (= n 0)
                '(0)
                (cons n
                      (seq-sub (- n 1)))))))
      (reverse (seq-sub (- n 1)))))


(define (function-args-values:main proc func-name which target-index)
  (define (arg-num func-name)
    (nth target-index (gimp-procedural-db-proc-info func-name)))

  (if (> (arg-num func-name) 0)
      (mapcar
       (lambda (n)
         (if (= which 0) ;; when type
             (nth (nth which (proc func-name n)) arg-type-list)
             (nth which (proc func-name n))))
       (seq (arg-num func-name)))
      '()))


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

(define (zip-args-values names types descs)
  (if (> (length names) 0)
      (unbreakupstr
       (map
        (lambda (n)
          (string-append (nth n names)
                         " ("
                         (nth n types)
                         ")\n"
                         (nth n descs)))
        (seq (length names)))
       "\n--\n")
      "-"))

(define (procedure-args func-name)
  (zip-args-values
   (function-args-name func-name)
   (function-args-type func-name)
   (function-args-desc func-name)))

(define (procedure-values func-name)
  (zip-args-values 
   (function-values-name func-name)
   (function-values-type func-name)
   (function-values-desc func-name)))

(define (procedure-info func-name)
  (define (strip-quote str)
    (substring str 1 (- (string-length str) 1)))

  (unbreakupstr
   (map
    (lambda (x)
      (string-append "-------- " (car x) " --------\n"
                     (strip-quote (item->string (cdr x)))))
    `(("Name" . ,func-name)
      ("Blurb" . ,(function-blurb func-name))
      ("Arguments" . ,(procedure-args func-name))
      ("Return values" . ,(procedure-values func-name))
      ("Help" . ,(function-help func-name))))
   "\n\n"))


(define (sfs:list-all-functions)
   (cadr (gimp-procedural-db-query "" "" "" "" "" "" "")))
(define (sfs:list-all-functions:lines)
  (unbreakupstr
   (script-fu-shell:list-all-functions)
   "\n"))
