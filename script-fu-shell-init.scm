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
