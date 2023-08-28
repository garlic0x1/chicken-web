(module (web globby) (globby->regexpstr globby->regexp globby-match)
  (import (garlic base) regex)

  ;; made this since the one in `regex` egg doesnt support globstar
  (define (globby->regexpstr glob)
    "Only supports | ? * **"

    ;; mutable variable to dedupe globstars, sorry
    (define ignoring-stars #f)

    (define (peek i)
      (let ([next (get (add1 i) glob)])
        (if next (string next) "")))

    (define (handle-char pair)
      (let ([c (cdr pair)]
            [i (car pair)])
        (if (equal? c "*")
            (cond [ignoring-stars #f]
                  [(equal? (peek i) "*") (begin (set! ignoring-stars #t) ".*")]
                  [else "([^/]*)"])
            (begin
              (set! ignoring-stars #f)
              (cond [(equal? c "|") (string-append "//" c)]
                    [(equal? c "?") "."]
                    [else c])))))

    (->> (string->list glob)
         (map string)
         (enumerate)
         (map handle-char)
         (filter identity)
         (apply string-append)))

  (define (globby->regexp glob)
    (regexp (globby->regexpstr glob)))

  (define (globby-match glob str) (string-match (globby->regexp glob) str))
  )
