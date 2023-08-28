(module (web)
    (*routes*
     *fallback-handler*
     *handle-endpoint*
     define-endpoint
     send-html
     send-json
     serve)
  (import (garlic base)
          (garlic sys)
          (web globby)
          uri-common
          intarweb
          spiffy
          medea
          sxml-serializer)
  (reexport (garlic base) intarweb spiffy uri-common)

  (define *routes* '())

  (define *fallback-handler* (lambda () (send-status 'not-found "Default fallback handler")))


  ;; public for the macro
  (define (*handle-endpoint* endpoint . middleware)
    (let ([ctx (make-hash-table)])
      (define (process mws continue?)
        (cond [(not continue?) #f]
              [(null? mws) #t]
              [else (process (cdr mws) ((car mws) ctx))]))
      (if (process middleware #t)
          (endpoint ctx))))


  (define (router-handler continue)

    (define (uri-path-string uri)
      (apply string-append
             (map (lambda (it) (string-append "/" it))
                  (cdr (uri-path uri)))))

    (define (match-glob method path)
      (find (lambda (pair) (globby-match (cdar pair) path))
            (filter (lambda (pair) (equal? method (caar pair))) *routes*)))

    (let ([path (uri-path-string (request-uri (current-request)))]
          [method (request-method (current-request))])
      (let ([route (match-glob method path)])
        (if route ((cdr route)) (*fallback-handler*))))

    (continue)                          ; done with request
    )

  (define (send-html sxml) (send-response status: 'ok body: (serialize-sxml sxml)))
  (define (send-json sxml)
    (with-headers `((content-type #(application/json ())))
                  (lambda () (send-response status: 'ok body: (json->string sxml)))))


  (define-syntax define-endpoint
    (syntax-rules ()
      ((_ (method path middleware middleware* ...) endpoint)
       (set! *routes*
         (append *routes*
                 `(((,method . ,path) .
                    ,(lambda () (*handle-endpoint* endpoint middleware middleware* ...)))))))
      ((_ (method path) body ...)
       (set! *routes*
         (append *routes*
                 `(((,method . ,path) . ,(lambda () body ...))))))))

  (define (serve host port)
    (vhost-map `((,host . ,router-handler)))
    (server-port port)
    (start-server))
  )
