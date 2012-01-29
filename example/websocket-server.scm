;;;
;;; websocket-server.scm -  an example of websocket library
;;;
;;;   Version: 0.0.1pre
;;;   Auther:  Masaori Koshiba
;;; 
(use rfc.822)
(use gauche.uvector)
(use gauche.net)

(require "../rfc2616")
(import rfc2616)
(require "../rfc6455")
(import rfc6455)

(define (websocket-server port)
  (let ([server (make-server-socket 'inet 8080 :reuse-addr? #t)])
    
    (define (session client)
      (let ([iport (socket-input-port client)]
            [oport (socket-output-port client)])
        
        (define (handshake request)
          (socket-send client (http-status-line 101))
          (rfc822-write-headers (handshake-header (rfc822-header-ref request "sec-websocket-key") "chat")
                                :output oport)
          (flush oport))
        
        (define (listen)
          (let* ([buf (make-u8vector 1024)]
                 [buf-length (socket-recv! client buf)])
            (cond 
              [(eq? (opcode buf) 'text) 
               (socket-send client (build-frame 'text #f (parse-frame buf)))
               (listen)]
              [(eq? (opcode buf) 'ping) (socket-send (pong) client)]
              [(eq? (opcode buf) 'close)
                    (print "WebSocket Closed")
                    (socket-close client)]
              [else (error "no such opcode")
                    (socket-close client)])))
        (rxmatch-case (read-line iport)
          [test eof-object? (socket-close client)]
          [#/^(GET)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
            (let ([request (rfc822-read-headers iport)])
              (cond [(string=? "websocket" (rfc822-header-ref request "upgrade"))
                     (handshake request)
                     (print "WebSocket Opened")
                     (print client)
                     (listen)]
                    [else (socket-close client)]))]
          [else (socket-close client)])))

    (let loop ([client (socket-accept server)])
      (session client)
      (loop (socket-accept server)))))

(websocket-server 8080)
