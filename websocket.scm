;; WebSocket Server
(use srfi-27)
(use rfc.sha)
(use rfc.base64)
(use rfc.822)
(use gauche.net)
(use gauche.selector)
(use gauche.uvector)

(random-source-randomize! default-random-source)

(define (websocket-server port)
  (let ((server   (make-server-socket 'inet port :reuse-addr? #t)))

    (define (make-session client)
      (let ((iport (socket-input-port client))
            (oport (socket-output-port client)))
        
        (define (handshake)
          (let* ((request (rfc822-read-headers iport))
                (key (rfc822-header-ref request "sec-websocket-key"))
                (response-format
                 "HTTP/1.1 101 Switching Protocols\r\n\
                  Upgrade: websocket\r\n\
                  Connection: Upgrade\r\n\
                  Sec-WebSocket-Accept: ~A\r\n\
                  Sec-WebSocket-Protocol: chat\r\n\
                  \r\n"))
            (display (format response-format (accept key)) oport)
            (flush oport)))
        
        (define (listen)
          (let* ((buf (make-u8vector 1024))
                 (buf-length (socket-recv! client buf))
                 (opcode (get-opcode buf))
                 (data (parse-dataframe buf)))
            (print (format "opcode: ~A" opcode))
            (cond ((= opcode 1) (socket-send client (build-dataframe data))
                                (listen))
                  ((= opcode 11) (listen))
                  ((= opcode 9) (socket-send (pong) client))
                  ((equal? (u8vector-ref buf 0) 136) 
                     (print "WebSocket Closed")
                     (socket-close client))
                  (else (print "error")
                        (socket-close client)))))
        
        (let ((request (read-line iport)))
          (if (or (eof-object? request)
                  (not (= 0 (string-scan request "GET"))))
              (socket-close client)
              (begin (handshake)
                     (print "WebSocket Opened")
                     (print client)
                     (listen))))))

    (let loop ((client (socket-accept server)))
      (make-session client)
      (loop (socket-accept server)))))

(define (accept key)
  (base64-encode-string (digest-string <sha1> (format "~A258EAFA5-E914-47DA-95CA-C5AB0DC85B11" key))))

(define (get-opcode buf)
  (logand (u8vector-ref buf 0) #b00001111))

(define (fin? buf)
  (logbit? 7 (u8vector-ref buf 0)))

(define (masked? buf)
  (logbit? 7 (u8vector-ref buf 1)))

(define (extended-payload-length buf)
  (cond ((= 126 (logand (u8vector-ref buf 1) #b01111111)) 4)
        ((= 127 (logand (u8vector-ref buf 1) #b01111111)) 8)
        (else 0)))

(define (parse-dataframe buf)
  (if (fin? buf)
      (let ((opcode         (logand (u8vector-ref buf 0) #b00001111))
            (payload-length (logand (u8vector-ref buf 1) #b01111111)))
        (if (masked? buf)
            (let ((mask    (u8vector-copy buf 2 6))
                  (payload (u8vector-copy buf 6 (+ 6 payload-length))))
              (unmask mask payload))
            (u8vector-copy buf 2 payload-length)))
        'multi-frame))

(define (unmask mask value)
  (let ((value-len (u8vector-length value))
        (mask-len  (u8vector-length mask)))

    (define (fit-mask buf index)
      (u8vector-copy! buf index mask) 
      (if (<= value-len (+ index mask-len))
          buf
          (fit-mask buf (+ index mask-len))))
     
    (u8vector-xor (fit-mask (make-u8vector value-len) 0)
                  value)))

(define (build-dataframe data)
  (let* ((len   (u8vector-length data))
         (frame (make-u8vector (+ len 2))))
    (u8vector-set! frame 0 #b10000001)
    (u8vector-set! frame 1 len)
    (u8vector-copy! frame 2 data)
    frame))

(define (close-frame)
  (let ((frame (make-u8vector 2)))
    (u8vector-set! frame 0 #b10001000)
    (u8vector-set! frame 1 #b00000000)
    frame))

(define (ping)
  (let ((frame (make-u8vector 2)))
    (u8vector-set! frame 0 #b10001001)
    (u8vector-set! frame 1 #b00000000)
    frame))

(define (pong)
  (let ((frame (make-u8vector 2)))
    (u8vector-set! frame 0 #b10001010)
    (u8vector-set! frame 1 #b00000000)
    frame))

(websocket-server 8080)
