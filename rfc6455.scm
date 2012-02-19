;;;
;;; rfc6455.scm -  websocket library
;;;
;;;   Version: 0.0.1pre
;;;   Auther:  Masaori Koshiba
;;; 
;;; RFC 6455 The WebSocket Protocol
;;; http://www.ietf.org/rfc/rfc6455.txt
;;;

(define-module rfc6455
  (use srfi-27)
  (use rfc.sha)
  (use rfc.base64)
  (use rfc.822)
  (use util.list)
  (use gauche.net)
  (use gauche.uvector)
  (export 
          handshake-header
          opcode
          parse-frame
          build-frame
  )
)
(select-module rfc6455)

(random-source-randomize! default-random-source)
(define *max-frame-size* 16777216)

(define (handshake-header key protocol)
  (define (accept key)
    (base64-encode-string (digest-string <sha1> (format "~A258EAFA5-E914-47DA-95CA-C5AB0DC85B11" key))))
  (list (list "Upgrade" "websocket")
        (list "Connection" "Upgrade")
        (list "Sec-WebSocket-Accept"   (accept key))
        (list "Sec-WebSocket-Protocol" protocol)))

(define-constant *opcode-table*
  '((#x0 . continuation)
    (#x1 . text)
    (#x2 . binary)
    (#x8 . close)
    (#x9 . ping)
    (#xA . pong)))

(define (opcode->name code)
  (assq-ref *opcode-table* code))

(define (name->opcode name)
  (rassq-ref *opcode-table* name))

(define (opcode frame)
  (opcode->name (logand (u8vector-ref frame 0) #b00001111)))

(define (parse-frame frame)
  (define (fin? frame)
    (logbit? 7 (u8vector-ref frame 0)))

  (define (masked? frame)
    (logbit? 7 (u8vector-ref frame 1)))

  (define (extended-payload-length frame)
    (cond [(= 126 (logand (u8vector-ref frame 1) #b01111111)) 2]
          [(= 127 (logand (u8vector-ref frame 1) #b01111111)) 8]
          [else 0]))
  
  (define (u8vector->num frame)
    (define (trancepose data res n)
      (if (eq? '() data)
          res
          (trancepose (cdr data) (+ res (* (car data) (expt 256 n))) (+ n 1))))
    (trancepose (reverse (u8vector->list frame)) 0 0))

  (define (payload-length-ref frame)
    (let ([extended-len (extended-payload-length frame)])
      (cond [(= 0 extended-len) (logand (u8vector-ref frame 1) #b01111111)]
            [(= 2 extended-len) (u8vector->num (u8vector-copy frame 2 4))]
            [(= 8 extended-len) (u8vector->num (u8vector-copy frame 2 10))]
            [else (error "invarid extended payload length")])))
  
  (if (fin? frame)
      (let* ([opcode  (logand (u8vector-ref frame 0) #b00001111)]
             [payload-length  (payload-length-ref frame)]
             [extended-length (extended-payload-length frame)])
        (if (masked? frame)
            (let* ([mask-position (+ 2 extended-length)]
                   [payload-position (+ mask-position 4)]
                   [mask    (u8vector-copy frame mask-position (+ mask-position 4))]
                   [payload (u8vector-copy frame payload-position (+ payload-position payload-length))])
              (xor mask payload))             
              (u8vector-copy frame 2 payload-length)))
      (error "multi-frame")))

;; masking and unmasking are same proc just xor.
(define (xor mask value)
  (let ([value-len (u8vector-length value)]
        [mask-len  (u8vector-length mask)])

    (define (fit-mask buf index)
      (u8vector-copy! buf index mask) 
      (if (<= value-len (+ index mask-len))
          buf
          (fit-mask buf (+ index mask-len))))
     
    (u8vector-xor (fit-mask (make-u8vector value-len) 0) value)))

;; build a data flame, but not support multi frame.
;; a server MUST NOT mask any frames that it sends to the client.
;; a client MUST close a connection if it detects a masked frame. 
(define (build-frame opcode :optional ismask? payload)
  (cond 
    [(eq? opcode 'text)  (text-frame ismask? payload)]
    [(eq? opcode 'close) (close-frame)]
    [(eq? opcode 'ping)  (ping-frame)]
    [(eq? opcode 'pong)  (pong-frame)]
    [else (error "no such opcode")]))

(define (text-frame ismask? payload)
  (define (payload-length->u8vector len size)
    (define (trancepose payload-vector index)
      (u8vector-set! payload-vector (- size index 1) (ash (logand len (ash #b11111111 (* index 8))) (* index -8)))
      (if (= index (- size 1))
          payload-vector
          (trancepose payload-vector (+ index 1))))
    (trancepose (make-u8vector size) 0))

  (define (mask data)
    (xor (list->u8vector (map (lambda (x) (random-integer 128)) '(0 1 2 3))) data))

  (let* ([payload-length (u8vector-length payload)]
         [header-length 0]
         [frame (make-u8vector *max-frame-size*)])

    (u8vector-set! frame 0 #b10000001)

    (cond [(<= payload-length 125) (u8vector-set! frame 1 payload-length)
                                   (set! header-length 2)]
          [(<= payload-length 65535) (u8vector-set! frame 1 126)
                                     (u8vector-copy! frame 2 (payload-length->u8vector payload-length 2))
                                     (set! header-length 4)]
          [else (u8vector-set! frame 1 127)
                (u8vector-copy! frame 2 (payload-length->u8vector payload-length 8))
                (set! header-length 10)])

    (if ismask? (begin (u8vector-set! frame 1 (logand (u8vector-ref frame 1) #b10000000))
                       (set! payload (mask payload))))

    (u8vector-copy! frame header-length payload)
    (u8vector-copy  frame 0 (+ header-length payload-length))))

(define (close-frame)
  (let ([frame (make-u8vector 2)])
    (u8vector-set! frame 0 #b10001000)
    (u8vector-set! frame 1 #b00000000)
    frame))

(define (ping-frame)
  (let ([frame (make-u8vector 2)])
    (u8vector-set! frame 0 #b10001001)
    (u8vector-set! frame 1 #b00000000)
    frame))

(define (pong-frame)
  (let ([frame (make-u8vector 2)])
    (u8vector-set! frame 0 #b10001010)
    (u8vector-set! frame 1 #b00000000)
    frame))
