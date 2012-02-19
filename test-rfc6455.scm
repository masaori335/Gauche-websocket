(use gauche.test)
(use gauche.uvector)
(use rfc.822)

(test-start "rfc6455 module")

(require "./rfc6455")
(import rfc6455)

(test-module 'rfc6455)

(define response-header
  '(("Upgrade" "websocket") ("Connection" "Upgrade") ("Sec-WebSocket-Accept" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=") ("Sec-WebSocket-Protocol" "chat")))
(test* "checking handshake-header"  response-header (handshake-header "dGhlIHNhbXBsZSBub25jZQ==" "chat"))

(test* "checking opcode" 'continuation (opcode #u8(#b10000000)))
(test* "checking opcode" 'text         (opcode #u8(#b10000001)))
(test* "checking opcode" 'binary       (opcode #u8(#b10000010)))
(test* "checking opcode" 'close        (opcode #u8(#b10001000)))
(test* "checking opcode" 'ping         (opcode #u8(#b10001001)))
(test* "checking opcode" 'pong         (opcode #u8(#b10001010)))

;; parse frame
(let* ([header  '(#b10000001 #b10001000)]
       [keymask '(#b11111111 #b11111111 #b11111111 #b11111111)]
       [payload (u8vector->list (make-u8vector #b00001000 #b11111111))]
       [expect  (make-u8vector #b00001000 #b00000000)]
       [frame   (list->u8vector (append header keymask payload))])
  (test* "checking parse-frame: 'Payload len' < 126" expect (parse-frame frame)))

(let* ([header  '(#b10000001 #b11111110 #b00000000 #b10000000)]
       [keymask '(#b11111111 #b11111111 #b11111111 #b11111111)]
       [payload (u8vector->list (make-u8vector #b10000000 #b11111111))]
       [expect  (make-u8vector #b10000000 #b00000000)]
       [frame   (list->u8vector (append header keymask payload))])
  (test* "checking parse-frame: 'Payload len' = 126" expect (parse-frame frame)))

(let* ([header  '(#b10000001 #b11111111 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b10000000)]
       [keymask '(#b11111111 #b11111111 #b11111111 #b11111111)]
       [payload (u8vector->list (make-u8vector #b10000000 #b11111111))]
       [expect  (make-u8vector #b10000000 #b00000000)]
       [frame   (list->u8vector (append header keymask payload))])
  (test* "checking parse-frame: 'Payload len' = 127" expect (parse-frame frame)))

;; build frame
(let* ([expect #u8(#b10000001 #b00000100 #b00000000 #b00000000 #b00000000 #b00000000)])
  (test* "checking build-frame: text-frame 'Payload len' < 126" expect (build-frame 'text #f #u8(#b00000000 #b00000000 #b00000000 #b00000000))))

(let* ([data   (make-u8vector #b10000000 #b00000000)]
       [header '(#b10000001 #b01111110 #b00000000 #b10000000)] 
       [expect (list->u8vector (append header (u8vector->list data)))])
  (test* "checking build-frame: text-frame 'Payload len' = 126'" expect (build-frame 'text #f data)))

(let* ([data   (make-u8vector #b10000000000000000 #b00000000)]
       [header '(#b10000001 #b01111111 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000001 #b00000000 #b00000000)] 
       [expect (list->u8vector (append header (u8vector->list data)))])
  (test* "checking build-frame: text-frame 'Payload len' = 127'" expect (build-frame 'text #f data)))

(test-end)
