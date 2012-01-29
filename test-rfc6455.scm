(use gauche.test)
(use rfc.822)

(test-start "rfc6455 module")

(require "./rfc6455")
(import rfc6455)

(test-module 'rfc6455)

(define response-header
  '(("Upgrade" "websocket") ("Connection" "Upgrade") ("Sec-WebSocket-Accept" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=") ("Sec-WebSocket-Protocol" "chat")))
(test* "check handshake-header"  response-header (handshake-header "dGhlIHNhbXBsZSBub25jZQ==" "chat"))

(test-end)
