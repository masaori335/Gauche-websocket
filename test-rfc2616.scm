(use gauche.test)
(test-start "rfc2616 module")

(require "./rfc2616")
(import rfc2616)

(test-module 'rfc2616)
(test* "check status-line" "HTTP/1.1 200 OK" (http-status-line 200))

(test-end)
