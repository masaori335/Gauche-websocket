;;;
;;; rfc2616.scm -  http library
;;;
;;; This library define only statuslines for http server.
;;; If you want client side library, "rfc.http" may help you.
;;;
;;;   Version: 0.0.1pre
;;;   Auther:  Masaori Koshiba
;;; 
;;; RFC 2616 Hypertext Transfer Protocol -- HTTP/1.1
;;; http://www.ietf.org/rfc/rfc2616.txt
;;;

(define-module rfc2616
  (export http-status-line)
)
(select-module rfc2616)

(define-constant *STATUS-TABLE*
  '((100 . "Continue")
	  (101 . "Switching Protocols")
	  (200 . "OK")
	  (201 . "Created")
	  (202 . "Accepted")
	  (203 . "Non-Authoritative Information")
	  (204 . "No Content")
	  (205 . "Reset Content")
	  (206 . "Partial Content")
	  (300 . "Multiple Choices")
	  (301 . "Moved Permanently")
	  (302 . "Found")
	  (303 . "See Other")
	  (304 . "Not Modified")
	  (305 . "Use Proxy")
	  (307 . "Temporary Redirect")
	  (400 . "Bad Request")
	  (401 . "Unauthorized")
	  (402 . "Payment Required")
	  (403 . "Forbidden")
	  (404 . "Not Found")
	  (405 . "Method Not Allowed")
	  (406 . "Not Acceptable")
	  (407 . "Proxy Authentication Required")
	  (408 . "Request Time-out")
	  (409 . "Conflict")
	  (410 . "Gone")
	  (411 . "Length Required")
	  (412 . "Precondition Failed")
	  (413 . "Request Entity Too Large")
	  (414 . "Request-URI Too Large")
	  (415 . "Unsupported Media Type")
	  (416 . "Requested range not satisfiable")
	  (417 . "Expectation Failed")
	  (500 . "Internal Server Error")
	  (501 . "Not Implemented")
	  (502 . "Bad Gateway")
	  (503 . "Service Unavailable")
	  (504 . "Gateway Time-out")
	  (505 . "HTTP Version not supported")
   ))

(define (http-status-line status-code)
  (let ([status (assq status-code *STATUS-TABLE*)])
    (format "HTTP/1.1 ~A ~A\r\n" (car status) (cdr status))))

