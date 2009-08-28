;; Copyright (c) 2009 Marc Hildmann
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; @module Dragonfly_SEO
;; @author Marc Hildmann <marc.hildmann at gmail.com>
;; @version 0.15
;; @location http://code.google.com/p/dragonfly-newlisp/
;; @description A newLISP web framework for rapid web development
;; <h4>About Dragonfly web framework</h4>
;; <p>Dragonfly is a small web framework which is currently under heavy development.
;; Its's features are a short learning curve, lightweight and fun in programming - 
;; just like newLISP itself.</p>

;===============================================================================
; !Defining new context
;===============================================================================

(context 'Dragonfly)


;===============================================================================
; !SEO functions (just ideas - really alpha!!!)
;===============================================================================

(define (clean-html url)
	(set 'page (get-url url))
	(replace "<[^>]*>" page "" 0)
	(println page)
)
   
(define (check-meta-description)
	
	(set 'description "<meta name=\"description\"`
	      content=\"newLISP is a general purpose scripting language for developing web applications and programs in general and in the domains of artificial intelligence (AI) and statistics.\">")
	;;(println (regex "<meta name=\"description\" content=\"(.*)\">/i" "<meta name=\"description\"`
	;;	      content=\"newLISP is a general purpose scripting language for developing web applications and programs in general and in the domains of artificial intelligence (AI) and statistics.\">"))
	;;(println $0)
	(set 'description-length (length description))
	(or
		(if (< description-length 60) (println "Your meta description with "description-length" characters is too short."))
		(if (> description-length 170) (println "Your meta description with "description-length" characters is too long."))
	)
	(and
		(if (>= description-length 60))
		(if (<= description-length 170))
		(println "Your meta description with "description-length" characters is ideal.")
	)
)

(define (check-meta-keywords keywords)
	
	(set 'number-keywords (length keywords))
	(or
		(if (< number-keywords 5) (println "The number of meta-keywords "number-keywords" are too small."))
		(if (> number-keywords 10) (println "The number of meta-keywords "number-keywords" are too much."))
	)
	(and
		(if (>= number-keywords 5))
		(if (<= number-keywords 10))
		(println "Your number of meta-keywords "number-keywords" are ideal.")
	)
)

;===============================================================================
; !Google functions
;===============================================================================

(define (google-results-domain domain)
	(set 'json (get-url (string "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q="domain)))
	(begin
		(regex "\"estimatedResultCount\":\"(.*)\"," json)
		(set 'erc $1)	
	)
)

(context Dragonfly)