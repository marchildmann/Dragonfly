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
;; EXPRESS OR IMPL
;; IED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; @module Dragonfly_TWITTER
;; @author Marc Hildmann <marc.hildmann at gmail.com>
;; @version 0.15
;; @location http://code.google.com/p/dragonfly-newlisp/
;; @description A newLISP web framework for rapid web development
;; <h4>About Dragonfly web framework</h4>
;; <p>Dragonfly is a small web framework which is currently under heavy development.
;; Its's features are a short learning curve, lightweight and fun in programming - 
;; 
;; 
;; just like newLISP itself.</p>

;===============================================================================
; !Loading modules and defining new context
;===============================================================================

(context 'Dragonfly)

;===============================================================================
; !twitter functions
;===============================================================================

;; @syntax (Dragonfly:twitter-search <keyword>)
;; @param <keyword> string containing the keyword for search
;; <p>Writes the results of the search in nice speech bubbles.</p>
;;

(define (twitter-search keyword rpp)
  (set 'xml (get-url (string "http://search.twitter.com/search.atom?rpp="rpp"&q="keyword) ))
  (xml-type-tags nil nil nil nil) ; no extra tags
  (set 'sxml (xml-parse xml 31)) ; turn on SXML options
  (set 'entry-index (ref-all '(entry *) sxml match))
  (when (empty? entry-index)
    (println "No entries found")
  )
  (println "<div id='twitter_search_results'><h3>You searched for '"keyword"' and we found</h3>")
  (dolist (idx entry-index)
	(println "<div class='bubble'><blockquote><p>")    
	(set 'entry (sxml idx))
    (println 
				(lookup 'title entry) 
				"</p></blockquote>"
				"<cite><strong>"
				(lookup '(author name) entry ) "</strong> on " 
			 	(lookup 'published entry) "</cite></div>")
	)
	(println "</div>")
)

(context Dragonfly) "debug"