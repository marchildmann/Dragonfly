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

;; @module Dragonfly
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
; !Table Functions
;===============================================================================

;; @syntax (Dragonfly:table_open <head_elements>)
;; @param <head_elements> a list containing the table head
;; <p>Writes a standard table open element including a head, generated from a list.</p>
;; 
(define (table_open table_head_elements)
  (set 'table_colspan table_head_elements) ;count of table_head_elements for defining a correct table-colspan
  (print "
	<!-- BEGIN table -->
	<table>
	<thead><tr>"
		(dolist (th-element table_head_elements)
			(if (= $idx 0) (print "<th class='table_firstcolumn'>"th-element"</th>") (print "<th>"th-element"</th>"))
		)
	"</tr></thead>"
  )
)

;; @syntax (Dragonfly:table_data <table_elements>)
;; @param <table_elements> a list containing all table elements (row by row, from left to right), including empty cells
;; <p>Writes the table data generated from a list.</p>
;; 

(define (table_data table_elements)
	; initialize maximum length per row with offset -1, because counting starts at "0"	
	(set 'rowlength (length table_colspan))	
	(set 'rowlength (- rowlength 1))
	(print "<tbody>"
			(dotimes (i 1)
				(dolist (td-element table_elements)
					; here we do some modulo calculation to determine if a new row begins
					(if (= (mod $idx (length table_colspan)) 0) (print "<tr><td class='table_firstcolumn'>"td-element"</td>") (print "<td>"td-element"</td>"))
					(if (= (mod $idx (length table_colspan)) rowlength) (println "</tr>"))
				)
			)	
			"</tbody>"
  	)
)

;; @syntax (Dragonfly:table_footer <table_footer_text>)
;; @param <table_footer_text> a string containing the table footer text
;; <p>Writes a standard table footer with number of colspan generated from the last table-head.</p>
;; 
(define (table_footer table_footer_text)
  (print "
	<tfoot><tr><td class='table_foot' colspan='"(length table_colspan)"'>"table_footer_text"</td></tr></tfoot>"
  )
)

;; @syntax (Dragonfly:table_close)
;; <p>Writes a standard table close element.</p>
;; 
(define (table_close)
  (print "</table><!-- END table -->")
)

(context Dragonfly)