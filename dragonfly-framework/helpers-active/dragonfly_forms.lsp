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
; !Form Functions
;===============================================================================

;; @syntax (Dragonfly:form_open <form_action> <form_method)
;; @param <name> a string containing the form action
;; @param <name> a string containing the form submit method POST/GET
;; <p>Writes a standard form open element</p>
;; 
(define (form-open form_action form_method)
  (print "<table><form action='"form_action"' method='"form_method"'>")
)

;; @syntax (Dragonfly:form_textfield <form_name>)
;; @param <name> a string containing the text field name
;; <p>Writes a standard form field with size '30'</p>
;; 
(define (form-textfield form_name)
  (print "<tr class='form_row'><td class='form_label'><label for='"form_name"'>"(title-case form_name)"</label></td><td class='form_input'><input id='"form_name"' name='"form_name"' size='30' type='text' /></td></tr>")
)

;; @syntax (Dragonfly:form_submit <form_value>)
;; @param <value> a string containing the value for the submit button
;; <p>Writes a standard form submit button.</p>
;; 
(define (form-submit form_value)
  (print "<tr class='form_row'><td class='form_label'>&nbsp;</td><td class='form_input'><input id='commit' type='submit' value='"form_value"' /></td></tr>")
)

;; @syntax (Dragonfly:form-submit-js <form_action> <form_value>)
;; @param <form_action> a string containing the URI after submitting the submit button
;; @param <form_value> a string containing the value for the submit button
;; <p>Writes a form submit button with some javascript
;; which modifies the action attribute.</p>
;; 
(define (form-submit-js form_action form_value)
  (print "<tr class='form_row'><td class='form_label'>&nbsp;</td><td class='form_input'><input id='commit' type='button' value='"form_value"' onClick='document.forms[0].action=\""form_action"\"+document.forms[0].Text.value;document.forms[0].submit();' /></td></tr>")
)


;; @syntax (Dragonfly:form_hidden <form_name> <form_value>)
;; @param <form_name> a string containing the name for the hidden field
;; @param <form_value> a string containing the value for the hidden field
;; <p>Writes a standard form hidden field.</p>
;; 
(define (form-hidden form_name form_value)
  (print "<input type='hidden' name='"form_name"' value='"form_value"' />")
)

;; @syntax (Dragonfly:form_close)
;; <p>Writes a form close tag</p>
;; 
(define (form-close)
  (print "</form></table>")
)

;; @syntax (Dragonfly:form_generate-from-columns <tablename>)
;; <p>Generates a form including input field from the given table.
;; The form uses POST Method and refers to itself. It includes one hidden
;; field called dragonfly_form, to detect form submit.
;; </p>
;; 
(define (form-generate-from-columns database tablename)
  (Dragonfly:load-database database) ; load the specified nldb
  (set 'columns (nldb:list-columns tablename))
  (print "<table><form id='form_"table"' action='?"viewname"/"{save}"' method='POST'>")
	(dolist (column_name columns)
		(print "<tr class='form_row'><td class='form_label'><label for='"column_name"'>"(title-case column_name)"</label></td><td class='form_input'><input id='"column_name"' name='"column_name"' size='30' type='text' /></td></tr>")
	)
  (print "<tr class='form_row'><td class='form_label'>&nbsp;</td><td class='form_input'><input id='form_submit' type='submit' name='commit' value='Add' /></td></tr>")
  (print "<input type='hidden' name='databasetable' value='"table"' />")
  (print "</form></table>")
)

(context Dragonfly)