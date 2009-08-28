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

;; @module Dragonfly_SQLite
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
; !sqlite3 Wrapper
;===============================================================================


;; @syntax (Dragonfly:sqlite-open <databasename>)
;; @param <databasename> string containing the database name
;; <p>Open the SQLite database or creates it, if it does not exist.</p>
;;
(define (sqlite-open databasename)
	;; close old connections
	(sql3:close)
	
	(if (sql3:open databasename) 
		(set 'flashnotice "Database was successfully opened or created.") 
	) 	
)

;; @syntax (Dragonfly:sqlite-tables)
;; <p>Shows the existing tables in the current database.</p>
;;
(define (sqlite-tables) 
	(println "
	<div id='dragonfly_database-information' style='border:1px dotted #00aeef; width:800px; padding:8px; margin-top:20px;margin-bottom:20px' >
	<h2>Existing tables in SQLite Database</h2><pre>
	"
	(sql3:tables)"
	</pre></div>")
)

;; @syntax (Dragonfly:sqlite-columns)
;; <p>Shows the existing columns in a given table.</p>
;;
(define (sqlite-columns table) 
	(println "
	<div id='dragonfly_database-information' style='border:1px dotted #00aeef; width:800px; padding:8px; margin-top:20px;margin-bottom:20px' >
	<h2>Existing columns in SQLite table <i>"table"</i></h2><pre>
	"
	(sql3:columns table)"
	</pre></div>")
)

(define (sqlite-empty-table table) 
	(set 'query (append "DELETE FROM "table))
	(sql3:sql query)
)

(define (sqlite-query query) 
	(set 'sqlarray (sql3:sql query)) ; results of query
	;; close old connections
	(sql3:close)	
)

(define (sqlite-insert table values)
	(set 'query (append "INSERT INTO "table" VALUES ("values")"))
	(sql3:sql query)
)

(define (sqlite-get-tabledata query) 
	(set 'sqlarray (sql3:sql query)) ; results of query
	
)

(context Dragonfly)
