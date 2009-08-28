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

;; @module Dragonfly_NLDB
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
; !nldb Wrapper for a pure newLISP Database (flat)
;===============================================================================

;; @syntax (Dragonfly:load-database <database>)
;; @param <database> database filename
;; <p>Loads the specified nldb database in the default
;; directory /databases.</p>
;; 
(define (load-database database)
	(nldb:load-db (append databases-path database))
)

;; @syntax (Dragonfly:save-database <database>)
;; @param <database> database filename
;; <p>Saves the specified nldb database in the default
;; directory /databases.</p>
;; 
(define (save-database database)
	(nldb:save-db (append databases-path database))
)

;; @syntax (Dragonfly:show-database)
;; <p>Writes some information about the currently used nldb</p>
;;
(define (show-database)
	(println "
	<div id='dragonfly_database-information' style='border:1px dotted #00aeef; width:700px; padding:8px; margin-top:20px;' >
	<h2>Database information</h2><pre>
	")
	(nldb:show)
	(println "</pre></div>")
)

;; @syntax (Dragonfly:count-columns database tablename)
;; <p>Returns the number of columns of the given table.</p>
;;
(define (count-columns database tablename)
	(Dragonfly:load-database database)
	(set 'table tablename)
	(print (nldb:count-columns table))
)

;; @syntax (Dragonfly:count-columns <database>)
;; @param <database> database filename
;; <p>Saves the specified nldb database in the default
;; directory /databases.</p>
;; 
(define (save-database database)
	(nldb:save-db (append databases-path database))
)

;; @syntax (Dragonfly:add-row <database> <tablename> <data>)
;; @param <database> the database filename
;; @param <tablename> target table
;; @param <data> data supplied as a list - all fields must be given
;; <p>Saves the data into the specified table.</p>
;; 
(define (add-row database tablename data)
	(Dragonfly:load-database database)
	(set 'table tablename)
	(nldb:add-row table data)
	(Dragonfly:save-database database)
)

;; @syntax (Dragonfly:new-row <database> <tablename> <data>)
;; @param <database> the database filename
;; @param <tablename> target table
;; @param <data> data supplied as a list - all fields must be given
;; <p>Saves the data into the specified table.</p>
;; 
(define (new-row database tablename data)
		(Dragonfly:load-database database)
		(set 'table tablename)
		(nldb:new-row table data)
		(Dragonfly:save-database database)
)

;; @syntax (Dragonfly:create-database database)
;; <p>Creates a new nldb database in the default
;; directory /databases or loads an existing database.</p>
;;
(define (use-database databasename)
	(if (not (find databasename ( directory databases-path )) )
		(begin
			(set 'nldb:tables '()) ; prepare a clean table
			(Dragonfly:save-database databasename) ; creating database
		)
		(Dragonfly:load-database databasename) ; else load existing database
	)
)

(define (create-table database tablename fields)
	(Dragonfly:load-database database)
	(set 'existing-tables nldb:tables)
	(set 'table tablename)
	(if (not (find table existing-tables))
		(begin
			(nldb:create-table table fields)
			(Dragonfly:save-database database)
		)
		;;(println "Table not created. Found existing table.")
	)
)

(context Dragonfly)