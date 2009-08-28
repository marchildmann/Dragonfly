#!/usr/bin/env newlisp
;; Dragonfly web framework
;; 
;; This software is copyright (c) Marc Hildmann, 2009. All rights reserved.
;; It is licensed by the MIT License http://www.opensource.org/licenses/mit-license.php
;;
;; Start this application on your localhost OS X
;; 
;; newlisp -http -d PORT -w /Users/USERNAME/Sites/DIRECTORY &
;; e.g. newlisp -http -d 8080 -w /Users/marc/Sites/newlisp_financier &
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load Dragonfly web framework

(load "./dragonfly-framework/dragonfly.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main entry point
;;

(define (run)

	(Dragonfly:listener)

)

(run)