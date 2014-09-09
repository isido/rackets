#! /usr/bin/env racket
#lang racket

;;
;; A simple script for parsing S Group purchase reports
;; probably useful only in Finland
;;
;; (C) 2014 Ilja Sidoroff, this file is placed in public domain.
;;

(require 2htdp/batch-io)
(require racket/cmdline)

(provide main)

; Modify these if necessary
(define header-lines 12)
(define group-column 4)
(define item-column 5)

(define (read-report-file fn)
  (drop
   (read-csv-file fn)
   header-lines))

(define (get-group item)
  (list-ref item group-column))

(define (get-price item)
  (string->number (list-ref item item-column)))

(define (get-group-and-price items)
  (map (lambda (item) (cons (get-group item) (get-price item))) items))

(define (group-item-prices items-and-prices)
  (foldl (lambda (elem v)
           (if (hash-has-key? v (car elem))
               (hash-set v (car elem) (+ (hash-ref v (car elem)) (cdr elem)))
               (hash-set v (car elem) (cdr elem))))
         (hash)
         items-and-prices))   

(define (process-file filename)
  (group-item-prices
   (get-group-and-price
    (read-report-file filename))))

(define (print-hash h)
  (for ([(item price) h])
    (printf "~a,~a\n" item price)))

(define main
  (let ([fn (command-line
             #:args (filename)
             filename)])
    (print-hash (process-file fn))))
