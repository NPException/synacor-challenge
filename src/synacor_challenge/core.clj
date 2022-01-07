(ns synacor-challenge.core
  (:use [criterium.core]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

;== architecture ==
;- three storage regions
;  - memory with 15-bit address space storing 16-bit values
;  - eight registers
;  - an unbounded stack which holds individual 16-bit values
;- all numbers are unsigned integers 0..32767 (15-bit)
;- all math is modulo 32768; 32758 + 15 => 5

(def initial-state
  {:ip        0
   :memory    (vec (repeat 32768 0))                        ;; 15-bit address space
   :registers [0 0 0 0 0 0 0 0]                             ;; 8 registers
   :stack     '()})
