(ns synacor-challenge.core
  (:use [criterium.core])
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]))

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

(defn println-vm
  [vm]
  (pp/pprint (dissoc vm :memory)))

;== binary format ==
;- each number is stored as a 16-bit little-endian pair (low byte, high byte)
;- numbers 0..32767 mean a literal value
;- numbers 32768..32775 instead mean registers 0..7
;- numbers 32776..65535 are invalid
;- programs are loaded into memory starting at address 0
;- address 0 is the first 16-bit value, address 1 is the second 16-bit value, etc

(defn ->reg
  "Returns the register index of the given value"
  [^long x]
  (- x 32768))

(defn reg?
  "Returns true if the value is a register address"
  [^long x]
  (>= x 32768))

(defn add
  [^long x ^long y]
  (rem (+ x y) 32768))

(defn nonzero?
  [^long x]
  (not (zero? x)))

(defn store
  [vm dst val]
  (if (reg? dst)
    (update vm :registers assoc (->reg dst) val)
    (-> (assoc vm :error (str "Value does not designate a register: " dst))
        (assoc vm :ip-at-error (:ip vm)))
    ;; this commented out form would instead store the value directly into the memory
    #_(update vm :memory assoc dst val)))

(defn ip+
  "Advance the instruction pointer by x"
  [^long ip ^long x]
  (+ ip x))

(defn update-ip
  [vm x]
  (update vm :ip ip+ x))

(defn arg
  "Returns the value in memory at ip + x"
  ^long [{:keys [^long ip memory]} ^long x]
  (-> ip (ip+ x) memory))

(defn arg-load
  "Looks at the value in memory at ip + x.
  If it designates a register, returns the content of that register.
  Otherwise returns the value."
  ^long [vm ^long x]
  (let [val (arg vm x)]
    (if (reg? val)
      ((vm :registers) (->reg val))
      val)))


;; opcode handling ;;

(defn opcode
  [{:keys [ip memory] :as _vm}]
  (memory ip))

(defmulti execute-instruction opcode)

(defmethod execute-instruction :default
  [vm]
  (assoc vm :error (str "Unimplemented opcode: " (opcode vm))))

; halt: 0
;  stop execution and terminate the program
(defmethod execute-instruction 0
  [vm]
  (assoc vm :halted true))

; set: 1 a b
;  set register <a> to the value of <b>
(defmethod execute-instruction 1
  [vm]
  (let [a (arg vm 1)
        b (arg-load vm 2)]
    (-> (store vm a b)
        (update-ip 3))))

; push: 2 a
;  push <a> onto the stack
(defmethod execute-instruction 2
  [vm]
  (let [a (arg-load vm 1)]
    (-> (update vm :stack conj a)
        (update-ip 2))))

; pop: 3 a
;  remove the top element from the stack and write it into <a>; empty stack = error
(defmethod execute-instruction 3
  [vm]
  (let [a (arg vm 1)
        val (peek (vm :stack))]
    (if val
      (-> (store vm a val)
          (update :stack pop)
          (update-ip 2))
      (assoc vm :error "Cannot pop an empty stack"))))

(defn cond-store
  [vm pred]
  (let [a (arg vm 1)
        b (arg-load vm 2)
        c (arg-load vm 3)]
    (-> (store vm a (if (pred b c) 1 0))
        (update-ip 4))))

; eq: 4 a b c
;  set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
(defmethod execute-instruction 4
  [vm]
  (cond-store vm =))

; gt: 5 a b c
;  set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
(defmethod execute-instruction 5
  [vm]
  (cond-store vm >))

; jmp: 6 a
;  jump to <a>
(defmethod execute-instruction 6
  [vm]
  (assoc vm :ip (arg-load vm 1)))


(defn cond-jmp
  [vm pred]
  (let [a (arg-load vm 1)]
    (if (pred a)
      (assoc vm :ip (arg-load vm 2))
      (update-ip vm 3))))

; jt: 7 a b
;  if <a> is nonzero, jump to <b>
(defmethod execute-instruction 7
  [vm]
  (cond-jmp vm nonzero?))

; jf: 8 a b
;  if <a> is zero, jump to <b>
(defmethod execute-instruction 8
  [vm]
  (cond-jmp vm zero?))

; add: 9 a b c
;  assign into <a> the sum of <b> and <c> (modulo 32768)
(defmethod execute-instruction 9
  [vm]
  (let [a (arg vm 1)
        b (arg-load vm 2)
        c (arg-load vm 3)]
    (-> (store vm a (add b c))
        (update-ip 4))))

; out: 19 a
;  write the character represented by ascii code <a> to the terminal
(defmethod execute-instruction 19
  [vm]
  (-> vm (arg-load 1) char print)
  (update-ip vm 2))

; noop: 21
;  no operation
(defmethod execute-instruction 21
  [vm]
  (update-ip vm 1))


;; main ;;

(defn run-vm
  "Runs the vm until it halts or errors"
  [vm]
  (loop [vm vm]
    (if (or (:halted vm) (:error vm))
      vm
      (recur (execute-instruction vm)))))


(defn load-program
  [vm program]
  (update vm :memory
    (fn [memory]
      (vec (concat program (subvec memory (count program)))))))


(defn load-binary
  []
  (with-open [in (io/input-stream (io/resource "challenge.bin"))]
    (loop [program (transient [])]
      (let [low (.read in)
            high (.read in)]
        (if (< low 0)
          (persistent! program)
          (recur (conj! program (bit-or (bit-shift-left high 8) low))))))))


(comment

  ;; prints [EOT] ascii symbol, then the halted VM state
  (let [test-vm (load-program initial-state [9, 32768, 32769, 4, 19, 32768])
        finished-vm (run-vm test-vm)]
    (println)
    (println-vm finished-vm))

  (def program (load-binary))

  (def vm (load-program initial-state program))

  (let [finished-vm (run-vm vm)]
    (println-vm finished-vm))

  (let [finished-vm (run-vm vm)]
    (println-vm finished-vm)
    (def vm2 finished-vm))

  ;
  )
