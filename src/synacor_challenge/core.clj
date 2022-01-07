(ns synacor-challenge.core
  (:use [criterium.core])
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp])
  (:gen-class))

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

(defn multiply
  [^long x ^long y]
  (rem (* x y) 32768))

(defn nonzero?
  [^long x]
  (not (zero? x)))

(defn store
  [vm dst val]
  (if (reg? dst)
    (update vm :registers assoc (->reg dst) val)
    (update vm :memory assoc dst val)))

#_(defn write-reg
  [vm dst val]
  (if (reg? dst)
    (update vm :registers assoc (->reg dst) val)
    (-> (assoc vm :error (str "dst does not designate a register: " dst))
        (assoc vm :ip-at-error (:ip vm)))))

#_(defn write-mem
  [vm dst val]
  (if (reg? dst)
    (-> (assoc vm :error (str "dst is not a memory address: " dst))
        (assoc vm :ip-at-error (:ip vm)))
    (update vm :memory assoc dst val)))

(defn ip+
  "Advance the instruction pointer by x"
  [^long ip ^long x]
  (+ ip x))

(defn update-ip
  [vm x]
  (update vm :ip ip+ x))

(defn mem
  "Returns the value in memory at ip + x"
  ^long [{:keys [^long ip memory]} ^long x]
  (-> ip (ip+ x) memory))

(defn arg
  "Looks at the value in memory at ip + x.
  If it designates a register, returns the content of that register.
  Otherwise returns the value."
  ^long [vm ^long x]
  (let [val (mem vm x)]
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
  (let [a (mem vm 1)
        b (arg vm 2)]
    (-> (store vm a b)
        (update-ip 3))))

; push: 2 a
;  push <a> onto the stack
(defmethod execute-instruction 2
  [vm]
  (let [a (arg vm 1)]
    (-> (update vm :stack conj a)
        (update-ip 2))))

; pop: 3 a
;  remove the top element from the stack and write it into <a>; empty stack = error
(defmethod execute-instruction 3
  [vm]
  (let [a   (mem vm 1)
        val (peek (vm :stack))]
    (if val
      (-> (store vm a val)
          (update :stack pop)
          (update-ip 2))
      (assoc vm :error "Cannot pop an empty stack"))))

(defn alu-op
  "Executes an ALU operation of 3 arguments a, b, and c where a is the destination register
  and the calculation takes b and c as arguments."
  [vm calc]
  (let [a (mem vm 1)
        b (arg vm 2)
        c (arg vm 3)]
    (-> (store vm a (calc b c))
        (update-ip 4))))

; eq: 4 a b c
;  set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
(defmethod execute-instruction 4
  [vm]
  (alu-op vm #(if (= %1 %2) 1 0)))

; gt: 5 a b c
;  set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
(defmethod execute-instruction 5
  [vm]
  (alu-op vm (fn [^long x ^long y]
               (if (> x y) 1 0))))

; jmp: 6 a
;  jump to <a>
(defmethod execute-instruction 6
  [vm]
  (assoc vm :ip (arg vm 1)))


(defn cond-jmp
  [vm pred]
  (let [a (arg vm 1)]
    (if (pred a)
      (assoc vm :ip (arg vm 2))
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
  (alu-op vm add))

; mult: 10 a b c
;  store into <a> the product of <b> and <c> (modulo 32768)
(defmethod execute-instruction 10
  [vm]
  (alu-op vm multiply))

; mod: 11 a b c
;  store into <a> the remainder of <b> divided by <c>
(defmethod execute-instruction 11
  [vm]
  (alu-op vm rem))

; and: 12 a b c
;  stores into <a> the bitwise and of <b> and <c>
(defmethod execute-instruction 12
  [vm]
  (alu-op vm bit-and))

; or: 13 a b c
;  stores into <a> the bitwise or of <b> and <c>
(defmethod execute-instruction 13
  [vm]
  (alu-op vm bit-or))

; not: 14 a b
;  stores 15-bit bitwise inverse of <b> in <a>
(defmethod execute-instruction 14
  [vm]
  (let [a       (mem vm 1)
        b       (arg vm 2)
        inverse (bit-and (bit-not b) 2r111111111111111)]
    (-> (store vm a inverse)
        (update-ip 3))))

; rmem: 15 a b
;  read memory at address <b> and write it to <a>
(defmethod execute-instruction 15
  [vm]
  (let [a   (mem vm 1)
        b   (arg vm 2)
        val ((vm :memory) b)]
    (-> (store vm a val)
        (update-ip 3))))

; wmem: 16 a b
;  write the value from <b> into memory at address <a>
(defmethod execute-instruction 16
  [vm]
  (let [a (arg vm 1)
        b (arg vm 2)]
    (-> (store vm a b)
        (update-ip 3))))

; call: 17 a
;  write the address of the next instruction to the stack and jump to <a>
(defmethod execute-instruction 17
  [vm]
  (let [a       (arg vm 1)
        next-ip (ip+ (vm :ip) 2)]
    (-> (update vm :stack conj next-ip)
        (assoc :ip a))))

; ret: 18
;  remove the top element from the stack and jump to it; empty stack = halt
(defmethod execute-instruction 18
  [vm]
  (let [dst (peek (vm :stack))]
    (if dst
      (-> (update vm :stack pop)
          (assoc :ip dst))
      (assoc vm :halted true))))

; out: 19 a
;  write the character represented by ascii code <a> to the terminal
(defmethod execute-instruction 19
  [vm]
  (-> vm (arg 1) char print)
  (update-ip vm 2))

; in: 20 a
;  read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
(defmethod execute-instruction 20
  [vm]
  (let [a (mem vm 1)
        input (.read System/in)]
    (-> (store vm a input)
        (update-ip 2))))

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
    (flush)
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
      (let [low  (.read in)
            high (.read in)]
        (if (< low 0)
          (persistent! program)
          (recur (conj! program (bit-or (bit-shift-left high 8) low))))))))


(defn -main [& _args]
  (->> (load-binary)
       (load-program initial-state)
       (run-vm)
       (println-vm)))


(comment

  ;; prints [EOT] ascii symbol, then the halted VM state
  (let [test-vm     (load-program initial-state [9, 32768, 32769, 4, 19, 32768])
        finished-vm (run-vm test-vm)]
    (println)
    (println-vm finished-vm))

  (def vm (load-program initial-state (load-binary)))

  (let [finished-vm (run-vm vm)]
    (println-vm finished-vm))

  (future
    (let [finished-vm (run-vm vm)]
      (println-vm finished-vm)
      (def vm2 finished-vm)))

  ;
  )
