(ns sicp.ch5)

(defn read-num [] (Integer. (read-line)))
(defn print-num [n] (println n))
(def id identity)
(defn abs [x] (max x (- x)))

(def fact-iter-machine
  "Exercise 5.2, expects the number to take the factorial of in :n, terminates when done"
   [[:assign :a id 1]
    [:assign :b id 1]
    'fact-loop
    [:test > :b :n]
    [:branch 'fact-done]
    [:assign :a * :a :b]
    [:assign :b inc :b]
    [:goto 'fact-loop]
    'fact-done
    [:finish]])

(def sqrt-iter-machine
  "Exercise 5.3, expects the number to take the square root of in :x, terminates when done"
  (let [improve-machine
        [[:assign :t / :x :guess]
         [:assign :t + :guess :t]
         [:assign :guess / :t 2]]

        good-enough?-machine
        [[:assign :t * :guess :guess]
         [:assign :t - :t :x]
         [:assign :t abs :t]
         [:test < :t 0.001]]

        remainder-machine
        ['rem-test
         [:test < :n :d]
         [:branch 'rem-done]
         [:assign :n - :n :d]
         [:goto 'rem-test]
         'rem-done]]
    (concat 
     ['sqrt-main
      [:assign :guess id 1] ;; start with a guess of 1
      'sqrt-loop]  ;; loop label
     good-enough?-machine ;; perform good-enough? test
     [[:branch 'sqrt-done]] ;; if it's true, goto done
     improve-machine ;; perform improve (in-place)
     [[:goto 'sqrt-loop] ;; repeat
      'sqrt-done
      [:finish]])))

(def exp-recur-machine
  "Exercise 5.4a, expects base in :b and exponent in :n"
   ['exp-main
    [:test = :n 0]
    [:branch 'base-case]
    [:save :continue]
    [:save :n] 
    ;; no need to save b - we know the exact characteristics of where
    ;; we are going, and it doesn't touch b
    [:assign :n - :n 1]
    [:assign :continue id 'exp-recur-cont]
    [:goto 'exp-main]
    'exp-recur-cont ;; the answer to (expt b (- n 1)) will be in :val
    [:restore :n]
    [:assign :val * :b :val] ;; calculate (expt b n) and put it in :val
    [:restore :continue]
    [:goto :continue] ;; return
    'base-case
    [:assign :val id 1]
    [:goto :continue]]) ;; only n affects the depth/how many steps the computation lasts
    
(def exp-iter-machine
  "Exercise 5.4b, expects base in :b and exponent in :n"
  ['exp-main
   [:assign :val id 1]
   'exp-loop
   [:test = :n 0]
   [:branch :continue]
   [:assign :n - :n 1]
   [:assign :val * :val :b]
   [:goto 'exp-loop]]) ;; only n affects the depth/how many steps the computation lasts

;; Exercise 5.7: I did and it was fun
;; Exercise 5.8: 3. Modification not done.
;; Exercise 5.9: Not done
;; Exercise 5.10: Yes. Not done
;; Exercise 5.11: a. Done (see the machine). b. and c. Not done
;; Exercise 5.12: TODO write these analysis functions on my own stuff
;; Exercise 5.13: I already do this, great!

(def fib-recur-machine
  "expects n in :n"
  ['fib-main
   [:test < :n 2]
   [:branch 'base-case]
   [:save :continue] ;; calculating a new value, save continuation so we know where to return it
   [:save :n] ;; calculate (fib (- n 1)) first
   [:assign :n - :n 1] 
   [:assign :continue id 'after-fib-1] ;; and return it to this continuation
   [:goto 'fib-main]
   'after-fib-1
   [:restore :n] 
   ;; [:restore :continue] ;; Unnecessary, Exercise 5.6
   [:save :val] ;; save the val of (fib (- n 1))
   [:assign :n - :n 2] ;; calculate (fib (- n 2))
   ;; [:save :continue] ;; Unnecessary, Exercise 5.6
   [:assign :continue id 'after-fib-2] ;; and return it to this continuation
   [:goto 'fib-main]
   'after-fib-2
   [:assign :b id :val] ;; store the value of (fib (- n 2)) in :b
   [:restore :a] ;; restore the :val of (fib (- n 1)) to :a (Exercise 5.11a)
   [:assign :val + :a :b] ;; add them to calculate (fib n)
   [:restore :continue] ;; return it to the saved continuation
   [:goto :continue]
   'base-case
   [:assign :val id :n]
   [:goto :continue]])

(defn machine-wrapper
  "Takes register/initializer pairs and a machine, and returns a
  wrapped machine with that initialization and :continue initialized
  to jump to a label that ends the computation
  Example: (machine-wrapper [:b 3 :n 4] exp-iter-machine)"
  [start-vals machine]
  (let [registers (take-nth 2 start-vals)
        vals (take-nth 2 (rest start-vals))]
    (concat
     [[:assign :continue id 'finish]]
     (map #(vec (list :assign %1 id %2)) registers vals)
     machine
     ['finish
      [:finish]])))

(defn fact-recur-machine
  [n]
  [[:assign :continue id 'fact-done]
   [:assign :n id n]
   'fact-loop
   [:test = :n 1]
   [:branch 'base-case]
   [:save :continue] ;; create a new continuation by saving the continuation it should return to,
   [:save :n] ;; storing the data it expects,  
   [:assign :continue id 'after-fact] ;; and putting the new continuation in our continue register
   [:assign :n - :n 1]  ;; a small amount of in-place work
   [:goto 'fact-loop] ;; enter the new computation!
   'after-fact
   [:restore :n] ;; restore this continuation's data
   [:assign :val * :n :val] ;; execute the continuation
   [:restore :continue] ;; and return the result to the saved continuation
   [:goto :continue] 
   'base-case
   [:assign :val id 1]
   [:goto :continue]
   'fact-done
   [:finish]])

(def looping-fact-iter-machine
  "Reads from stdin, prints to stdout, eternally"
   ['main-loop
    [:assign :n read-num]
    [:assign :a id 1]
    [:assign :b id 1]
    'fact-test
    [:test > :b :n]
    [:branch 'fact-done]
    [:assign :a * :a :b]
    [:assign :b inc :b]
    [:goto 'fact-test]
    'fact-done
    [:perform print-num :a]
    [:goto 'main-loop]])

(def init-state {:pc 0})

(defn run-machine 
  [start-state machine]
  ;; OH MY GOD I LOVE CLOJURE
  (take-while 
   #(not (:done %))
   (iterate #(step-machine % machine) start-state)))
   
(defn step-machine 
  "Returns the new state"
  [{:keys [pc done] :as curstate} machine]
  (let [next (nth machine pc)]
    (cond done curstate
          (symbol? next) (exec-noop curstate)
          :else (exec-instruction curstate next machine))))

(defn val-of [val curstate]
  (if (keyword? val)
    (or (curstate val) (throw (Exception. "val-of: Tried to get val of uninitialized register!")))
    val))

(defn exec-instruction 
  "Returns the new state"
  [curstate [inst & rest] machine]
  (case inst
    :assign (exec-assign curstate rest)
    :test (exec-test curstate rest)
    :branch (exec-branch curstate rest machine)
    :goto (exec-goto curstate rest machine)
    :save (exec-save curstate rest)
    :restore (exec-restore curstate rest)
    :perform (exec-assign curstate (cons :devnull rest))
    :finish (assoc curstate :done true)
    ))

(defn exec-save
  "Returns the new state"
  [{:keys [pc stack] :as curstate} [operand]]
    (assoc curstate
      :stack (cons (val-of operand curstate) stack)
      :pc (inc pc)))
    
(defn exec-restore
  "Returns the new state"
  [{:keys [pc stack] :as curstate} [dest]]
    (assoc curstate
      :stack (rest stack)
      dest (first stack)
      :pc (inc pc)))

(defn exec-assign
  "Returns the new state"
  [{:keys [pc] :as curstate} [dest op & operands]]
  (let [operand-vals (map #(val-of % curstate) operands)]
    (assoc curstate
      dest (apply op operand-vals)
      :pc (inc pc))))

(defn exec-test
  "Returns the new state"
  [{:keys [pc] :as curstate} [test & operands]]
  (let [operand-vals (map #(val-of % curstate) operands)]
  (assoc curstate 
    :test (apply test operand-vals)
    :pc (inc pc))))

(defn exec-noop
  "Returns the new state"
  [{:keys [pc] :as curstate}]
  (assoc curstate 
    :pc (inc pc)))

(defn exec-branch
  "Returns the new state"
  [{:keys [test] :as curstate} [dest] machine]
  (if test 
    (exec-goto curstate [dest] machine)
    (exec-noop curstate)))

(defn exec-goto
  "Returns the new state"
  [curstate [dest] machine]
  (assoc curstate 
    :pc (.indexOf machine (val-of dest curstate))))
