(ns push307.core
  (:gen-class)
  (:require [push307.translate :as trans])
  )

(import push307.SpaceInvaders)
(import push307.Board)
;; (import push307.GameState)
(import java.awt.EventQueue)

;;;;;;;;;;
;; Examples

;; An example Push state
(def example-push-state
  {:exec '(int_+ int_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :boolean '(true false)
   :input {:in1 4 :in2 6}})

;; An example Push program
(def example-push-program
  '(3 5 int_* "hello" 4 "world" int_-))

(def example-do-range
  '(1 6 exec_do*range "hello"))

;; An example individual in the population
;; Made of a map containing, at minimum, a program, the errors for
;; the program, and a total error
(def example-individual
  {:program '(3 5 int_* "hello" 4 "world" int_-)
   :errors [8 7 6 5 4 3 2 1 0 1]
   :genome '()
   :total-error 37})


(def genome-example
  {:program '()
  :error []
  :genome '({:instruction true :silent false :close 0}
            {:instruction false :silent false :close 0}
            {:instruction int_+ :silent false :close 0}
            {:instruction bool_not :silent false :close 0})
  :total-errors 0})

;; The empty individual
(def empty-individual
  {:program '()
   :errors []
   :genome '()
   :total-error 0})

(defn abs
  "Takes the absolute value of a number."
  [x]
  (if (< x 0)
    (- x)
    x))

;;;;;;;;;;
;; Instructions must all be either functions that take one Push
;; state and return another or constant literals.
(def instructions
  (list
   ;; 'in1

   ;; integer operators
   `int_+
   `int_-
   `int_*
   `int_%
   `int_=
   `int_<
   `int_>
   `int_dup
   `int_flush
   `int_swap

   ;; boolean operators
   `bool_=                           ; logical equivalence
   `bool_and                         ; logical and
   `bool_dup                         ; duplicate top boolean 
   `bool_flush                       ; empty the boolean stack
   ; `bool_not                         ; logical not
   `bool_or                          ; logical or
   `bool_pop                         ; pop top element of boolean stack
   `bool_swap                        ; swaps the top two booleans

   ;; exec instructions
   ;; see http://faculty.hampshire.edu/lspector/push3-description.html for
   ;; detailed descriptions
   `exec_=
   `exec_dup
   `exec_pop
   `exec_if
   `exec_do*range
   
   ;; literals
   0
   1
   true
   false
   "Left"
   "Right"
   ))

;;;;;;;;;;
;; Utilities

;; The initial empty push state
(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '()
   :input {}})

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (assoc state stack (conj (get state stack) item)))

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (assoc state stack (rest (get state stack))))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (get state stack)))

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty-stack? state stack)
    :no-stack-item
    (first (get state stack))))

(defn not-enough-args
  "takes a vector of args and checks if they contain :no-stack-item
  returns true if it does, false otherwise"
  [vec]
  (some #(= :no-stack-item %) vec))

(defn swap-stack
  "Swaps the top two items of a stack. If stack has <2 items,
  returns :no-stack-item"
  [state stack]
  (let [first-item (peek-stack state stack)
        temp-state (pop-stack state stack) 
        second-item (peek-stack temp-state stack)]
    (if (not-enough-args [first-item second-item])
      state
      (push-to-stack (push-to-stack (pop-stack temp-state stack)
                                    stack
                                    first-item)
                     stack
                     second-item))))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough
  args on each of the desired stacks, returns a map of the form {:state :args},
  where :state is the new state with args popped, and :args is a list of args
  from the stacks. If there aren't enough args on the stacks, returns
  :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          ;; The instruction required too many arguments
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to
  return the result to. Applies the function to the args (taken from the stacks)
  and pushes the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;;
;; Instructions

(def legal-instructions
  `(                
    int_+           ; y
    int_-           ; y
    int_*           ; y
    int_%           ; y
    int_=           ; y
    int_<           ; y
    int_>           ; y
    int_dup         ; y
    int_flush       ; y
    int_swap        ; y
    bool_=          ; y
    bool_and        ; y              
    bool_dup        ; y               
    bool_flush      ; y               
    bool_not        ; y              
    bool_or         ; y               
    bool_pop        ; y                
    bool_swap       ; y                
    exec_=          ; y
    exec_dup        ; y
    exec_pop        ; y
    exec_if         ; y
    exec_do*range   ; y
    get_player_x    ; y
    get_player_y    ; y
    shot_exists     ; y
    get_shot_x      ; y
    get_shot_y      ; y
    get_alien_x     ; y
    get_alien_y     ; y
    get_bomb_x      ; y
    get_bomb_y      ; y
    get_alien_dist  ; y
    get_bomb_dist   ; y
    ))



;; Integer operations

(defn int_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn int_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top
  integer."
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn int_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn int_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (make-push-instruction state
                         #(if (= 0 %2)
                            %1
                            (quot %1 %2))
                         [:integer :integer]
                         :integer
                         ))
                         
(defn int_=
  "Pushes TRUE onto the boolean stack if the top two integers on the integer
  the stack are equal. FALSE otherwise."
  [state]
  (make-push-instruction state = [:integer :integer] :integer))                      
   
(defn int_<
  "Pushes TRUE onto the BOOLEAN stack if the second item is less than the top item.
  FALSE otherwise."
  [state]
  (make-push-instruction state < [:integer :integer] :integer))

(defn int_>
  "Pushes TRUE onto the BOOLEAN stack if the second item is greater than the top item.
  FALSE otherwise."
  [state]
  (make-push-instruction state > [:integer :integer] :integer))

(defn int_dup
  "Duplicates the top integer on the integer stack."
  [state]
  (let [top (peek-stack state :integer)]
    (if (= top :no-stack-item)
      state
      (push-to-stack state :integer (peek-stack state :integer)))))
   
(defn int_flush
  "Empties the integer stack."
  [state]
  (assoc state :integer '()))

(defn int_swap
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (swap-stack state :integer))




;; Boolean operations

(defn bool_=
  "Takes the top two booleans and leaves true or false, whether they are
  equal, on the boolean stack."
  [state]
  (make-push-instruction state
                         =
                         [:boolean :boolean]
                         :boolean))

(defn bool_and
  "Takes the top two booleans and leaves the logical and of those two on
  the boolean stack."
  [state]
  (make-push-instruction state
                         'and
                         [:boolean :boolean]
                         :boolean))

(defn bool_dup
  "Duplicates the top boolean on the boolean stack."
  [state]
  (let [top (peek-stack state :boolean)]
    (if (= top :no-stack-item)
      state
      (push-to-stack state :boolean (peek-stack state :boolean)))))

(defn bool_flush
  "Empties the boolean stack."
  [state]
  (assoc state :boolean '()))

(defn bool_not
  "Pushes the logical not of the top boolean."
  [state]
  (make-push-instruction state
                         'not
                         [:boolean]
                         :boolean))
(defn bool_or
  "Pushes the logical or of the top two booleans."
  [state]
  (make-push-instruction state
                         'or
                         [:boolean :boolean]
                         :boolean))

(defn bool_pop
  "Pops the top item of the boolean stack"
  [state]
  (pop-stack state :boolean))

(defn bool_swap
  "Swaps the top two items of the boolean stack if the stack has 2 or more items"
  [state]
  (swap-stack state :boolean))

  

  
;; Exec operations

(defn exec_=
  "pushes TRUE to the boolean stack if the top two items of the exec stack are
  the same. FALSE otherwise."
  [state]
  (let [first-item (peek-stack state :exec)]
  (if (= first-item :no-stack-item)
    state
    (let [temp-state (pop-stack state :exec) 
          second-item (peek-stack temp-state :exec)]
      (push-to-stack state
                     :boolean
                     (if (= '(first-item) '(second-item))
                       true
                       false))))))
(defn exec_dup
  "Pushes the top item of the exec stack onto the exec stack"
  [state]
  (let [top (peek-stack state :exec)]
    (if (= top :no-stack-item)
      state
      (push-to-stack state :exec (peek-stack state :exec)))))

(defn exec_pop
  "Pops the top item of the exec stack"
  [state]
  (pop-stack state :exec))

(defn exec_swap
  "Swaps the top two items of the exec stack if the stack has 2 or more items"
  [state]
  (swap-stack state :exec))

(defn exec_if
  "If the top item of the boolean stack is false, pop the top item of the exec
  stack. If the top item of the "
  [state]
  (let [first-exec (peek-stack state :exec)
        second-exec (peek-stack (pop-stack state :exec) :exec)
        bool (peek-stack state :boolean)]
    ; check if there are enough args
    (if (not-enough-args [first-exec second-exec bool])
      state
      (if (= false bool)
        (pop-stack state :exec)
        (push-to-stack (pop-stack (pop-stack state :exec) :exec)
                       :exec
                       first-exec)))))

          
(defn exec_do*range
  "Loops a piece of code for a number of iterations
  based on the integer stack"
  [state]
  (let [dest-indx (peek-stack state :integer)
        curr-indx (peek-stack (pop-stack state :integer) :integer)
        loop-code (peek-stack state :exec)]
    ; check if there are enough args
    (if (not-enough-args [dest-indx curr-indx loop-code])
      state
      (if (= dest-indx curr-indx)
        (pop-stack state :integer)
        (let [next-indx (+ curr-indx (if (< dest-indx curr-indx) -1 1))]
          (push-to-stack
           (push-to-stack
            (push-to-stack
             (push-to-stack
              (pop-stack state :integer)
              :exec
              'exec_do*range)
             :exec
             dest-indx)
            :exec
            next-indx)
           :exec
           (peek-stack state :exec)))))))


(defn get-gamestate-info
  [state name]
  (get (get state :input) name))

(defn get_player_x
  [state]
  (push-to-stack state :integer (get-gamestate-info state :player_x)))

(defn get_player_y
  [state]
  (push-to-stack state :integer (get-gamestate-info state :player_y)))

(defn shot_exists
  [state]
  (push-to-stack state :boolean (get-gamestate-info state :shot_exists)))

(defn get_shot_x
  [state]
  (push-to-stack state :integer (get-gamestate-info state :shot_x)))

(defn get_shot_y
  [state]
  (push-to-stack state :integer (get-gamestate-info state :shot_y)))

(defn get_alien_x
  [state]
  (push-to-stack state :integer (nth (get-gamestate-info state :alien_position) 0)))

(defn get_alien_y
  [state]
  (push-to-stack state :integer (nth (get-gamestate-info state :alien_position) 1)))

(defn get_bomb_x
  [state]
  (push-to-stack state :integer (nth (get-gamestate-info state :bomb_position) 0)))

(defn get_bomb_y
  [state]
  (push-to-stack state :integer (nth (get-gamestate-info state :bomb_position) 1)))

(defn get_alien_dist
  [state]
  (push-to-stack state :integer (get-gamestate-info state :alien_distance)))

(defn get_bomb_dist
  [state]
  (push-to-stack state :integer (get-gamestate-info state :bomb_distance)))

;;;;;;;;;;
;; Interpreter

(defn push-program-to-exec
  "Takes a state and a program and pushes each instruction in the program to
  the top of the exec stack."
  [state prog]
  (cond (empty? prog) state
        :else (push-to-stack (push-program-to-exec state (rest prog))
                             :exec
                             (first prog))))

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Returns the new Push state."
  [push-state]
  (let [top (peek-stack push-state :exec)
        top_type (type top)
        state (pop-stack push-state :exec)]
    ;; determine which stack the top goes into or if it is an instruction
    ; (println "legal-instructions")
    ; (println legal-instructions)
    ; (println "top")
    ; (println top)
    ; (println "namespace:")
    ; (println (namespace top))
    (cond
      ;; empty sublist
      (= top_type (type '())) state
      ;; sublist
      (= top_type (type '(1))) (push-program-to-exec state top)
      ;; case for string
      (= top_type (type "")) (push-to-stack state :string top)
      ;; both java.lang.Long and java.lang.Integer
      (or (= top_type (type 0))
          (= top_type (type (int 0)))) (push-to-stack state :integer top)
      ;; case for booleans
      (= top_type (type true)) (push-to-stack state :boolean top)
      ;; case for legal instructions
      (some #(= top %) legal-instructions) ((eval top) state)
      ;; illegal instructions return :illegal-instruction
      :else :illegal-instruction)))
      

(defn direction-command-to-int
  [c]
  (cond (= "Right" c) 1
        (= "Left" c) 2
        :else 0))

(defn bool-to-int
  [b]
  (if b 1 0))

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing."
  [program start-state]
  (let [state (push-program-to-exec start-state program)]
    (loop [curr-state state]
      (if (empty-stack? curr-state :exec)
        curr-state
        (recur (interpret-one-step curr-state))))))

(defn gs-to-map
  [gs]
  {:player_x (nth (vec (.getPlayerPosition gs)) 0)
   :player_y (nth (vec (.getPlayerPosition gs)) 1)
   :shot_exists (.playerShotExists gs)
   :shot_x (nth (vec (.getShotPosition gs)) 0)
   :shot_y (nth (vec (.getShotPosition gs)) 1)
   :alien_position (vec (.getAlienPosition gs))
   :bomb_position (vec (.getBombPosition gs))
   :alien_distance (.distanceToNearestAlien gs)
   :bomb_distance (.distanceToNearestBomb gs)
   })

(defn java-push-interpreter
  [gs prog]
  (let [end-state (interpret-push-program
                   prog
                   (push-to-stack empty-push-state :input (gs-to-map gs)))]
    ;; (println end-state)
    ;; (println (peek-stack end-state :string))
    ;; (println (peek-stack end-state :boolean))
    ;; (println "End state:")
    ;; (println end-state)
    (java.util.ArrayList. [(direction-command-to-int
                            (peek-stack end-state :string))
                           (bool-to-int (peek-stack end-state :boolean))])))


;;;;;;;;;;
;; GP

(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-initial-program-size]
  ;; size of the program will be <=  the given max size
  (repeatedly (+ (rand-int (- max-initial-program-size 5)) 5) 
                     #(rand-nth instructions)))
                                       

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population tournament-size]
  ;; get a random sample of individuals
  ;; get their errors
  ;; find the minimum error value, then get the individual(s) with that value
  (let [sample (repeatedly tournament-size #(rand-nth population))
        errors (map #(get % :total-error) sample)
        min-error (apply min errors)
        min-individuals (filter #(= min-error (get % :total-error)) sample)]
    (rand-nth min-individuals)))

(defn choose-50%-chance
  "Takes a list containing one or two elements. If the list contains two
  elements then a list containing one of the elements is returned with an equal
  chance for either element being returned. If the list contains only one
  element then there is a 50% chance that a list containing only the element
  will be returned and a 50% chance that the empty list will be returned."
  [lst]
  (cond (empty? lst) (throw (AssertionError.
                             "choose-50%-chance expected a non-empty list"))
        (= (count lst) 1) (if (< (rand) 0.5) lst '())
        (= (count lst) 2) (list (rand-nth lst))
        :else (throw (AssertionError.
                      "choose-50%-chance expected a list of 1 or 2 elements"))))

(defn crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns child program."
  [prog-a prog-b]
  (loop [p1 prog-a
         p2 prog-b
         child '()]
    ;; base case: if both are empty, return an empty list
    (if (and (empty? p1) (empty? p2))
      child
      (recur
       (rest p1)
       (rest p2)
       (concat
        child
        ;; choose which parent to pick from
        (choose-50%-chance (concat
                            ;; check if parent is empty so as to not return nil
                            (if (empty? p1) '() (list (first p1)))
                            (if (empty? p2) '() (list (first p2))))))))))

(defn element-of-list-chance
  "Takes a list. 5% of the time, returns a list containing one ranodm element of
  the list. Otherwise, the empty list is returned."
  [lst]
  (if (<= (rand) 0.05) (list (rand-nth lst)) '()))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the program) with some probability. Returns child program."
  [prog]
  (loop [parent prog
         child '()]
    ;; base case: if the parent is empty, return a random length 1 program
    (if (empty? parent)
      (concat child (element-of-list-chance instructions))
      (recur (rest parent)
             (concat
              child
              (element-of-list-chance instructions)
              (list (first parent)))))))

(defn element-keep-chance
  "Takes an element as a parameter. 5% of the time, a list containing that
  element is returned. Otherwise, the empty list is returned."
  [element]
  (if (<= (rand) 0.95) (list element) '()))

(defn uniform-deletion
  "Takes a list 'program' and removes 0 or more elements from the list. Each
  element has a 5% chance of being removed."
  [program]
  ;; initialize loop parameters
  (loop [parent program
         child '()]
    ;; base case, return the child
    (if (empty? parent)
      child
      ;; decide if first element is being kept, then loop with the rest of the
      ;; program
      (recur (rest parent)
             (concat child (element-keep-chance (first parent)))))))


(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 80% chance to crossover,
  10% to uniform-addition, and 10% to uniform-deletion."
  [population tournament-size]
  ;; Start building the new individual to be returned. Rest of function to make
  ;; its program
  (assoc empty-individual :genome
         ;; always need to choose 1 parent, random num to determine gen operator
         (let [parent1 (tournament-selection population tournament-size)
               rand-number (rand)]
           ;; perform uniform addition
           (cond (> 0.1 rand-number) (uniform-addition (get parent1 :program))
                 ;; perform uniform deletion
                 (> 0.2 rand-number) (uniform-deletion (get parent1 :program))
                 ;; perform crossover
                 :else (crossover (get parent1 :program)
                                  ;; need a second parent for crossover
                                  (get (tournament-selection
                                        population
                                        tournament-size)
                                       :program))))))


;;;;;;;;;;;;
;;Plush GP

(defn make-random-plush-gene
  "Makes a random plush gene based off an instruction set, chance to be silent
  and a maximum number of close parens"
  [instructions silent-chance max-close]
  {:instruction (rand-nth instructions)
   :silent (if (> (rand) silent-chance) true false)
   ; random number of closed parens
   :close (rand-int max-close)})

(defn evolve-close
  "Gives a small chance for close to be randomized"
  [gene change-close]
  (if (> (rand) change-close)
    gene
    ; randomly increments or decrements the close value
    (let [close-gene (get gene :close)]
      (if (= close-gene 0)
        (assoc gene :close (inc close-gene))
        (assoc gene :close ((eval (rand-nth '(inc dec))) close-gene))))))

(defn make-random-plush-genome
  "Creates and returns a new genome. Takes a list of instructions and
  a maximum initial program size."
  [instrustions max-init-prog-size]
  (loop [genome '()
         instr-left  (inc (rand-int max-init-prog-size))]
    (if (> instr-left 0)
      (recur (conj genome
                   (make-random-plush-gene instructions 0.95 2))
             (dec instr-left))
      genome)))

(defn plush-crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns child program."
  [genome-a genome-b]
  (loop [g1 genome-a
         g2 genome-b
         child '()]
    ;; base case: if both are empty, return an empty list
    (if (and (empty? g1) (empty? g2))
      child
      (recur
       (rest g1)
       (rest g2)
       (concat
        child
        ;; choose which parent to pick from
        (choose-50%-chance (concat
                            ;; check if parent is empty so as to not return nil
                            (if (empty? g1) '() (list (evolve-close (first g1) 0.05)))
                            (if (empty? g2) '() (list (evolve-close (first g2) 0.05))))))))))

(defn random-plush-chance
  "Takes a list. 5% of the time, returns a list containing one ranodm element of
  the list. Otherwise, the empty list is returned."
  [lst chance]
  (if (<= (rand) chance)
    (list (make-random-plush-gene lst 0.05 2))
    '()))

(defn uniform-plush-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the program) with some probability. Returns child program."
  [prog]
  (loop [parent prog
         child '()]
    ;; base case: if the parent is empty, return a random length 1 program
    (if (empty? parent)
      (concat child (random-plush-chance instructions 0.05))
      (recur (rest parent)
             (concat
              child
              (random-plush-chance instructions 0.05)
              (list (first parent)))))))


(defn plush-select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 80% chance to crossover,
  10% to uniform-addition, and 10% to uniform-deletion."
  [population tournament-size]
  ;; Start building the new individual to be returned. Rest of function to make
  ;; its program
  (assoc empty-individual :genome
         ;; always need to choose 1 parent, random num to determine gen operator
         (let [parent1 (tournament-selection population tournament-size)
               rand-number (rand)]
           ;; perform uniform addition
           (cond (> 0.1 rand-number) (uniform-addition (get parent1 :genome))
                 ;; perform uniform deletion
                 (> 0.2 rand-number) (uniform-deletion (get parent1 :genome))
                 ;; perform crossover
                 :else (crossover (get parent1 :genome)
                                  ;; need a second parent for crossover
                                  (get (tournament-selection
                                        population
                                        tournament-size)
                                       :genome))))))


(defn run-game
  [prog seed]
  (.getResult (SpaceInvaders. prog seed)))
  
; (apply list
;                                      `(shot_exists				       
; 				       true
; 				       bool_or
; 				       ))
;                               100)))

;;;;;;;;::::
;;GP LOOP


(defn get-best-individual
  "This takes a population of individuals and determines which one has the
  best fitness score."
  [population]
  (let [best-error (apply min (map #(get % :total-error) population))]
    (first (filter #(= best-error (get % :total-error)) population))))

(defn report
  "Reports information on the population each generation. Looks similar to the
  following.

  -------------------------------------------------------
               Report for Generation 3
  -------------------------------------------------------
  Best program:
  (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1
  integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_*
  integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1
  integer_%)
  Best program size: 33
  Best total error: 727
  Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)"
  [population generation]
  (println "-------------------------------------------------------")
  (print "             Report for Generation ")
  (println generation)
  (println "-------------------------------------------------------")
  (println (rand-nth population))
  (println "Best program:")
  (let [best-individual (get-best-individual population)]
    (println (get best-individual :program))
    (print "\nBest program genome: ")
    (println (get best-individual :genome))
    (print "\nBest program size: ")
    (println (count (get best-individual :program)))
    (print "\nBest total error: ")
    (println (get best-individual :total-error))
    (print "\nBest errors: ")
    (println (get best-individual :errors))))

(defn initialize-plush-population
  "Takes a population size, list of instructions, and maximum size for any Push
  program and returns a population of random Push programs built out of the
  given instructions and with each program having at most a number of
  instructions equal to the maximum size."
  [population-size
   instructions
   max-size]
  (repeatedly
   population-size
   ;; assigns the program field the random program
   #(assoc
     empty-individual
     :genome
     ;; make a random program based on the instruction set up to the maximum
     ;; size 
     (make-random-plush-genome instructions max-size))))

;;;;;;;;;;
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  (+ (* x x x) x 3))

;; This is the list of test inputs for the push program.
(def test-cases
  (range 30 80 5))

;; This is the list of target values for the fitness function.
(def test-targets
  (map target-function test-cases))

(defn fitness
  "This is the fitness function for the problem. This should be changed for each
  problem. Takes two numbers: the result of running the test case through the
  push program and the target value."
  [result-val target]
  (if (= result-val :no-stack-item)
    100000   ; this is the penalty
    (abs (- result-val target))))


(defn game-error-function
  [individual stack]
  (let [program (get individual :program)
        errors (pmap #(run-game program %) test-cases)]
    (assoc (assoc individual :errors errors) :total-error (apply + errors))))

(defn regression-error-function
  "Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the input set to :in1 in the :input map part of the Push
  state. Then, the output is the integer on top of the integer stack in the Push
  state returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors."
  [individual stack fitness-function]
  ;; get the program
  ;; then evaluate each test case 
  ;; get the resulting values
  ;; then calculate the errors 
  (let [program (get individual :program)
        result-states (map #(interpret-push-program
                             program
                             (push-to-stack empty-push-state :input {:in1 %}))
                           test-cases)
        result-values (map #(peek-stack % stack) result-states)
        errors (map fitness result-values test-targets)]
    ;; assign the errors list and total error to the individual
    (assoc (assoc individual :errors errors) :total-error (apply + errors))))

(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Prints a
  report each generation.
  --
  The only argument is a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-program-size (max size of randomly generated programs)"
  [{:keys [population-size
           max-generations
           error-function
           instructions
           max-initial-program-size]}]
  ;; for the 0th generation, initialize the population
  (loop [generation 0
         population (initialize-plush-population
                     population-size
                     instructions
                     max-initial-program-size)]
    ;; translate the genomes from the current pop to programs
    ;; then get the errors for the current population
    (let [prog-pop (map
                    #(assoc %
                            :program
                            (trans/translate-plush-genome-to-push-program
                                       {:genome (get % :genome)}))
                    population)
          curr-pop (map
                    #(game-error-function % :integer)
                    prog-pop)]
      (report curr-pop generation)
      ;; if it has exceed the number of generations, terminate the prog
      (if (> generation max-generations)
        ;; saved for later use in testing
        ;; (get-best-individual curr-pop)
        nil
        ;; if no individual has error 0, continue GP
        (if (empty? (filter #(= 0 (get % :total-error)) curr-pop))
          (recur
           (inc generation)
           ;; do variation to make the new population
           (repeatedly population-size #(plush-select-and-vary curr-pop
                                                         (* 0.05
                                                            population-size))))
          ;; saved for later use in testing
          ;; (get-best-individual curr-pop))))))
          ;; if an individual has error 0, return SUCCESS
          :SUCCESS)))))


;;;;;;;;;;
;; The main function. Uses some problem-specific functions.

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (push-gp {:instructions instructions
            :error-function game-error-function
            :max-generations 10
            :population-size 10
            :max-initial-program-size 10}))

; (defn run-me
;   []
;   (.getResult (SpaceInvaders. (apply list
;                                      '(shot_exists
				       
; 				       true
; 				       bool_or
; 				       ))
;                               100)))
  

; (defn -main
;   [& args]
;   (println (repeatedly 1 run-me)))

  
