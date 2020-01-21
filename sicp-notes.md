With David Beazley

# Day 1

- Elements of programming
    - Primitives: `32`, `3.7`
    - Combinations (built-in operators and expressions): `42 + 3.7`
        - Evaluation Rules
            - There are multiple ways to do things.
                - ALL of computation can be described by the evaluation of expressions and the application of functions
                - The devil is in the details. Mostly **when**.
            - There are [three ways a form may be evaluated](https://stackoverflow.com/questions/1171252/whats-the-explanation-for-exercise-1-6-in-sicp/59833295#59833295) in Scheme:
                1. **Applicative Order** (do it first)
                    - *fully expand and then **reduce***
                    - Scheme: arguments are *fully evaluated* before calling a procedure (like Python)
                    - This is how most programming languages work
                    - `f(1+1)`: `3*2*2`
                2. **Normal Order** (do it later)
                    - *evaluate the arguments and then **apply***
                    - `f(1+1)`: `3*(1+1)*(1+1)` (also used in "lazy evaluation")
                3. **Special Forms** For example:
                    - Booleans
                        - `(and <e1> ... <en>)`: Left &rarr; Right. If any <e> evaluates to false, the value of the and expression is false, and the rest of the `<e>`'s are not evaluated.
                        - `(or <e1> ... <en>)`: Left &rarr; Right. If any <e> evaluates to a true value, that value is returned as the value of the or expression, and the rest of the `<e>`'s are not evaluated.
                        - `(not <e>)`
                        - `and` and `or` are special forms
                        - `not` is an ordinary procedure.
                    - Conditionals like `if` and `cond`
                        -  `(if <predicate> <consequent> <alternative>)`: If the `<predicate>` evaluates to a true value, the interpreter then evaluates the `<consequent>` and returns its value. Otherwise it evaluates the `<alternative>` and returns its value
                        - `(cond (<p1> <e1>) ... (<pn> <en>))`: The predicate `<p1>` is evaluated first. If its value is false, then `<pn>` is evaluated. If `<pn>`'s value is also false, then `<pn+1>` is evaluated. When true predicate, the interpreter returns the value of the corresponding consequent expression `<e>`.
                    - Example: `((if (> 1 0) + -) 1 1)` &rArr; `#<procedure:+>`
                        - The order of operation allows for operators (objects in the first position of the list) that are compound expressions (i.e. `(+ (* 2 2) (* 2 1))`)
                        - This is possible because functions are first class functions
                        - Evaluating `+` -> `#<procedure:+>`
                        - If returns either `#<procedure:+>` or `#<procedure:->` not the SYMBOL `+` or `-` because the if evaluates and then returns
                        - Works in applicitive order. Would it work in normal order?
    - Naming (abstraction): `radius = 5`
- Scheme
    - `#;` comments out the entire thing
    - Functions are first class
        - `(define + 4)`: Defines `+` as `4`.
        - `(define op +)`, `(op 2 4)` &rArr; `6`
    - `(define (square x) (* x x))` := `(define square (lambda (x) (* x x)))`
    - Syntax is extremely minimal
        - `42`: a primitive
        - `(x y z)`: a list
    - Special Forms
        - `(* x 20)` & `(define z 20)`
            - Note the syntax is the same
            - `define` behaves differently than `*` even though they look the same
            - `define` is not a procedure like `*`.
        - Special forms don't follow the normal rules of evaluation
        - Python
            - `2 + 1/0` &rArr; blows up
            - `2 or 1/0` &rArr; `2`
            - So `or` and `and` (`0 and 1/0` &rArr; `0`) are essentially special forms in Python.
            - `and` and `or` cannot be customized so they are not used in Pandas, for example. They use different rules of evaluation.
        - `if`: `(if (< 3 4) 1 (/ 1 0))` &rArr; `1` &rarr; does not evaluate `(/ 1 0)` &there4; it is a special form
            - `(define (choose test x y) 1)` `(choose (< 3 4) 1 (/ 1 0))` &rArr; blows up
            - `(define a if)` &rArr; `bad syntax`

## Recursion and Iteration

Quality measured by size and the number of steps

- n is the size
    - Determined by the number of steps it takes to get there
    - Example, if we are looking at the factorial of `n` where `n=5`
        - On a stack (general recursion), 5 calls will be added, so the size is &Theta;(n) (remember, `n=5` and 5 procedures will be added to the stack)
        - Using registers (iterators, tail recursion), each iteration will update in place, so the size is &Theta;(1)
- &Theta;(1): constant
- &Theta;(log n): logarithmic - Doubling the problem size increases the resource requirement by a constant amount.
- &Theta;(n): linear - doubling the size will roughly double the amount of resources used.
- &Theta;(n&sup2;): exponential - each increment in problem size will multiply the resource utilization by a constant factor.
- O(n) represents upper bound. &Theta;(n) means tight bound. &Omega;(n) represents lower bound.


### Linear Recursive Process

* This is a recursive *procedure* (defined by syntax) and it is a recursive *process*.
* General characteristics
    - Requires a *stack*, an auxiliary data structure (does not use registers)
    - **Space is &Theta;(n)**
    - &Theta;(n): Amount if information to keep track of grows *linearly* with the number of recursions
* Linear Recursion: the memory use increases linearly:  `(define (sum-n n) (if (= n 1) 1 (+ n (sum-n (- n 1)))))` &rarr; `(sum-n 2)` &rArr; `3`
* Expansion via substitution reveals a chain of deferred operations. The stack shrinks as the operations are performed.
* **Left Behind**: Leaving a part of the calculation behind to do later.

#### Example

```
(define (fact n)
  (if (= n 1)
  n
  (* n (fact (- n 1)))))

(fact 5)
(* 5 (fact 4))
(* 5 (* 4 (fact 3)))
(* 5 (* 4 (* 3 (fact 2))))
(* 5 (* 4 (* 3 (2 * (fact 1)))))
```


### [Tree Recursion](http://taoofcode.net/on-triangles/) (with Sierpinski Triangles)

* &Theta;(n&sup2;): Number of steps grows exponentially with the input, proportional to the number of nodes on the tree (lots of redundant computation).
* &Theta;(n): Space required grows linearly with the input, proportional with the maximum depth of the tree (only need to keep track of the nodes above in the tree)
* Often easier to specify and understand. Could a "smart compiler" transform tree-recursive procedures into more efficient execution processes?
    * One solutin is *memoization*
    * Can transform exponential number of steps into processes with linear space and time growth
- Tree Recursion
    - Most efficient with hierarchical data structures over linear numerical computations
    - Efficiency
        - Number of steps: &Theta;(n&sup2;) (grows **exponentially** with the input)
            - Number of steps required grows proportionally with the number of nodes.
            - Example: Fibonacci
                - Number of steps: &Theta;(&phi;&sup2;)
                - Where &phi; the golden ratio, ~1.6180
        - Space required: &Theta;(n) (grows **linearly** with the input)
            - We only need to keep track of which nodes are above us at any point in the computation.
            - &there4; space required is proportional to the depth of the tree.
    - Memoization
        - Sometimes tree recursive solutions are simply more clear. Can a compiler simple remove redundant calculations?
        - Avoid redundant calculations by storing them in a table and checking if answer is already stored before each operation.
        - Can be used to transform processes that require an exponential number of steps into processes whose space and time requirements grow linearly with the input.
        - Fibonacci
            - Rule for `Fib(n)` &rArr;
                - if: `(= n 0)` &rarr; `0`
                - if: `(= n 1)` &rarr; `1`
                - else: `Fib(n - 1)+Fib(n - 2)`
            - Lisp idiosyncratic (no state variables), but more inefficient (tree recursion):

#### Tree Recursion: Fibonacci

    (define (fib n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fib (- n 1))
                     (fib (- n 2))))))

### Linear Iterative Process

* This is a recursive *procedure* (defined by syntax) but it is an iterative *process*.
* General Characteristics
    - **Number of steps is &Theta;(n)**
    - Needs three state variables
        - Result (often of a mathematical function)
        - Iterator (often `i`)
        - Max (the value that is tested against the iterator)
        - &there4; **space is &Theta;(1)**
        - Uses Registers (not a stack)
* `(define (sum-i n) (define (iter n result) (if (< n 1) result (iter (- n 1) (+ n result)))) (iter n 0))`
* Program variables provide a complete description of the state of the process at any point
* Syntax for linear iterative processes
    * Ada, Pascal, C use looping constructs (`do`, `repeat`, `for`, `while`)
    * Scheme: even if an iterative process is described using a recursive procedure, it will execute in constant space - *tail-recursion*
* **Carry Forward** Everything is carried forward to the next call.

#### Tail Call Optimization

- Iteration and Tail Recursion: can be realized “in hardware” as a machine that has a fixed set of registers and no auxiliary memory.
- The final resursive call can't be wrapped within another function, e.g. `(+ (iter x) 1)` vs. `(iter x)`
- [Tail Call Optimization for Python](https://github.com/baruchel/tco): it's not native

#### Example (using TCO)

```
(define (fact n)
  (define (fact-iter n result)
    (if (= n 1) result
    (fact-iter (- n 1) (* n result))))
    (fact-iter n 1))

 This is Linear Iteration (result is carried along each step)
(fact-iter 5 1)
(fact-iter 4 5)
(fact-iter 3 20)
(fact-iter 2 60)
(fact-iter 1 120)
...
```

#### Iteratively: Fibonacci

    (define (fib n)
      (fib-iter 1 0 n))

    (define (fib-iter a b count)
      (if (= count 0)
          b
          (fib-iter (+ a b) a (- count 1))))


## Dynamic Scope

```python
def f():
  z = 42
  g()

def g():
  print(z)

def h():
    g()  # Fail (no z)

f() # this would work in dynamic scope
```
