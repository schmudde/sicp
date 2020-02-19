With David Beazley

TODO: Read 232 *Sameness and Change*

> I don't know what your opinion of pair programming is, but if you're in a SICP class, I assume you work alone.

~ Da Beaz (22/January/2020)

Functional programming is programming without assignments. No mutation.

~ Chapter 3 in SICP, where they introduce assignment for the first time.

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
                    - *evaluate the arguments and then **apply***
                    - Scheme: arguments are *fully evaluated* before calling a procedure (like Python)
                    - This is how most programming languages work
                    - `f(x)=x+x`: `3*f(1)*f(1)` &rarr; `3*2*2`
                2. **Normal Order** (do it later)
                    - *fully expand and then **reduce***
                    - `f(x)=x+x`: `3*f(1)*f(1)` &rarr; `3*(1+1)*(1+1)` (also used in "lazy evaluation")
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

# Day 2

- Evaluate: substitute (expand)
- Apply: reduce

Substitution drives all the computation of the first chapter. Evaluate the symbols and substitute.

```python
square=('*','x','x')

def substitute(items, name, value):
  if isinstance(items, tuple):
    return tuple(substitute(item, name, value) for item in items)
  elif items == name:
    return value
  else:
    return items

> substitute(23, 'x', 10) # 32
> substitute('x','x', 15) # 15
> square # ('*', 'x', 'x')
> substitute(square, 'x', 2) # ('*', 2, 2)
```

## Data Abstraction

- *Constructors*: implement the abstract data in terms of the concrete representation, i.e. `make-rational-number`.
- *Selectors*: select parts of the data, i.e. `numerator` and `denominator.`

## Let

```
(let ((var1 exp1)
      (var2 exp2)
       ...
      (varn expn))
  body)
```

It is a syntax translation to the following: `((lambda (var1 var2 ... varn) body) exp1 exp2 ... expn)`

&there4; this works:

```
(define (g a)
  (let ((x (+ a 10))
        (y (- a 2)))
    (+ x y)))
```

&there4; this doesn't work:

```
(define (g a)
  (let ((x (+ a 10))
        (y (- x 2)))
    (+ x y)))
```

Because `x` is not defined before `(- x 10)` is evaluated. All the expresions in the lambda (` exp1 exp2 ... expn`) are evaluated first.

But this does work:
```
(define (h a)
  (let ((x (+ a 10)))
    (let ((y (+ x 2)))
      (+ x y))))
```

And this does work:

```
(define (g a)
  (let* ((x (+ a 10))
         (y (- x 2)))
    (+ x y)))
```

`let*` works by nesting lambdas


## Fixed Point

As an operation converges on a value. Such as a square root converging to 1.

`f = sqrt`
`1 = f(1) = f(f(1)) = f(f(f(1))))`

## Data Structures

What is this? -  `(define a (cons 3 4))`

```
(define (cons a b)
  (lambda (m)
    (cond ((= m 0) a)
          ((= m 1) b))))

(define (car p) (p 0))
(define (cdr p) (p 1))
```

All you need is what you learn in Chapter 1 (see page 91). `cons` can just be defined in anonymous functions.

## Functions as Data

Exercise 2.6

```
(define zero (lambda (f) (lambda (x) x))) ; means don't do the function at all
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
```

`(inc (inc (inc 0)))` = `((three inc) 0)`

How do we add one more `(f x)` for `four`?

`(define (add-1 n)
  (f (... previous number ...)))`

`(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))`

Note: `(f ((n f) x))` is the same format as `((three inc) 0)`

How do we implement plus (`+`)? - `(define (plus a b) ... )` (hint must return `(define (plus a b) (lambda (f) (lambda (x) ... )))`)

What is truth?

```
(define (TRUE x y) x)
(define (FALSE x y) y)

(TRUE 0 1) ; 0
(FALSE 0 1) ; 1

(define (NOT x) (x FALSE TRUE))
(NOT TRUE) ; #<procedure:FALSE>
```

- Suppose that `x` is TRUE, the result of the AND is determined what `y` is. If the `y` is TRUE, then it is TRUE. If the `y` is FALSE, then it is FALSE.
- The behavior of TRUE is to pick the first thing you give it: `(define (TRUE x y) x)`.
- `(define (AND x y) (x y x))`
    - T+T: T: True returns the first thing you give it: `y` (T).
    - T+F: F: True returns the first thing you give it: `y` (F).
    - F+T: F: False returns the second thing you give it: `x` (F).
    - F+F: F: False returns the second thing you give it: `x` (F).
- `(define (OR x y) (x x y))`

## Lambda Calculus

λ-Calculus

There are only three valid expressions:

1. Names: x, y, etc.
2. Function: λ<name>.<expression>
3. Application: <expression><expression>

There is NOTHING else

- No numbers or other primitives
- No math operators (+,-, etc.)
- No data structures (pairs, etc.)

All you need are functions and substitutions.

- (λx.(λy.x))ab &larr; this is the TRUE function above. Explained:
    - `a` substitutes into the `x`: (λx.(λy.x)) &darr;
    - returns the second function (λy.a)b &darr;
    - returns `a` (there is no `y`)
- (λx.(λy.y))ab &larr; this is the FALSE function above
    - returns (λy.y)b &darr;
    - returns `b` (there was no substitution into `a`)

## Lists

```scheme
(list? '(1 2)) ; #t
(list? '(1 . 2)) ; #f
(list '() 1) ; '(() 1)
(cons '() 1) ; '(() . 1)
```

Compute the length

```scheme
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length '(1 2 3)) # 3
```

Pick out the nth item

```scheme
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref '(1 2 3) 2) # 3
```

Shortcuts: `(cadr <arg>) = (car (cdr <arg>))`

# Day 3

## Recursion and Lists

Templates:

    (define (map prodc items)
      (if (null? items)
          null
          (cons (...) (map proc (cdr items)))))

Filter only return itmes where (proc item) is true


    (define (filter proc items)
      (cond ((null? items) null)
            ((proc (car items)) ...) ;; predicate is false (keep the item)
            (else (filter proc (cdr items))))) ;; predicate is true (discard the item)

## Fold and Accumulate

`append` as `accumulate`

    (define (append* seq1 seq2)
      (accumulate cons seq2 seq1))

```
initial: '(4 5 6) sequence: '(1 2 3)
(cons '(4 5 6) (accum cons '(4 5 6) '(1 2 3)))
               (cons 1 (accum cons '(4 5 6) '(2 3)))
                        (cons 2 (accum cons '(4 5 6) '(2 3)))
                                 (cons 3 (accum cons '(4 5 6) '()))
                                          return cons '(4 5 6)
```

`(append* '(1 2 3) '(4 5 6))` &rArr; '(1 2 3 4 5 6)


Why does it work to

(define fold-right accumulate)
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list null (list 1 2 3))
(fold-left list null (list 1 2 3))

(define (reverse2 sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse3 sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

## Quote

Quotation ensures Scheme does not evaluate something. It allows us to manipulate symbolic data, not just numeric data.

Subtlties:

- `(quote a)` is a **special form**.
- The special form maintains the principle that any expression seen by the interpreter can be manipulated as a data object. Thus they are all equvalent:
    - `(car '(a b c))`
    - `(car (quote (a b c)))`
    - `(list 'car (list 'quote '(a b c)))`
- Use `eq?` to test wheter two symbols are the same.

A literal evaluates to the literal symbol or a literal primitive.

```
> 1
1
> '1
'1
> 'a
'a
> (eval '1)
1
```

    (define b 2)
    (list 'a b) ; (a 2)

## Mappings

- In key/value pairs, the keys are often symbolic: `(define record '((foo 2) (bar 3) (spam 4)))`
- `record` &rarr; `'((foo 2) (bar 3) (spam 4))`
- `(assoc 'bar record)` &rarr; `(bar 3)`

### How does assoc work?

(define (assoc* key items)
  (if (null? items)
      false
      (if (eq? (caar items) key)
          (car items)
          (assoc key (cdr items)))))

## Expressions With Abstract Data

    (define (sum? exp)
      (and (pair? exp) (eq? (car exp) '+)))

    (sum? '(+ (* 2 3) 5)) ; #t

    (define (make-sum left right)
      (list '+ left right))

    (make-sum '(* 2 x) 'y) ; '(+ (* 2 x) y)

## Objects and Types

Objects carry state. **The state changes.**

### Objects

- OO programming is based on message passing
    - *Messages* passed to an object: depdend on the the objects infastructure to select and run the code.
        - Used to achieve encapsulation and distribution.
            - Distinguishes the general function from the specific implementation.
            - Rather than executing the process by name, send a message and the object will select and execute the correct code.
        - Message passing is different because it does invoke a process, subroutine, or function by name
    - *Parameters* in a method: are variables in a method declaration (the names)
    - *Arguments* passed to a method/procedure: are the data passed into the method's parameters (the values)
    - You define parameters and you make arguments
- Terminology is a bit unusual, but it's the basis of (`.`) `obj.attr # Send "attr" message to "obj"`
- A class serves as a namespace for functions:
    - *Class*: classes and structs are *user-defined types* (i.e. objects can be declared a specific type).
        - `class`: members and base classes are private by default
        - `struct`: members and base classes are public by default.
    - *Namespace*: declares a scope for its functions, objects, types, etc....
- Dispatching often table-driven (e.g., dicts in Python)

### Types

Type inheritance is very difficult. See page 200, footnote 52.

```python
x=23
y=4.5
type(x) # <class 'int'>
type(y) # 'float'
x + y # 27.5
y + x # 27.5
x # 23
x.__add__(y) # NotImplemented
y.__add__(x) # 27.5
```
Type `int` knows nothing about `float` but `float` knows about `int`.

```python
y.__sub__(x) # -18.5
x.__sub__(y) # NotImplemented But!! it's not commutative
y.__rsub__(x) # 18.5
```

`rsub` is *reverse subtraction*. `int` doesn't know about `float` so the `float` have to have a *reverse subtraction*. It is the more "capable number."

Python uses *Type Linearization* to deal with multiple inheritance.

# Day 4

Racket vs. Scheme

- Pair is essentially anything that has both a `car` and `cdr` (Racket)
    - `(pair? '(1))` &rArr; #t, `(car '(1))` &rArr; `1`
    - `(pair? (quote 1))` &rArr; #f, `(pair? (quote 1))` &rArr; `expected: pair?`
    - `(pair '(1 2 3))` &rArr; #t
        - `(car '(1 2 3))` &rArr; `1`
        - `(car '(1 2 3))` &rArr; `'(2 3)`

```racket
'a ; 'a
(eval 'g) ; undefined
```

```scheme
'a ; a
eval ; #<procedure:meval>
(eval 'g)  ; meval: arity mismatch
```

## Variables

> When we defined the evaluation model in section 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions whould be evaluated (e.g. left to right or right to left). When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result.

~ page 236

`set!` does not work unless you've previously defined the variable somewhere

 You can do this in Python too (although a bit strange)
```python
def make_counter(n):
  def incr():
    nonlocal n # without this, Python would assume that n is a `local` variable
    n += 1
    return n
  return incr

c = make_counter(0)
d = make_counter(10)
c() # 1
c() # 2
d() # 11
```

- If you want to modify a variable within a closure it has to be tagged a `nonlocal`.
- Python assumes that all locally declared variables are within a local scope.
- This would be solved if Python had special syntax for variable declaration (like Ada).
    - `var x = 23;`
    - `x = 5` would look for the nearest `var` declaration in scope.
    - The lack of a `var` is probably the #1 reason why you have to use the `self` within classes.

```python
class Spam:
  # Without `var x;`
  # def yow():
  #   x = 13

  def yow(self):
    self.x = 13
```

## Environments

1. What environment was the object declared in?
2. What enviornment was the object executed within?

## Concurent Execution

Page 300: *Correct Behavior of Concurrent Programs*

A single thread:
```python
import time

def countdown(n):
  while n>0:
  print("T-minus", n)
  time.sleep(10)
  n -=1
```

Run again after starting a thread:
```python
import threading
threading.Thread(target=countdown, args=[5]).start()
```

```python
class Counter:
    def __init__(self):
        self.value=0
    def incr(self)
        self.value += 1
    dec decr(self)
        self.value -= 1

def down(n):
    while n>0:
        c.decr()
        n -= 1

def up(n):
    while n>0:
        c.incr()
        n -=1
```

Obviously:
```python
c.value # 0
up(10_000_000)
c.value # 10000000
down(10_000_000)
c.value # 0
```

With concurrent threads:
```python
t1 = threading.Thread(target=up, args=[10_000_000])
t2 = threading.Thread(target=down, args=[10_000_000])
t1.start(); t2.start()
t1.join(); t2.join() # wait for complete

c.value # some fucked up number because of the race conditions
```

With a mutex (mutual exclusion) lock:
```python
import threading
lock = threading.Lock()

def down(n):
    while n>0:
        lock.acquire()
        c.decr()
        lock.release()
        n -= 1

def up(n):
    while n>0:
        lock.acquire()
        c.incr()
        lock.release()
        n -=1
```

With concurrent threads:
```python
t1 = threading.Thread(target=up, args=[10_000_000])
t2 = threading.Thread(target=down, args=[10_000_000])
t1.start(); t2.start()
t1.join(); t2.join() # wait for complete

c.value # 0
```

## Streams

Delay

- `(define p (delay (+ 10 20)))`
- `(force p)` &rArr; `30`
- As a lambda
    - `(define p (lambda () (+ 10 20)))`
    - `(p)` &rArr; `30`
- `(cons a (delay b))`
    - `define (range start stop) (if (>= start stop) '() (cons start (delay (range (+ 1 start) stop))))`
    - `(range 1 10)` &rArr; `'(1 ... #<promise...)`
    - `(force (cdr a))` &rArr; `'(2 ... #<promise...)**

# Day 5

**Metacircular Evaluation**

Programming languages are designed as a kind of dance involving "eval" and "apply**

- When are expressions evaluated?
- How are expressions evaluated?
- When are procedures applied?

**Lisp**

- There are deep consequences of programs as data
- Can manipulate programs using same tools
- There is a uniformity to it
- Contrast: Having to use a lexer/parser to construct abstract syntax trees, manipulating syntax trees, etc.

## 4.2 Lazy Evaluation

Up to this point, there has been a differentiation between Lists and Streams. With lazy evaluation there turns out to be no difference.

## 4.3 Nondeterministic Evaluator

`(define x (amb 1 2 3))`: the evaluator will pick one value of `x` and try the program. If it fails, it will backtrack and try `2`. If it fails, it will backtrack and try `3`. Eventually it will try enough possibilities and arrive at an answer (if there is an ansewr).

It's a form of programming with continuations:

- Functions don't return results, they return functions
- A function is given a function that is called to produce a result
- To continue the calculation call the success function, keep doing this to chain.

## Register Machines

```scheme
(define (fact n)
  (define (fact-iter n result)
    (if (= n 1) result
        (fact-iter (- n 1) (* n result))))
        (fact-iter n 1))
```

`(- n 1) (* n result)` can't do it in this order in the register machine (subtle difference)

```scheme
(n 'set! 4) ; initalize
(1->result) ; initalize
(n-is-one?) ; #f
(p->result) ; (* n result)
(dec->n)    ; (- n 1)
(n-is-one?) ; #f
(p->result)
(dec->n)
(n-is-one?) ; #f
(p->result)
(dec->n)
(n-is-one?) ; #t
(result 'get) ; 24
```
