data Expr = Val Int | Add Expr Expr deriving (Show)

-- Original eval function
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

-- Step One: Add an Stack

-- eval' :: Expr -> Stack -> Stack
-- eval' e s = eval e : s

-- Base Case
-- eval' (Val n) s
-- = { specification of eval' }
-- eval (Val n) : s
-- = { applying eval }
-- n : s
-- = { define push n s = n : s }
-- push n s

push n s = n : s

{-

Inductive Case
eval' (Add x y) s
= { specification of eval' }
eval (Add x y) : s
= { applying eval }
(eval x + eval y) : s
= { define: add (m : n : s) = n + m : s }
add (eval y : eval x : s)
= { induction hypothesis for x }
add (eval y : eval' x s)
= { induction hypothesis y }
add (eval' y s : eval x' s)

-}

add [] = error "Can't perform add on empty array"
add [x] = error "Can't perform add on array with less than two items"
add (m : n : s) = n + m : s

-- eval' explicitly manipulates its values
-- eval' (Val n) s = push n s
-- eval' (Add x y) s = add (eval' y (eval' x s))

-- New eval function
-- eval :: Expr -> Int
-- eval e = head (eval' e [])

-- Step Two: Adding a continuation
-- This step generalizes the eval' function, in order to make the flow
-- of control explicit.

type Cont = Stack -> Stack

-- We seek to define the following function:
-- eval'' :: Expr -> Cont -> Cont
-- eval'' e c s = c (eval' e s)

-- Induction on e

-- Base Case
-- eval'' (Val n) c s
-- = { specification of eval'' }
-- c (eval' (Val n) s)
-- = { applying eval' }
-- c (push n s)

-- Inductive Case
-- eval'' (Add x y) c s
-- = { specification of eval'' }
-- c (eval' (Add x y) s)
-- = { applying eval' }
-- c (add (eval' y (eval' x s)))
-- = { unapplying . }
-- (c . add) (eval' y (eval' x s))
-- = { induction hypothesis for y }
-- eval'' y (c . add) (eval' x s)
-- = { induction hypothesis for x }
-- eval'' x (eval'' y (c . add)) s

-- eval'' :: Expr -> Cont -> Cont
-- eval'' (Val n) c s = c (push n s)
-- eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

-- By substituting the identity continuation c = id,
-- using the original eval'' equation, we obtain the
-- following definition
-- eval' :: Expr -> Cont
-- eval' e = eval'' e id

-- eval :: Expr -> Int
-- eval e = head (eval' e [])

-- Step Three: Defunctionalising

-- This step involves replacing the Cont function with specific types
-- of continuation. In this case, we have three types:
-- 1.- Halt the process
-- 2.- Push an element into the stack
-- 3.- Add the first two elements of the stack
-- So, the next step is to define three types of combinators for
-- constructing the required continuations.

-- haltC :: Cont
-- haltC = id

-- pushC :: Int -> Cont -> Cont
-- pushC n c = c . push n

-- addC :: Cont -> Cont
-- addC c = c . add

-- Using this combinators, we now rewrite our evaluators as follows

-- eval' :: Expr -> Cont
-- eval' e = eval'' e haltC

-- eval'' :: Expr -> Cont -> Cont
-- eval'' (Val n) c = pushC n c
-- eval'' (Add x y) c = eval'' x (eval'' y (addC c))

-- The next defunctionalising step is to define the Code type

-- data Code = HALT | PUSH Int Code | ADD Code deriving (Show)

-- This constructors of this new type has the same type as the
-- corresponding combinators, just Code instead of Cont.

-- The Code name references the fact that its values represent code
-- for a virtual machine that evaluates expressions using a stack.

-- We now define some functions to map from a Code value to its
-- corresponding combinator

-- exec :: Code -> Cont
-- exec HALT = haltC
-- exec (PUSH n c) = pushC n (exec c)
-- exec (ADD c) = addC (exec c)

-- We then simplify the definition of exec by expanding the
-- definitions of type Cont and its three combinators

-- HALT case
-- exec HALT s
-- = { applying exec }
-- haltC s
-- = { applying haltC }
-- id s
-- = { applying id }
-- s

-- PUSH case
-- exec (PUSH n c) s
-- = { applying exec }
-- pushC n (exec c) s
-- = { applying pushC }
-- (exec c . push n) s
-- = { applying . }
-- exec c (push n s)
-- = { applying push }
-- exec c (n : s)

-- ADD case
-- exec (ADD x) s
-- = { applying exec }
-- addC (exec c) s
-- = { applying addC }
-- (exec c . add) s
-- = { applying . }
-- exec c (add s)
-- = { assume s of the form m : n : s' }
-- exec c (add (m : n : s'))
-- = { applying add }
-- exec c (n + m : s')

-- In conclusion, we get the following definition
-- for exec

-- addErr = "The ADD operation must have at least one element"

-- exec :: Code -> Stack -> Stack
-- exec HALT s = s
-- exec (PUSH n c) s = exec c (n : s)
-- exec (ADD c) (m : n : s) = exec c (n + m : s)
-- exec (ADD _) [_] = error addErr
-- exec (ADD _) [] = error addErr

-- exec is a virtual machine for executing code

-- Finally, defunctionalisation itself now proceeds by replacing
-- ocurrences of the every combination in the evaluation functions
-- eval' and eval'' with their CODE counterparts, which results in the
-- following definitions:

-- comp :: Expr -> Code
-- comp e = comp' e HALT

-- comp' :: Expr -> Code -> Code
-- comp' (Val n) c = PUSH n c
-- comp' (Add x y) c = comp' x (comp' y (ADD c))

{-

comp is a function that takes an expression and
compiles it into code.

Here, all the compilation machinery (compiler, target language and virtual machine)
has been systematically derived from the semantics for the source language
using equational reasoning.

The correctness of the new comp functions is proved as follows:

exec (comp e) s = eval’ e s
exec (comp’ e c) s = eval’’ e (exec c) s

Expanding the right hand side using the original specifications of
eval' and eval'' we get the following compiler corrects equations:

exec (comp e) s = eval e : s
exec (comp’ e c) s = exec c (eval e : s)

Combining the steps

The three steps to transform an arithmetic expression evaluation function
to a compiler are the following:

1.- Calculate a generalised evaluation function that uses a stack
2.- Calculate an even generalized function that uses a continuation
3.- Defunctionalize to produce a compiler and a virtual machine

The process of calculating the compiler involves:

1.- Define type Expr, which represents the syntax of the language
2.- Define function eval :: Expr -> Int that provider semantic for the language
3.- Define type Stack that represents a stack of integers

Then four additional components were derived:

1.- Type Code that represents code for the Virtual Machine
2.- comp :: Expr -> Code function that compiles a expression into code
3.- comp':: Expr -> Code -> Code function with a Code argument
4.- A function exec :: Code -> Stack -> Stack that executes code

The relationships between semantics, compilers and virtual machine were
captured by the following correctness equations:

exec (comp e) s = eval e : s
exec (comp' e c) s = exec c (eval e : s)

The key for combining the two transformation steps is to use
the previous equations as specification for the four missing
pieces.

Transform exec (comp' e c) s into exec c' s
comp' e c = c'

Base Case

exec (comp' (Val n) c) s
= { specification of comp' }
exec c (eval (Val n) : s)
= { applying eval }
exec c (n : s)

exec c' s = exec c (n : s)
exec (PUSH n c) s = exec c (n : s)

Specification is satisfied in the base case by defining:
=> comp' (Val n) c = PUSH n c

Inductive Case

exec (comp' (Add x y) c) s
= { specification of comp' }
exec c (eval (Add x y) : s)
= { applying eval }
exec c (eval x + eval y : s)

Use induction hypothesis for the two argument expressions x and y

exec (comp’ x c’) s’ = exec c’ (eval x : s’)
exec (comp’ y c’) s’ = exec c’ (eval y : s’)

Transform term into:

exec c’ (eval y : eval x : s)

Solve equation:

exec c’ (eval y : eval x : s) = exec c (eval x + eval y : s)

Generalize

exec c’ (m : n : s) = exec c (n + m : s)

Define ADD :: Code -> Code

exec (ADD c) (m : n : s) = exec c (n + m : s)

exec c (eval x + eval y : s)
= { unapplying exec }
exec (ADD c) (eval y : eval x : s)
= { induction hypothesis for y }
exec (comp' y (ADD c)) (eval x : s)
= { induction hypothesis for x }
exec (comp' x (comp' y (ADD c))) s

Specification is satisfied in the inductive case by defining:
=> comp' (Add x y) c = comp' x (comp' y (ADD c))

Focus on: exec (comp e) s = eval e : s

exec c s
comp e = c

HALT :: Code

exec (comp e) s
= { applying exec }
eval e : s
= { define HALT s = s }
exec HALT (eval e : s)
= { specification of comp' }
exec (comp' e HALT) s

In conclusion:

-}

data Code = HALT | PUSH Int Code | ADD Code deriving (Show)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

addErr = "The ADD operation must have at least one element"

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)
exec (ADD _) [_] = error addErr
exec (ADD _) [] = error addErr
