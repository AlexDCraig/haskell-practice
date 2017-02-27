module Syntax where
import Prelude hiding(Num) 
import Data.List

-- Abstract syntax
-- num ::= (any natural number)
-- var ::= (any variable name)
-- macro ::= (any macro name)
--
-- prog ::= epsilon | cmd ; prog
-- mode ::= down | up
-- expr ::= var | num | expr + expr
-- cmd ::= pen mode | move (expr, expr) | define macro(var*){prog} | call macro(expr*)

-- **Problem 1: Abstract syntax defined as Haskell data types
type Num = Int -- these aren't new data types, they can be defined in terms of other data types
type Var = String 
type Macro = String

-- a program is just a list of commands
type Prog = [Cmd]

-- pen is either up or down, one of two states
data Mode = Down | Up 
 deriving (Eq, Show)

 -- Expr takes a reference to a variable, a number, 
 -- or it gets two expr and adds them together
data Expr = Var Var 
            | Num Num
            | Add Expr Expr
 deriving (Eq, Show)

-- Pen Mode- holds the current Mode of the pen
-- Move Expr Expr- takes two expr which serve as coordinates, moves the pen to those coordinates
-- Define Macro [Var] Prog- pass Define a Macro, a list of variables, and a program
data Cmd = Pen Mode 
           | Move Expr Expr
           | Define Macro [Var] Prog 
           | Call Macro [Expr]
 deriving (Eq, Show)
 
-- **Problem 2: Define a macro line (x1,y1,x2,y2) that 
-- (starting from anywhere on the canvas) draws a line segment 
-- from (x1,y1) to (x2,y2).

-- Macro written in concrete syntax:
-- define line (x1, x2, y1, y2) {pen up, move (x1, y1); pen down, move (x2, y2) }
-- you could also add a "; epsilon" to the end of that statement

-- Haskell encoding:
-- Explanation: line is of type command, it uses the Define constructor
-- to name the macro (of type string) "line", passes it a 
-- list of string variables (x1, y1, x2, y2), then passes it a
-- sequence of commands to complete the task (a Prog is merely a list of
-- commands)

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] -- define the macro name and assign it a list of variables
 -- Pass it a list of commands
 -- Pseudocode: 1) Pick the pen up 2) move the pen to x1, y1
 -- 3) put pen down 4) draw line to x2, y2 5) pick pen up
 [Pen Up, Move (Var "x1") (Var "y1"), Pen Down, Move (Var "x1") (Var "y2"), Pen Up]
 
-- **Problem 3: 
--Use the line macro you just defined to define a new
--macro nix (x,y,w,h) that draws a big “X” of width w and height h, 
--starting from position (x,y). Your definition should not contain any 
--move commands.

-- Written in concrete syntax:
-- define nix (x, y, w, h) { call line (x,y, x+w, y+h); call line (x, y+h, x+w, y) }

-- Haskell encoding
nix :: Cmd 
nix = Define "nix" -- this macro is called nix
   ["x", "y", "w", "h"] -- pass the Define constructor the original coordinates and how wide and how tall the X must be
   [Call "line" -- call the established macro, pass it the coordinates, draw the first line
   [Var "x", Var "y", Add (Var "x") (Var "w"), Add (Var "y") (Var "h")], 
   Call "line" -- call the macro again and draw the other line, completing the X
   [Var "x", Add (Var "y") (Var "h"), Add (Var "x") (Var "w"), Var "y"]]

-- **Problem 4: Define a Haskell function steps :: Int -> Prog 
-- that constructs a program that 
-- draws a staircase of n steps starting from (0,0). 

-- Recall prog is just [cmd]
-- Case 0: []
-- Case 1: [Pen Down, Move (Num 0 Num 1), Move (Num 1 Num 1)
-- Case 2: [Pen Down, Move (Num 0 Num 1), Move (Num 1 Num 1), Move (Num 1 Num 2), Move (Num 2 Num 2)]
-- Explanation: Base case- No steps to take, return an empty program 
-- Recursive case- Use cons operator ':' to construct a list of commands that begins with 
-- positioning the pen at (0, 0) and calls a helper function. The helper function
-- receives (0, 0) as its starting coordinates and provides a list of commands
-- that corresponds to the number of (y+1) and (x+1) movements necessary
-- to reach n steps

steps :: Int -> Prog
steps 0 = []
steps n = Pen Up : Move (Num 0) (Num 0) : Pen Down : stepsHelper 0 0 n

-- Helper function stepsHelper
-- given (x,y) coordinate and # of steps to draw, generate a program that draws from (x,y) to n # of steps, moving up once (i.e. "y + 1") and over once (i.e. "x + 1") to constitute 1 step
-- base case- n steps requires 0 commands, thus an empty program
-- recursive case- combine the list that results from moving up one, then over one with the list that results from
-- calling stepsHelper again to inch closer to (n-1), which marks the final step needed
stepsHelper :: Int -> Int -> Int -> Prog
stepsHelper _ _ 0 = [] -- doesn't matter what x or y is passed into here, 0 steps is 0 steps
stepsHelper x y n = [Move (Num x) (Num (y+1)), Move (Num (x+1)) (Num (y+1))]
     ++ stepsHelper (x+1) (y+1) (n-1)

-- **Problem 5: Define a Haskell function macros :: Prog -> [Macro]
-- that returns a list of the names of all of the macros that are 
-- defined anywhere in a given program
-- Explanation: base case- empty program yields list of 0 Macros (recall Macros are Strings)
-- recursive case- for each x in a list of s x's, if x corresponds to a "Define name" constructor
-- of the macros data type, map the name of the macro to a list, combine this list with the
-- list of all macros in the program
macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = case x of
    (Define name _ _) -> [name]
    _ -> []
  ++ macros xs
  
-- **Problem 6: Define a Haskell function pretty :: Prog -> String
-- that pretty-prints a program. That is, it transforms the 
-- abstract syntax (a Haskell value) into nicely formatted concrete 
-- syntax (a string of characters). 

-- Given set of commands corresponding to abstract syntax, translate commands into concrete syntax of miniLogo
pretty :: Prog -> String
pretty p = intercalate "\n" (map pCmd p) -- insert newline in between lists 
-- map program given to pCmd, which uses pMode and pExpr to parse the program
-- and convert to pretty print format

-- helper function #1
-- parse the commands using two helper functions
pCmd :: Cmd -> String
pCmd (Pen p) = "pen " ++ pMode p ++ ";"
pCmd (Move x y) = "move (" ++ pExpr x ++ "," ++ pExpr y ++ ");"
pCmd (Define name vars prog) = "define " ++ name ++ " (" ++ intercalate "," vars ++ ") {\n"
  ++ intercalate "\n" (map pCmd prog) ++ "\n}\n"
pCmd (Call name exprs) = "call " ++ name ++ " (" ++ intercalate "," (map pExpr exprs) ++ ");"

-- helper function #2
-- given a Mode data type, we convert into the corresponding concrete syntax
-- two states, the string Up or Down
pMode :: Mode -> String
pMode Up = "up"
pMode Down = "down"

-- helper function #3
-- given an expr data type, we convert into the corresponding concrete syntax
-- Expr can be a Var (which is a string), a Num (which is an Int that we convert
-- into a string using show), or an addition function that takes two expr
pExpr :: Expr -> String
pExpr (Var v) = v
pExpr (Num n) = show n
pExpr (Add e1 e2) = pExpr e1 ++ "+" ++ pExpr e2

