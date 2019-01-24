## Array Calculator & Parser in Haskell
This is a parser which can parse expressions like `[1,2,3] + [1,1,1]` to return the corresponding expression trees and the answer `[2,3,4]`. Built using expression trees on Haskell, this was made in an effort to use functional programming for a practical purpose.

#### How to run
- Make sure you have GHCI installed.
- Run `ghci array_calculator_parser.hs` in the folder containing both Parser.hs and this file.
- Use the commands like `parse pExpr "[1,2,3]+[1,1,1]"` to run the parser.
- Further you can run the command `applyOp parse pExpr "[1,2,3]+[1,1,1]" `to actually compute the answer, which in this case would be `[2,3,4]`.

#### Sample Input/Output
    *Main> parse pExpr "[1,2,3]+[1,1,1]"
    [(Bin Add (Val [1,2,3]) (Val [1,1,1]),"")]
    *Main> parse pExpr "[1,2,3]-[1,1,1]"
    [(Bin Sub (Val [1,2,3]) (Val [1,1,1]),"")]
    *Main> parse pExpr "[] / []"
    [(Bin Div (Val []) (Val []),"")]
    *Main> parse pExpr "a + b"
    [(Bin Add (Var "a") (Var "b"),"")]
    *Main> parse pExpr "a + [1]"
    [(Bin Add (Var "a") (Val [1]),"")]
    *Main> parse pExpr "a + [1,2]"
    [(Bin Add (Var "a") (Val [1,2]),"")]
    *Main> parse pExpr "a1 + [1,2]"
    [(Bin Add (Var "a1") (Val [1,2]),"")]
    *Main> parse pExpr "[1] + [2] / [3]"
    [(Bin Add (Val [1]) (Bin Div (Val [2]) (Val [3])),"")]
    *Main> parse pExpr "[1] - [2] / [3]"
    [(Bin Sub (Val [1]) (Bin Div (Val [2]) (Val [3])),"")]
    *Main> parse pExpr "[1] - [2] - [3]"
    [(Bin Sub (Bin Sub (Val [1]) (Val [2])) (Val [3]),"")]
    *Main> parse pExpr "[1] + [2] - [3]"
    [(Bin Sub (Bin Add (Val [1]) (Val [2])) (Val [3]),"")]
    *Main> parse pExpr "[1] - [2] + [3]"
    [(Bin Add (Bin Sub (Val [1]) (Val [2])) (Val [3]),"")]
    *Main> parse pExpr " [10] - [3] - [2] - [1]"
    [(Bin Sub (Bin Sub (Bin Sub (Val [10]) (Val [3])) (Val [2])) (Val [1]),"")]
