module Compiler where

import Data.List

data Exp = StrConst String 
         | IntConst Integer
         | Prim String
         | Atom String
         | Var String  -- Symbols
         | Lambda [Exp] Exp -- what is the the second exp?  begin or sequence?
         | Let [(Exp,Exp)] Exp
         | LetRec [(Exp, Exp)] Exp -- (letrec ((<var> (lambda (<var>...) <exp>))) <exp>)
         | Begin [Exp]
         | Seq [Exp] -- List
         | Spawn Exp
         | Send Exp Exp
         | Tuple [Exp]
         | List [Exp]
         | App [Exp]
         | Receive [(Exp,Exp)]
           deriving Show

compile_seq xs = concat (intersperse "," (map erlc xs))

erlc (Atom s) = s
erlc (Var s) = s
erlc (StrConst s) = concat ["\"", s, "\""]
erlc (IntConst n) = show n
erlc (Spawn exp) = concat ["spawn(fun() -> ", 
                           erlc exp,
                           " end )"]

erlc (Tuple xs) = "{" ++ compile_seq xs ++ "}"
erlc (List xs) = "[" ++ compile_seq xs ++ "]"
erlc (App (x:xs)) = erlc x ++ "(" ++ compile_seq xs ++ ")"


erlc (Send receiver sender) = concat [ erlc receiver, " ! ", erlc sender ]

-- eventually support gaurds for patterns here
erlc (Receive cs) = concat [ "receive ",
                             concat (intersperse "; "
                                     (map (\(p, e) -> concat[ erlc p, " -> ", erlc e ])
                                                      cs)),
                           " end"]

                                             


-- spawn(fun() -> psearch(X,N, Item) end)
-- (spawn (psearch X N Item)) 

-- Pid ! {message, Foo}
-- > (! Pid {message Foo})

-- receive block end
-- >
-- (rec (message1 X)
--      (message2 Y))



