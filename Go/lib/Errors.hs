module Errors where

todo :: Show a1 => a1 -> a2
todo a = error ("Not implemented!" ++ " (" ++ show a ++ ")")

todo' :: a
todo' = error "Not implemented!"

unreachable :: Show a1 => a1 -> a2
unreachable a = error ("Unreachable!" ++ " (" ++ show a ++ ")")

unreachable' :: a
unreachable' = error "Unreachable!"
