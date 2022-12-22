module Errors where

todo :: Show a1 => a1 -> a2
todo a = error ("Not implemented!" ++ " (" ++ show a ++ ")")

todo' :: a
todo' = error "Not implemented!"
