module Sanitizer (Sanitizer) where

stripComments :: String -> String
stripComments [] = []
stripComments ('/':'/':xs) = dropTillEnd xs
    where
        dropTillEnd [] = []
        dropTillEnd ('\n':xs) = stripComments xs
        dropTillEnd (_:xs) = dropTillEnd xs -- ignore head till '\n'

stripComments ('/':'*':xs) = dropTillEnd xs
    where
        dropTillEnd [] = []
        dropTillEnd ('*':'/':xs) = stripComments xs
        dropTillEnd (_:xs) = dropTillEnd xs  -- ignore head till "*/"

-- Note: string can contain comment looking tokens
stripComments ('\"':xs) = '\"' : noop xs
    where
        noop [] = []
        noop ('\"':xs) = '\"':stripComments xs
        noop (x:xs) = x:noop xs

stripComments (x:xs) = x:stripComments xs
