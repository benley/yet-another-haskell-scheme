module Yahs.Util where

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip str = case str of
               [] -> []
               (x:xs) -> if elem x " \t" then lstrip xs else str

-- This double-reverse isn't great but stripping whitespace here is a hack
-- to begin with
rstrip :: String -> String
rstrip = reverse . lstrip . reverse
