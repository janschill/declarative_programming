module Combinators where

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
-- compose f g   = \x -> f (g x)
-- compose f     = \g x -> f (g x)
-- compose       = \f g x -> f (g x)

fun1 :: (a -> b -> c) -> (b -> a) -> b -> b -> c
-- fun1 f g = \x y -> f (g x) y
fun1 f g = (flip f g) . id

fun2 :: (a -> b) -> (b -> c) -> a -> c
fun2 = \f g x -> g (f x)

fun3 :: b -> (b -> c) -> c
-- fun3 = flip id
fun3 = \x f -> f x

fun4 :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
-- fun4 f g = ((.) f) . g
fun4 f g = \x y -> (f (g x y))

fun5 :: (a1 -> b) -> (a2 -> b -> c) -> a2 -> a1 -> c
-- fun5 f g = (flip (.) f) . g
fun5 f g = \x y -> g x (f y)