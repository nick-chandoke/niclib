{-
General parsing:
    1) A _context_ is a collection of data satisfying a given predicate. instance Eq context.
    2) A _state_ is a possible configuration of a thing. instance Eq state.
    3) A parser is defined for a set of states, or set of contetxs: a collection of directives that describe, for each state or context, the morphisms from that state to each of potentially many successor states
-}

module NicLib.Parser (parse) where
import Data.List (find) 
-- | Stateful parsing catamorphism. Parse a thing of type b into a thing of type a by starting at an initial state; then moves from state to successive state until there's nothing left to parse, either because of parsing parameter exhaustion or manual short-circuiting.
-- It's expected that you'll create a simple sum type for your states, e.g. data States = State1 | State2 | ... deriving Eq.
-- The first argument (the "parser") is a list of (state, successor function). A successor function takes the parsed value and yet-to-parse value and produces a list of possible future states (a, b, Maybe s) where a is the updated yet-parsed value, b is the yet-to-be-parsed value, and s is the next state or Nothing, to short-circuit the parsing, returning a. In this short-circuit case, b may be bottom/undefined
-- see URL parser in NicLib.URL:str2url for example
parse :: (Foldable t, Eq s) => t (s, a -> b -> [(a, b, Maybe s)]) -> s -> a -> b -> [a]
parse parser s a b = case find ((==s) . fst) parser of -- if we have very many states, we may impose an order on them and binary search through the list of states as a Set of states just for faster lookup. That's a very small quick change.
    Nothing -> error "Unaccounted-for state."
    Just (_, succFunc) -> succFunc a b >>= \(a', b', ms') -> case ms' of Nothing -> return a'; Just s' -> parse parser s' a' b'
