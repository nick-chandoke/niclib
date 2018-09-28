-- 3 stages of data that is yet-to-be-parsed:
--   1. raw data without schema (we need to determine its schema)
--   2. structured data that has a structure different from what we want (isomorphic) (e.g. scraped website; we preserve some structure of the scraped site, though we'll filter & map it into a different structure)
--   3. already ready-to-process (it's already in our target format)
-- much information we'll process will already be structured when we parse it; in this case, 

{- GENERAL NOTE: herein, "state" does not refer to the configuration of the thing being parsed; "state" refers to a manually specified state. This allows for easy writing of automata. Consider:
   data State = Begin | Middle | End deriving Eq
   type B = String -- as is very common, we're going to parse a String into something. We use A and B here correspondent to a and b type variables in the parse function's type signature
   type A = [Int] -- we're going to parse a comma-delimited string of alphanumeric characters into a list of numbers
   parser = [ (Begin, \numList str -> if null str then [(numList, str, Just End)] else let (num, nonNum) = span isDigit str in [(read num:numList, nonNum, Just Middle)])
            , (Middle, \numList str -> if null str then [(numList, undefined, Nothing)] else [(numList, take (min (length str - 2) 4) str, Just Middle)]
            , (End, \numList _ -> [(numList ++ [1..10], undefined, Nothing)]) -- the last step (marked by being a singleton with Nothing in the third tuple position) may have undefined in the second tuple position, as that value will never be considered
            ]
   parse parser Begin [] "" --> [[1,2,3,4,5,6,7,8,9,10]]
   parse parser Begin [] "12098abuo0283te02" --> [[12098]]
   parse parser Begin [] "abo283te2" --> *** Exception: Prelude.read: no parse
   Notice that the parser may fail at some steps; if it fails, then the empty list is returned.
   You can see that parsers are basically easy-to-read recursion or mutual recursion, much like pushdown automata.

General parsing:
    1) A _context_ is a collection of data satisfying a given predicate. instance Eq context.
    2) A _state_ is a possible configuration of a thing. instance Eq state.
    3) A parser is defined for a set of states, or set of contetxs: a collection of directives that describe, for each state or context, the morphisms from that state to each of potentially many successor states
-}

{- Example parser (assumes that URL has only scheme, domain, and path):
    p = [ (SchemeState, \url text -> case takeFirst ["://", ":"] text of Nothing -> [(url, text, Just PathState)]; Just (scheme', rest) -> [(url {scheme = scheme'}, rest, Just DomainState)])
        , (DomainState, \url text -> T'.break (=='/') text & \(domain', rest) -> [(url {domain = domain'}, rest, Just PathState)])
        , (PathState,   \url text -> [(url {path = text}, undefined, Nothing)])
        ]

    -- Run the parser on a given "starting" URL (basically an "empty" URL) and a String to parse into the URL:
    parse p SchemeState (URL "" "" "") "http://domain.com/path/subpath" --> URL "http" "domain.com" "path/subpath"
-}

module NicLib.Parser (parse) where
import Data.Foldable (find) 
import qualified Data.ListLike as LL
-- | Stateful parsing catamorphism. Parse an input into some outputs, starting at an initial state; then moves from state to successive state until there's nothing left to parse, either by input exhaustion or manual short-circuiting (done by returning LL.singleton (thing, undefined, Nothing) for some thing)
-- It's expected that you'll create a simple sum type for your states, e.g. data States = State1 | State2 | ... deriving Eq. (probably usually deriving Enum and/or Bounded, though this doesn't affect the parse function)
-- The first argument (the "parser") is a collection of (state, successor function). A successor function takes the currently-parsed value and yet-to-parse value and produces a list of possible future states (currentParsedValue, inputToPassToNextState, Maybe nextState). I suppose that in most parsers, all successor functions will return LL.singleton's.
parse :: (Eq s, Foldable t, LL.ListLike full (output, input, Maybe s), LL.ListLike full' output)
      => t (s, output -> input -> full) -- ^ set of states and their associated functions that "step forward"
      -> s -- ^ initial state
      -> output -- ^ initial output value
      -> input -- ^ input
      -> full' -- ^ some ListLike full of outputs
parse parser s a b = case find ((==s) . fst) parser of -- if we have very many states, we may impose an order on them and binary search through the list of states as a Set of states just for faster lookup. That's a very small quick change.
    Nothing -> error "Unaccounted-for state."
    Just (_, succFunc) -> flip LL.concatMap (succFunc a b) $ \(a', b', ms') -> case ms' of Nothing -> LL.singleton a'; Just s' -> parse parser s' a' b'
