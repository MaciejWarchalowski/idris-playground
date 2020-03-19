module Main

import Data.Vect

data DataStore : Type where
  MkData : (size: Nat) -> (items: Vect size String) -> DataStore

data Command = Add String
             | Get Integer
             | Quit

size : DataStore -> Nat
size (MkData size items) = size

items: (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) y = MkData _ $ addToData items
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [y]
    addToData (x :: xs) = x :: addToData xs

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just $ Add str
parseCommand "get" ind = (case all isDigit (unpack ind) of
                               False => Nothing
                               True => Just $ Get (cast ind))
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd args

fetchIndex : (idx : Integer) -> (x : DataStore) -> Maybe (String, DataStore)
fetchIndex idx store@(MkData size items) = case integerToFin idx size of
                                            Nothing => Just ("Out of range\n", store)
                                            (Just x) => Just (index x items ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput x y = case parse y of
                        Nothing => Nothing
                        (Just (Add datum)) => Just (datum, addToStore x datum)
                        (Just (Get idx)) => fetchIndex idx x
                        (Just Quit) => ?test_4

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
