module Main

import Data.Vect

main : IO ()
main = ?main_rhs

data DataStore : Type where
  MkData : (size: Nat) -> (items: Vect size String) -> DataStore

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