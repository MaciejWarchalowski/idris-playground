import Data.Vect

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not $ isEven k

allLengthsVector : Vect len String -> Vect len Nat
allLengthsVector [] = []
allLengthsVector (x :: xs) = length x :: allLengthsVector xs

insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                    insert x xsSorted

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = S $ myLength xs

myReverseNaive : List a -> List a
myReverseNaive [] = []
myReverseNaive (x :: xs) = myReverseNaive xs ++ [x]

myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: ys) (y :: xs) = (x :: y) :: transposeHelper ys xs

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                            transposeHelper x xsTrans

transposeMat_withZip : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat_withZip [] = createEmpties
transposeMat_withZip (x :: xs) = let xsTrans = transposeMat xs in
                                     zipWith (\x, y => x :: y) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (\x, y => x + y) x y :: addMatrix xs ys
