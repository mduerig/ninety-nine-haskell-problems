import Data.List
import Control.Monad (liftM )
import Test.QuickCheck
import Problems1

main :: IO ()
main = do
    quickCheck lastElemProp
    quickCheck lastButOneProp
    quickCheck elementAtProp
    quickCheck elementCountProp
    quickCheck reverseListProp
    quickCheck isPalindromeProp1
    quickCheck isPalindromeProp2
    quickCheck flattenListProp
    quickCheck dedupListProp
    quickCheck packProp
    quickCheck rlEncProp

lastElemProp :: [Int] -> Bool
lastElemProp [] = True
lastElemProp xs = last xs == lastElem xs

lastButOneProp :: [Int] -> Bool
lastButOneProp []  = True
lastButOneProp [_] = True
lastButOneProp xs = (head $ tail $ reverse $ xs) == lastButOne xs

elementAtProp :: Int -> [Int] -> Property
elementAtProp k xs =
    k > 0 && k <= length xs ==>
    xs!!(k - 1) == elementAt k xs

elementCountProp :: [Int] -> Bool
elementCountProp xs = length xs == elementCount xs

reverseListProp :: [Int] -> Bool
reverseListProp xs = reverse xs == reverseList xs

isPalindromeProp1 :: [Int] -> Bool
isPalindromeProp1 xs = (reverse xs == xs) == isPalindrome xs

isPalindromeProp2 :: [Int] -> Bool
isPalindromeProp2 xs = isPalindrome (xs ++ reverse xs)

instance Arbitrary a => Arbitrary (NestedList a) where
    arbitrary = sized nestedList
        where
            nestedList 0         = liftM Elem arbitrary
            nestedList n | n > 0 = liftM List (vectorOf n (nestedList (n `div` 3)))

flattenListProp :: NestedList Int -> Bool
flattenListProp xs = (length $ flattenList xs) == flatLen xs
    where
        flatLen (Elem _ ) = 1
        flatLen (List xs) = sum $ map flatLen xs

dedupListProp :: [Int] -> Bool
dedupListProp xs = all (== 1) (map length (group $ dedupList $ xs))

packProp :: [Int] -> Bool
packProp xs = group xs == pack xs

rlEncProp :: [Int] -> Bool
rlEncProp xs = (length $ rlEnc xs) == (length $ group xs)