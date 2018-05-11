
import System.Random
import Control.Monad
import SBox

type Key = [Bool]
type Message = [Bool]
type Blocks = [Message]

showBits :: [Bool] -> String
showBits [] = ""
showBits (x:xs) = (if x then "1" else "0") ++ showBits xs


generateKey :: IO Key
generateKey = replicateM 64 randomIO

generateMessage :: IO Message
generateMessage = replicateM 400 randomIO

splitIntoBlocks :: Message -> Blocks
splitIntoBlocks m | null m        = []
                  | length m < 64 = [addZeros m]
                  | otherwise     = take 64 m : splitIntoBlocks (drop 64 m)
  where
    addZeros m' | length m == 64 = m'
               | otherwise       = addZeros (m' ++ [False])

splitBlock :: [Bool] -> ([Bool], [Bool])
splitBlock m = (take 64 m, drop 64 m)

feistelRound :: Message -> Key -> Message
feistelRound m k = rightSide ++ leftSide `xor` fistelFunction rightSide k
  where
    leftSide = fst $ splitBlock m
    rightSide = snd $ splitBlock m

fistelFunction :: Message -> Key -> Message
fistelFunction m k = permutation $ sbox $ expand m `xor` k
  where
    expand :: Message -> Message
    expand m' = m' !! 63 : expand' m' ++ [head m]
    expand' :: Message -> Message
    expand' [] = []
    expand' m' = take 4 m' ++ [m' !! 5] ++ [m' !! 4] ++ expand' (drop 8 m')
    permutation :: Message -> Message
    permutation m = m

xor :: [Bool] -> [Bool] -> Message
xor = zipWith xor'
  where
    xor' :: Bool -> Bool -> Bool
    xor' True True = False
    xor' True False = True
    xor' False True = True
    xor' False False = False

applyXTimes :: (a -> a) -> Int -> a -> a
applyXTimes f 1 x = f x
applyXTimes f i x = applyXTimes f (i-1) (f x)

initialPermutation :: Message -> Message
initialPermutation m = m

generateKeys :: Key -> [Key]
generateKeys k = map (uncurry (++)) (zipWith (applyXTimes leftShift) [1 .. 16] shortKeys)
  where
    shortenKey [] = []
    shortenKey k' = take 7 k' ++ shortenKey (drop 8 k')
    shortKey = shortenKey k
    splitKey = (take 28 shortKey, drop 28 shortKey)
    shortKeys = replicate 16 splitKey
    leftShift :: (Key, Key) -> (Key, Key)
    leftShift (k1, k2) =  (tail k1 ++ [head k1], tail k2 ++ [head k2])

decript :: Message -> Key -> Message
decript m k = concat $ concatMap (\m' -> zipWith fistelFunction (replicate 16 m') keys) blocks
  where
    keys = generateKeys k
    blocks = splitIntoBlocks m

encript :: Message -> Key -> Message
encript m k = concat $ concatMap (\m' -> zipWith fistelFunction (replicate 16 m') keys) blocks
  where
    keys = reverse $ generateKeys k
    blocks = splitIntoBlocks m

main :: IO ()
main = do
  key <- generateKey
  message <- generateMessage
  putStrLn $ showBits message
  putStrLn "------------------------"
  let decriptedMessage = decript key message
  putStrLn $ showBits decriptedMessage
  putStrLn "------------------------"
  let encriptedMessage = encript key decriptedMessage
  putStrLn $ showBits encriptedMessage
