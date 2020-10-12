import Data.Char
import Data.List

{-- DATA TYPES --}

-- as expected, 0 or 1
type Bit = Int

-- representing an arbitrary-length bitstring
type Bitstring = [Bit]

{-- CONVERSION UTILITIES --}

-- converts integers into bitstrings of their binary representations (highest-order on the right)
int2bin :: Int -> Bitstring
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- converts bitstrings into integers of their decimal representations (assuming highest-order on the right)
bin2int :: Bitstring -> Int
bin2int bs = sum [(b * 2 ^ i) | (b, i)  <- zip bs [0..]]

-- adds arbitrary zeros to a bitstring to make it length n
make :: Int -> Bitstring -> Bitstring
make n bs = take n (bs ++ (replicate n 0))

-- uses int2bin to convert strings into bitstrings, with every 8 bits representing a character
string2bin :: String -> Bitstring
string2bin = concat . map (make 8 . int2bin . ord)

-- splits a bitstring into segments of arbitrary length
split :: Int -> Bitstring -> [Bitstring]
split _ [] = []
split n bs = take n (bs ++ (replicate n 0)) : split n (drop n bs)

-- uses bin2int to convert bitstrings into strings, with every 8 bits representing a character
bin2string :: Bitstring -> String
bin2string = map (chr . bin2int) . (split 8)

{-- HAMMING ENCODING --}

-- adds the extra "parity bits", excluding the initial "extension bit"
-- for a Hamming encoding of size n, bs should have length (n - (log2 n) - 1) and ps length (log2 n)
-- NOTE: this function requires strict lengths for its arguments and probably should not be used on its own
addpar :: Int -> Bitstring -> Bitstring -> Bitstring
addpar n [] bs = []
addpar n ps bs = ((head ps) : take (n - 1) bs) ++ (addpar (n * 2) (tail ps) (drop (n - 1) bs))

-- gives a list of the indices (in binary) of the on bits in a message bitstring
-- each bitstring should have length r where r^2 is the length of bs, that is n = r
-- NOTE: this function requires strict lengths for its argument and probably should not be used on its own
onpositions :: Int -> Bitstring -> [Bitstring]
onpositions r bs = [p | (p, b) <- zip (map (reverse . make r . int2bin) [0..]) bs, b == 1]

-- determines the proper values of the (nonextension) parity bits in a bitstring
detpar :: Int -> Bitstring -> Bitstring
detpar r bs = reverse ( map ((\x -> mod x 2) . sum) (transpose (onpositions r  bs)))

-- adds the extension parity bit
extend :: Bitstring -> Bitstring
extend bs = (length (filter (==1) bs) `mod` 2) : bs

-- encodes a bitstring in extended Hamming code with message blocks of size (2^r - r - 1)
hamize :: Int -> Bitstring -> [Bitstring]
hamize r bs = [extend (addpar 1 (detpar r (fix bs')) bs') | bs' <- (split m bs)]
              where m = (2 ^ r - r - 1)
                    fix bs'' = (0 : (addpar 1 (replicate r 0) bs''))

-- encodes a character string in extended Hamming code
hamstring :: Int -> String -> [Bitstring]
hamstring r = hamize r . string2bin

{-- HAMMING CHECKING AND CORRECTION --}

-- flips a bit
flips :: Bit -> Bit
flips b = (b + 1) `mod` 2

-- corrects an error at the specified position
correct :: Bitstring -> Bitstring -> Bitstring
correct p bs = [if n == ip then flips b else b | (n, b) <- zip [0..] bs]
               where ip = bin2int p

-- corrects the first bit and checks whether a second error is present
checksec :: Int ->  Bitstring -> (Bitstring, Bool)
checksec r bs | all (==0) csum && parity == 0 = (bs, False)
              | parity == 0                   = (correct csum bs, True)
              | otherwise                     = (correct csum bs, False)
              where
                  csum = detpar r bs
                  parity = length (filter (==1) bs) `mod` 2

{-- HAMMING DECODING --}

-- removes the parity bits from a hamized bitstring with the parity bit already removed
rempar :: Int -> Int -> Bitstring -> Bitstring
rempar n 1 bs = []
rempar n r bs = (take (n - 1) (tail bs)) ++ (rempar (n * 2) (r `div` 2) (drop (n - 1) (tail bs)))

-- decodes a single message block in extended Hamming code
dehamize :: Int -> Bitstring -> Bitstring
dehamize r = rempar 1 (2 ^ r) . tail

{-- TESTING AND DEMONSTRATION --}

-- shaves trailing zeros from strings that may have been lengthened for block size consistency
remnul :: String -> String
remnul [] = []
remnul ('\NUL' : xs) = remtrail xs
remnul xs = xs
remtrail :: String -> String
remtrail = reverse . remnul . reverse

-- checks, corrects, and decodes a hamized string; returns both the resulting string and the number of second errors
checkdec :: Int -> [Bitstring] -> (String, Int)
checkdec r bss = (remtrail (bin2string (concat (map (dehamize r . fst) checks))), length (filter id (map snd checks)))
                 where checks = map (checksec r) bss

-- transmits a message across a "channel" with blocks of size 2 ^ r
transmit :: ([Bitstring] -> [Bitstring]) -> Int -> String -> (String, Int)
transmit c r = checkdec r . c . hamstring r

-- one bit is changed in each bitstring
flipone :: Bitstring -> Bitstring
flipone bs = (take n bs) ++ [flips (bs !! n)] ++ (drop (n + 1) bs)
                where n = (length bs) `div` 2

-- two bits are changed in the bitstring
fliptwo :: Bitstring -> Bitstring
fliptwo bs = (take (n - 1) bs) ++ [flips (bs !! (n - 1)), flips (bs !! n)] ++ (drop (n + 1) bs)
                    where n = (length bs) `div` 2

-- a "channel" that flips no bits
goodchannel :: [Bitstring] -> [Bitstring]
goodchannel = id

-- a "channel" that flips one bit in each bitstring
badchannel :: [Bitstring] -> [Bitstring]
badchannel = map flipone

-- a "channel" that flips two bits in each bitstring
verybadchannel :: [Bitstring] -> [Bitstring]
verybadchannel = map fliptwo