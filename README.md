# haskell-hamming
A haskell implementation of arbitrary-length extended (SECDED) Hamming code, with block length `2 ^ r` for message length `2 ^ r - r - 1`.

This is more for illustration than efficiency.

Output of `transmit` is of the form `(s, n)` where `s` is a string matching as closely as possible the input string and `n` is an integer representing the number of uncorrectable second errors detected.  Three "test channels" are provided to ensure the error correction works.  `goodchannel` changes no bits, so the output should be exactly the input with zero errors detected.  `badchannel` flips one bit in each block, so the output should be exactly the same with zero second errors detected since the code is corrected automatically.  `verybadchannel` flips two bits in each block, so the output should be nonsense with multiple second errors detected.
