---
title: German Street Dictionary Compression
---
While at ScanSoft, I asked the manager of the embedded speech team what I can
help out with, and he gave me something related to compression. To tackle the
task at hand, I read about Huffman coding and other algorithms such as
arithmetic coding and sliding window method. After doing some research, I
decided to write a length limited Huffman coding program customized to
ScanSoft's pronunciation dictionary file. The method was able to perform better
than the 6-bits method that was used at ScanSoft. The 6-bits method performs
compression by using only 6 bits to represent each character.

[compression code](/files/compression/compress.c) - 227 lines of code

[decompression code](/files/compression/decompress.c) - 309 lines of code
