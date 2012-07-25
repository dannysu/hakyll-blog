---
title: Recording Feature for spwave
---
During my 2A co-op work term at ScanSoft, Inc, I added recording feature to a
great software called [spwave][1].

I got the motivation after hearing the QA manager at ScanSoft say that his
unregistered CoolEdit doesn't allow him to record voice and save it as wav
format.

At the time I was using spwave to play files saved in ulaw format, so I was
aware of spwave. It is open source and has wide range of support for different
file formats.

I was also sure that spwave can support read/write to wav file format since it
utilizes libsndfile.

libsndfile library is what I used when writing some part of OCVolume in C. I
successfully used libsndfile to read from wav files and perform various
pre-processing techniques on the input signal.

I believed that spwave can be modified to suit that QA manager's needs, so I
took some time to add the recording feature. spwave's author already has
recording functionality written for one of his other programs, so what I had to
do was integrate the recording functions into spwave.

My modification can be downloaded below in both binary and source:

[spwave-0.6.8 binary](/files/spwave-0.6.9.zip)

[spwave-0.6.8 source](/files/spwave-0.6.9.src.zip)

  [1]: http://www.sp.m.is.nagoya-u.ac.jp/people/banno/spLibs/spwave/
