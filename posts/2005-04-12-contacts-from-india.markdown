---
date: 2005-04-12 01:01:02
title: Contacts from India
tags: OCVolume
---

The OCVolume category contains posts with emails I received regarding [OCVolume](http://ocvolume.sf.net/). These emails were received while I was in 2nd year of university.


> Hi Danny,

it was nice to know that you are still in school. actually we people are too in the schools.we are a group of three people doing B.Tech at Indian School of Mines, Dhanbad INDIA. we used to call our group XPEG(Xperienced Programmers' Expert Group).
we are basically working for software awareness and promotion in india. we have a feeling that programming is a brach of science as others and it must be treated equally. we are working to make people realize that programming is an art. and one who really lives it will defenitily enjoy it.
we are working to promote competition among young programmers.
i will send you the detail information about our group and our past activities in next mail. presently, we are heading towards All India National Programming Contest. and need some sponsorships. can you help us to arrange sponsorship for us.
i would like to know about u in details if u dont mind.waiting in anticipation,


Here's another one from the same person:


> sir,
i have tried your speech recognition engine. it is working
fine. as far as i have studied it... "training" program
trains the system for Vector Quantization method..and
VoiceType also does the same thing.

i want to train the system using HMM..and at the same
time ..like to test it using the same method.

chaging the constructor in voicetype for ocvolume.java
willl do it? but what abt the codebook? what do i need to
do?
please help me.


Some more support questions:


> Hi Danny,
It’s a long time since I had last mailed you. I hope you must be doing great in your studies. I am still trying to find some more sponsorship for XPEG. I have collected around  $1500.

Anyway, I am writing to you for solutions to some of my queries. You had sent me some .mfcc and .hmm files. I tried recognition system with these .hmm files & vq.vq and they worked quite fine. At the same time I couldn’t train the system for HMM. I mean I couldn’t prepare HMM files.
Let me summarize my steps,
1. trainCodebook.java
codebook cbk = new codebook(pts);
cbk.saveToFile("c:/netbeans/files/sr/centroids/vq.vq");

It overwrites general codebook file vq.vq in each iteration. Hence, finally we are left with vq file corresponding to the last supplied mfcc file.
2. trainHMM.java

int num = Integer.parseInt(br.readLine());
training[i][j] = new int[num];
for (int k = 0; k < num; k++){
int temp = Integer.parseInt(br.readLine());
training[i][j][k] = temp;
}
at each iteration br.readLine() will return something like “-9.434228612906503 0.4207418722895202 -1.8604431629654252 0.5286063107419006 0.75098220413883430.9159399156201466 0.6298866369643994 1.0489847302384545 1.2777171708335926 0.5203764201302334 -0.30381123641461016 -0.15582105653264122“ hence we need to do something else to get temp value instead of parseInt statement.

Due to these difficulties I couldn’t test these files. But, I tried something else. I initialized ocvolume class enabling HMM mode and supplied input to dict with hmm files supplied by you along with the general codebook vq.vq. Surprisingly system was able to recognize my utteraces zero, one and two.

Will you please help me to make general codebook and hmm training model? I am waiting in anticipation.


Unfortunately I couldn't help him with the Hidden Markov Model because OCVolume didn't have a working implementation.
