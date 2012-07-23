---
date: 2005-04-12 01:10:04
title: Hello Sweden!
tags: OCVolume
---

The OCVolume category contains posts with emails I received regarding [OCVolume](http://ocvolume.sf.net). This time, a Swedish student emailed me to ask for help.


> Hi!

I'm a Swedish student hoping maybe I could use your speech recognition engine in a project at the university. Our aim is to try to include voice commands in a client that should control a Lego robot. We hope that maybe your software could be useful for this purpose.

For starters, I was now trying to test the two sample programs from your homepage, but I have encountered some problems when trying to use it. I have downloaded all the files but I don't know where to put them and in what directory I should run e.g. "java training".

I have j2sdk1.4.2_02 installed on my laptop (with XP Home) and have tried to put the ocvolume.jar-file in the directory c:\j2sdk1.4.2_02\jre\lib\ext like it says on
your documentation page. I assume that is the directory to put it in with this verison of java, or should I put it in another directory?

The other question is, where should I put the class-files for the training- and voiceType-programs...? The install-files in the zip-files for those programs both says to "Simply extract the files from training.zip to your JRE's Extension Library directory (e.g. c:\jdk1.3\jre\lib\ext)\". I guess it means you have forgot to change the install-files, but I still tried to extract both the files in the ext-directory, but it didn't work... When I'm in the same directory as the trainng.class file (c:\j2sdk1.4.2_02\jre\lib\ext\training\dist), I get "Exception in thread "main" java.lang.NoClassDefFoundError: org/oc/ocvolume/train at training.main(training.java:30)" when trying to run "java training".
So I guess the program can't find the ocvolume-classfiles it needs, but as I said I don't know where to put the needed ocvolume.jar-file to be able to run the program.

I would be very happy if I could get an answer soon since the project deadline is on Friday...

Yours sincerely,




I helped her out by answering questions through email and she got it working:


> Hey Danny,

I just wanted to let you know that we managed to include OC Volume into our client for the project now! We had our final presentation today and it
worked out terrific! Here in Sweden we could use the voice commands to control a lego robot in a university in the US! It was really cool, and everyone was impressed!! ;) We used the non-continuous recording with no problems.

It worked fine when we downloaded the latest versions of the code... The link for all versions was kind of hidden, so maybe it would be good if you could show that a bit more clearly... And make sure that the links for downloading on the downloadpage give you the lastest version, not v1.0.0 as it gives you now... (I think...)

Well, maybe I'll use your speech engine in another project some times, now that I know that it works fine! I'll let you know if I will!!

Good luck with your university studies and free time development!! :)

Best regards,
