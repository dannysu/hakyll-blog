---
date: 2013-04-13 01:25:43
title: Poor man's PagerDuty
tags: Start-up, AvidTap, AppFog, Twilio, Mad Coding, Amazon
---
Amazon is a really cool place with very interesting challenges to tackle. The
group I was in when I interned there was the Item Authority team. They are the
ones who determine if two items being sold on the Amazon platform are actually
the same thing. Anyway, Amazon engineers are all on pager rotation for fixing
stuff when things go down. How frequently one needs to carry pager varies
depending on the team size. For my startup [AvidTap][1], I also needed something
to monitor the health of our services and be alerted when things go bad. We used
to have some issues with the VM on our host, but thankfully it hasn't happened
lately.

I'm aware of the start-up PagerDuty that probably makes this very easy, but
throwing together something simple is also very easy with the technology now.
Below I walk through how you can get your own poor man's PagerDuty.


# Accounts You Need

1. Sign up a free account with [AppFog][3]
1. Sign up for a trial account with [Twilio][4]
1. [AWS][5] account

Personally I was already using all of the above services for my personal
projects, so putting this together was quick.


# Grab The Source Code

My [monitoring repo on github][2] contains the code needed for this.

1. Start by git clone the repo: `git clone https://github.com/dannysu/monitoring.git`
1. `cd monitoring`
1. Grab the node.js modules needed: `npm install`
1. Modify the test function to access your services and check for status


# Setting Things Up For AppFog

1. Install [AppFog CLI][6]
1. `af login` and provide your credentials
1. `af push monitoring`
1. Answer `Y` to "Would you like to deploy from the current directory? [Yn]:"
1. Answer `n` to "Detected a Node.js Application, is this correct? [Yn]:"
1. Type the number associated with Standalone. At this time it's 6: "Select Application Type: 6"
1. Select `node08` as runtime
1. Use `node app.js` as the Start Command
1. Choose wherever you want the monitoring service to be running from
1. Then just choose default options rest of the way

The app should now be deployed.


# Filling in AWS Details

1. Login to AWS
1. Grab your security credential (Available by hovering over your name and click
   on it)
1. Login to AppFog and select the monitoring app
1. Click on the "Env Variables" tab on the side
1. Provide values for both `AWSAccessKeyID` and `AWSSecretKey`
1. Modify app.js to fill in the email addresses you want to send alerts to


# Filling in Twilio Details

1. Login to Twilio
1. Your account SID and auth token are right at the top of the dashboard
1. Login to AppFog and select the monitoring app
1. Click on the "Env Variables" tab on the side
1. Provide values for both `TWILIO_ACCOUNT_SID` and `TWILIO_AUTH_TOKEN`
1. Modify app.js to fill in the phone numbers you want to send alerts to

You also need to have a TwiML file accessible somewhere. You can see [Twilio
documentation][7] for more info on what can be in the TwiML file. I included an
example in my code [here][8]. Just host it somewhere and modify the code to
point the `url` variable to it.

## 

Finally, after all these changes, run `af update monitoring` to upload changes
to AppFog. Enjoy! Let's hope you don't ever get alerts from this. However,
should your service go down, this will alert you via email/sms/phone.

  [1]: http://avidtap.com
  [2]: https://github.com/dannysu/monitoring
  [3]: https://www.appfog.com
  [4]: http://www.twilio.com
  [5]: http://aws.amazon.com
  [6]: https://docs.appfog.com/getting-started/af-cli
  [7]: http://www.twilio.com/docs/api/2008-08-01/twiml/say
  [8]: https://github.com/dannysu/monitoring/blob/master/twiml.xml
