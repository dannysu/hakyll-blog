---
date: 2014-01-16 19:28:52
title: Accessing Google API using Service Account in Node.js
tags: Node.js, Mad Coding
---
Integrating [Google API][1] into a node.js application is fairly
straightforward.

First step is to include the [googleapis][2] module in your application:
<pre class="brush:bash">
npm install googleapis
</pre>

Then you need to specify the API and version you want to use. Below I show the
usage for Google+ Sign-In:
<pre class="brush:js">
var googleapis = require('googleapis');

var client;
googleapis
    .discover('plus', 'v1')
    .execute(function(err, data) {
        client = data;
    });
</pre>

If you make use of the code retrieved from client side in order to obtain
an OAuth token, then the API call would look something like this:
<pre class="brush:js">
    var oauth2 = new googleapis.OAuth2Client(CLIENT_ID, CLIENT_SECRET, 'postmessage');
    oauth2.getToken(code, function(err, tokens) {

        oauth2.credentials = tokens;
        client.plus.people.get({
            userId: 'me'
        })
        .withAuthClient(oauth2)
        .execute(function(err, result) {
        });
    });
</pre>

What if you want to use a Service Account that doesn't require interaction with
user & browser? For example, if you want to access Google Analytics
programmatically?

Well, what you'll need to use is the JWT (JSON Web Token) method. Doing it this
way wasn't well documented, so I want to share how it's done. I figured out how
to do this through googleapis module's [test code][3]. This is also another
reason why I want people to write tests.

First, you should have gotten a .p12 file and a secret to decrypt the file when
you created the service account in Google API console. Run the following command
to decrypt the p12 file.
<pre class="brush:bash">
openssl pkcs12 -in googleapi-privatekey.p12 -out googleapi-privatekey.pem -nocerts -nodes
</pre>

Second, in your node.js code, you'll instantiate googleapis.auth.JWT with the
path to the decrypted key file.
<pre class="brush:js">
    var CLIENT_ID = env.googleapis.client_id;
    var CLIENT_SECRET = env.googleapis.client_secret;
    var oauth2 = new googleapis.OAuth2Client(CLIENT_ID, CLIENT_SECRET, 'postmessage');

    var SERVICE_ACCOUNT_EMAIL = 'email@serviceaccount.com';
    var SERVICE_ACCOUNT_KEY_FILE = '/path/to/decrypted/key/file';
    var jwt = new googleapis.auth.JWT(
            SERVICE_ACCOUNT_EMAIL,
            SERVICE_ACCOUNT_KEY_FILE,
            null,
            ['https://www.googleapis.com/auth/analytics.readonly']);
</pre>

Lastly, obtain a client for the API you want to access and then call
jwt.authorize to obtain an access token. With the access token in hand, you can
give it to oauth and set it as the auth client.
<pre class="brush:js">
    var client;
    googleapis
        .discover('analytics', 'v3')
        .execute(function(err, data) {
            client = data;

            jwt.authorize(function(err, result) {
                oauth2.setCredentials({
                    access_token: result.access_token
                });

                client.analytics.data.ga.get({
                    "ids": "ga:########",
                    "start-date": 'YYYY-MM-DD',
                    "end-date": 'YYYY-MM-DD',
                    "metrics": "ga:visits"
                })
                .withAuthClient(oauth2)
                .execute(function(err, result) {
                });
            });
        });
</pre>

That's all! Hope it helps.

  [1]: https://developers.google.com/apis-explorer
  [2]: https://npmjs.org/package/googleapis
  [3]: https://github.com/google/google-api-nodejs-client/blob/master/test/test.auth.js
