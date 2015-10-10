---
date: 2014-02-17 15:47:07
title: Accessing Beanstream with Node.js
tags: Mad Coding, Beanstream, Node.js, Stripe, Braintree, payment
---
I needed to access [Beanstream][1] from node.js server but there wasn't any
existing library that I could re-use. It wasn't too hard to write one, but I
needed help from Beanstream support to resolve some confusion that came from bad
documentation. This post provides details on how you can access Beanstream from
node.

<br>

## **1. Getting and Configuring a Test Account**

Visit Beanstream's page for [creating test account][2] and create one. 

Note down your **merchant ID**. It's visible at the top right of the page after you
login.

Next click `configuration -> payment profile configuration`, and under "Security
Settings" section, get an API access passcode.

Then click `administration -> order settings`, and under "Transaction Validation
Options" check the following options:

 - "Require hash validation on all Payment Gateway transaction requests"
 - "Include hash validation in Transaction Response Page redirection and
   Payment Gateway Response Notification"

In the Hash key text box, enter the desired hash key and select SHA-1.

<br>

## **2. Talk to Legato API**

With the new [Legato API][3], Beanstream now works pretty much like how Stripe,
Braintree and others work. You pass credit card information directly to
Beanstream server and receive a representation back. You then send the
representation to your server for processing in order to ensure credit card info
never touches your server.

To talk to the Legato API, I used [restify][4] as the client but you can use
others. One thing that the documentation was wrong about is that it talked about
some testing endpoint, but when I called Beanstream the support person said to
just use https://www.beanstream.com.

<pre><code class="javascript">
var restify = require('restify');

function getSingleUseToken(credit_card_number, callback) {

    var client = restify.createJSONClient({
        url: "https://www.beanstream.com",
        headers: {
            'Connection': 'close'
        }
    });

    var data = {
        "number": credit_card_number,
        "expiry_month": "11",
        "expiry_year": "15",
        "cvd": "123"
    };

    client.post("/scripts/tokenization/tokens", data, function(err, req, res, obj) {
        if (err) {
            return callback(err);
        }

        if (obj.code != 1) {
            return callback("Code is not 1");
        }

        // You get back the single-use token to be used later
        return callback(null, obj.token);
    });
}

</code></pre>

<br>

## **3. Create a Secure Payment Profile**

What Stripe call "customer", Beanstream calls them "Secure Payment Profile"
(SPP). To create a SPP that you can later charge, you will create a SPP and
associate it with the token you receive in previous step. To do that it's just a
simple POST to the server.

<pre><code class="javascript">
var client = restify.createJSONClient({
    url: "https://www.beanstream.com",
    headers: {
        'Connection': 'close'
    }
});

// Use merchant ID and pass code from test account
var data = {
    operationType: 'N',
    serviceVersion: "1.0",
    merchantId: merchant_id,
    passCode: pass_code,
    responseFormat: "QS",
    singleUseToken: single_use_token,
    "trnCardOwner": "Card Owner Name",
    "ordEmailAddress": "email@email.com",
    "cardValidation": 1
};

client.post(path, data, function(err, req, res, result) {
    if (err) {
        // ERROR!
    }

    var obj = querystring.parse(result);
    // You get back result.customerCode that you can use later
});

</code></pre>

For details about what you can pass as parameters to the SPP API, see the
Classic Developer Guides [here][5].

<br>

## **4. Charge a Secure Payment Profile**

Charging a Secure Payment Profile involves using the customer code you obtained
in step 3 and signing the request. In step 1 you already set up a hash key in
the test account for Beanstream to validate your requests. In this step, you
have to calculate a SHA-1 hash using the same key before sending request to
Beanstream.

<pre><code class="javascript">
// Use merchant ID from test account
// Customer code is obtain in step 3
// amount is how much to charge to the credit card
var data = {
    requestType: 'BACKEND',
    merchant_id: merchant_id,
    customerCode: customer_code,
    trnAmount: amount
}

// Create a unique order number
var buffer = new Buffer(16);
uuid.v4(null, buffer);
data.trnOrderNumber = buffer.toString('base64');

var qs = querystring.stringify(data);
// The hash key you set up in test account
var to_be_hashed = qs + hash_key;
var shasum = crypto.createHash('sha1');
shasum.update(to_be_hashed);
var digest = shasum.digest('hex');

data.hashValue = digest;

client.post(path, data, function(err, req, res, result) {
    if (err) {
        // ERROR!
    }

    var obj = querystring.parse(result);
});

</code></pre>

Both Stripe and Braintree both have node.js libraries, [here][6] and [here][7].
If you happen to need to use Beanstream, I hope this post helped.

  [1]: http://www.beanstream.com
  [2]: http://developer.beanstream.com/create-test-account/
  [3]: http://developer.beanstream.com/documentation/legato/
  [4]: http://mcavage.me/node-restify/
  [5]: http://developer.beanstream.com/documentation/classic-apis/
  [6]: https://github.com/stripe/stripe-node/
  [7]: https://www.npmjs.org/package/braintree
