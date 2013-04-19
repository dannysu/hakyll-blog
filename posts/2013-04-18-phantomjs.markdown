---
date: 2013-04-19 00:09:09
title: Headless WebKit with PhantomJS
tags: Mad Coding, PhantomJS
---
[PhantomJS][1] is just pure awesomeness. I gave a presentation on it at Well.ca
and talked about how one can use it to facilitate website testing (with
[CasperJS][4]). At the time we just encountered a bug with the website, so it
was fitting. Having a headless WebKit can do so much for you other than just
tesitng too!

<br>
**Taking Screenshots**

For another project I was working on, I wanted to capture webpages and save them
as images. Before I discovered PhantomJS, I used [CutyCapt][2] and [Qt
graphics-dojo example][5]. PhantomJS is much much simpler to use though. Check
out the official [rasterize example][3] for details.

<br>
**Fetching Actual Website Content**

Another thing that's great about having a headless browser is that you can use
it to fetch the actual content of a website. Nowadays so many sites are
javascript driven so simply fetching the initial HTML won't do. Below is how you
can use PhantomJS to capture the actual content.

<pre class="brush:c">
var page = require('webpage').create(),
    system = require('system'),
    fs = require('fs'),
    address, output;

if (system.args.length != 3) {
    console.log('Usage: grab.js URL filename');
    phantom.exit(1);
} else {
    address = system.args[1];
    output = system.args[2];

    page.open(address, function (status) {
        if (status !== 'success') {
            console.log('Unable to load the address!');
            phantom.exit();
        } else {
            window.setTimeout(function () {
                var results = page.evaluate(function() {
                    return document.documentElement.innerHTML;
                });

                try {
                    var f = fs.open(output, "w");
                    f.write(results);
                    f.close();
                } catch (e) {
                    console.log(e);
                }
                phantom.exit();
            }, 200);
        }
    });
}
</pre>

Save the code to grab.js and run it via PhantomJS by providing the URL to fetch
and the output file to save the content to.

  [1]: http://phantomjs.org
  [2]: http://cutycapt.sourceforge.net
  [3]: https://github.com/ariya/phantomjs/blob/master/examples/rasterize.js
  [4]: http://casperjs.org
  [5]: http://qt.gitorious.org/qt-labs/graphics-dojo/trees/master
