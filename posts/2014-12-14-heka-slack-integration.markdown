---
date: 2014-12-14 21:09:23 PST
title: Heka + Slack Integration
tags: Mad Coding, Heka, Slack
---
At [Kash][1] we want to be notified of things that happen in our logs. After
surveying what's out there, I decided to give [Heka][2] a try. Heka is written
in Go by people at Mozilla. I am really impressed with how well thought out the
design is and how extensible it is. You can plug parts into the processing via
Lua scripts.

Heka has pretty good [documentation][3] and it gives you an idea of how Inputs,
Decoders, Filters, Encoders and Outputs all come together to form a really great
design. Here I'll just describe how I integrated Heka with Slack so that
relevant log entries populate a particular channel I have at work.


# Heka Design Overview

Heka has 5 major components:

- Inputs
- Decoders
- Filters
- Encoders
- Outputs

You can customize any of the 5 parts to do what you want. This makes for an
extensible and flexible system.

Inputs pull data into Heka's processing pipeline. You can then optionally pipe
things through a decoder, and optionally pipe things through filters if you
want. Finally before sending data to the destination (Outputs) you can
optionally do some more encoding as well.


# Heka Input

The hekad.toml configuration file is used to setup the processing pipeline. For
my situation, my inputs are simply log files on Linux systems. Luckily there are
pre-built Input for grabbing data from log files.

Here is the relevant part in my toml file:

```ini
[ExampleLog]
type = "LogstreamerInput"
log_directory = "/var/log/somewhere"
file_match = 'some_filename.log'
```

What this does is set up an Input of type LogstreamerInput and configure it to
monitor a particular log file.


# Heka Decoder

For my purposes, I didn't need a decoder but I'm including a section here for
completeness. For later stages of the processing that I want to do, the line
by line data from my log file is sufficient.

If, for example, my input took in binary data and I want to transform it first
into plain text, then I could make use of a decoder to process it. Maybe if the
input is slurping up compressed data and I wanted to use a decoder to decompress
it, then this is where you could use one.


# Heka Filter

Heka Filter gives you the ability to aggregate data or filter things out before
the later parts processes data. You can use it to do things like alert me on the
3rd time something happens, or only alert me if X events happen within Y interval.

For my use, I'm using the filter to group lines from my log file before sending
the group of lines through. Below is an example toml configuration:

```ini
[ExampleLogAggregator]
type = "SandboxFilter"
message_matcher = "Logger == 'ExampleLog'"
filename = "/some/place/log_aggregator.lua"
ticker_interval = 15
can_exit = false
```

This configuration says that it wants data that came from ExampleLog Input. As
data goes through the Heka system, they get metadata attached that associate
them with specific components that processed them. In my case, I want the data
coming from my log file as provided by ExampleLog Input.

Heka allows you to write custom logic in Lua without having to compile
anything. The configuration above points Heka to the location of the Lua script.

`ticker_interval` is an optional configuration. I set it to trigger the filter
  processing at least every 15 seconds. I do this because I'm using the Filter
  to aggregate log data and send the data out in groups, so I need to
  periodically flush out what's been collected.

`can_exit` is also an optional configuration. I set it to false to make sure
  if the filter exits the entire Heka system also exits.

---

Next up is to actually implement the filter logic in Lua. There are two main
functions to implement: `process_message` and `timer_event`.

`process_message` is called whenever the upstream Input has new data. For my
aggregator Filter, my implementation simply accumulates data for later.

```lua
require "os"
require "string"

local buffer = ""

function process_message()
    local payload = read_message("Payload")
    buffer = buffer .. payload
    return 0
end
```

The `process_message` implementation uses a variable `buffer` to concatenate
string with each call to `process_message`. The Heka function `read_message`
allows you to grab data ("Payload") coming out from Input or some metadata
associated with it.

Now that I have the logic to simply accumulate data until some time, I need to
implement `timer_event` to actually send it downstream for processing. Here's
my example implementation:

```lua
function timer_event(ns)
    if string.len(buffer) > 0 then
        inject_payload("txt", "", buffer)
        buffer = ""
    end
end
```

Note that I configured `timer_event` to be triggered every 15 seconds via the
`ticker_interval` configuration in toml. So what this function does is when it
gets invoked, it checks to see if we accumulated any log data in variable
`buffer`, if we did then inject the payload back into Heka pipeline to be processed.


# Heka Encoder

In order to integrate with Slack, we need to somehow transform log data into the
JSON format Slack expects. That's where Heka Encoder comes in. Again, it's
something we can code in Lua. Here's an example configuration:

```ini
[SlackEncoder]
type = "SandboxEncoder"
filename = "/some/place/slack_encoder.lua"
```

It's very simple. All it does is define an Encoder and point to the Lua script
that implements the logic.

The Lua script is also not too bad. Slack expects JSON to be sent to its Webhook
URL, so we need to transform our raw log data into a JSON format.

```lua
require "os"
require "string"
require "table"

local cjson = require("cjson")

function process_message()
    local payload = read_message("Payload")

    local slack_alert = {}
    slack_alert["icon_emoji"] = ":heavy_exclamation_mark:"
    slack_alert["username"] = "something you like"
    slack_alert["text"] = payload
    slack_alert["channel"] = "#something"

    inject_payload("json", "Slack", cjson.encode(slack_alert))
    return 0
end
```

Here we again implement the `process_message` function that gets invoked for
each piece of data to be processed. We read out the content via `read_message`
call and then construct an object then encode it and inject it back into the
pipeline to be processed.


# Heka Output

Finally we're ready to actually send the JSON data to Slack. We do this using
the Heka Output component. Below is the configuration used to do that.

```ini
[HttpOutput]
message_matcher = "Logger == 'ExampleLogAggregator'"
address = "https://your/slack/webhook/url"
encoder = "SlackEncoder"
```

This configuration does the following things:

- Tells Heka to use pre-built [HttpOutput] component
- Gives it a URL via the `address` configuration
- Tells it to grab data from ExampleLogAggregator and encode it using
SlackEncoder


# Conclusion

So that's how you can use Heka to monitor log files and get notified via Slack.

Putting it all together, the configuration file in the end looks like this:

```ini
[ExampleLog]
type = "LogstreamerInput"
log_directory = "/var/log/somewhere"
file_match = 'some_filename.log'

[ExampleLogAggregator]
type = "SandboxFilter"
message_matcher = "Logger == 'ExampleLog'"
filename = "/some/place/log_aggregator.lua"
ticker_interval = 15
can_exit = false

[SlackEncoder]
type = "SandboxEncoder"
filename = "/some/place/slack_encoder.lua"

[HttpOutput]
message_matcher = "Logger == 'ExampleLogAggregator'"
address = "https://your/slack/webhook/url"
encoder = "SlackEncoder"
```

  [1]: http://withkash.com
  [2]: https://github.com/mozilla-services/heka
  [3]: http://hekad.readthedocs.org
