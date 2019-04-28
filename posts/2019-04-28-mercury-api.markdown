---
date: 2019-04-28 12:14:41 PDT
title: Permissions for Mercury API
tags: Readability, Mercury
description: Permissions for Mercury API
---
I've been using various tools to gather my RSS feeds for several years now.
First with [Huginn][2] and now I have a [setup using Node-RED][1]. A key part
of my setup involves parsing webpages and output just the essential content
similar to Reader mode in browsers.

Originally, I was using the Readability Parser API. However, when it was
[shutdown][3] in 2016, they recommended people move to use the [Mercury
Toolkit][4].

Sadly, Mercury also [shutdown their parser API][5] recently. I suspect it's
probably also a cost thing. Their blog [post on reducing serverless cost][8] is
a good read. But they kindly open-sourced both the [parser code][7] and the
[API][6] so that people can just self-host.

Now that I've set it up on my own AWS account, here are some tips to help you
do the same.

## AWS IAM Policy

The Mercury README doesn't specify what AWS IAM Policy permissions you need to
grant. This [GitHub Issue][11] linked to the serverless documentation that
contains the [exhaustive list][12] that you need.

However, it doesn't seem like you need that exhaustive list. This is a trimmed
down list that I'm currently using:
```
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "VisualEditor0",
            "Effect": "Allow",
            "Action": [
                "apigateway:*",
                "cloudformation:CancelUpdateStack",
                "cloudformation:ContinueUpdateRollback",
                "cloudformation:CreateChangeSet",
                "cloudformation:CreateStack",
                "cloudformation:CreateUploadBucket",
                "cloudformation:DeleteStack",
                "cloudformation:Describe*",
                "cloudformation:EstimateTemplateCost",
                "cloudformation:ExecuteChangeSet",
                "cloudformation:Get*"
                "cloudformation:List*",
                "cloudformation:UpdateStack",
                "cloudformation:UpdateTerminationProtection",
                "cloudformation:ValidateTemplate",
                "events:DeleteRule",
                "events:DescribeRule",
                "events:ListRuleNamesByTarget",
                "events:ListRules",
                "events:ListTargetsByRule",
                "events:PutRule",
                "events:PutTargets",
                "events:RemoveTargets",
                "iam:GetRole",
                "iam:PassRole",
                "lambda:*",
                "logs:CreateLogGroup",
                "logs:DeleteLogGroup",
                "logs:DescribeLogGroups",
                "logs:DescribeLogStreams",
                "logs:FilterLogEvents",
                "logs:GetLogEvents",
                "s3:CreateBucket",
                "s3:DeleteBucket",
                "s3:DeleteBucketPolicy",
                "s3:DeleteObject",
                "s3:DeleteObjectVersion",
                "s3:GetObject",
                "s3:GetObjectVersion",
                "s3:ListBucket",
                "s3:PutObject",
            ],
            "Resource": "*"
        }
    ]
}
```

## Adding x-api-key Validation

The original Mercury API requires that you include x-api-key in the header and
use an API key assign to your account. The open-source serverless.yml doesn't
set this up, which potentially opens your own deployment to anybody to use.

Adding the same thing to your own deployment is easy. Looking at the
[documentation][10], all you need to do is add the following lines to your
serverless.yml file. An API key will automatically be created in Amazon API
Gateway for you.

```
  provider:
+   apiKeys:
+     - mercury
  functions:
    mercuryParser:
      events:
        - http:
+           private: true
    parseHtml:
      events:
        - http:
+           private: true
```

See commit [here][9].



  [1]: /2016/12/29/huginn-to-node-red/
  [2]: https://github.com/huginn/huginn
  [3]: https://medium.com/@readability/the-readability-bookmarking-service-will-shut-down-on-september-30-2016-1641cc18e02b
  [4]: https://mercury.postlight.com/
  [5]: https://mailchi.mp/postlight/action-required-mercury-parser-api-will-sunset-in-60-days
  [6]: https://github.com/postlight/mercury-parser-api
  [7]: https://github.com/postlight/mercury-parser
  [8]: https://postlight.com/trackchanges/serving-39-million-requests-for-370-month-or-how-we-reduced-our-hosting-costs-by-two-orders-of
  [9]: https://github.com/dannysu/mercury-parser-api/commit/eba818b6730d5c5f83920997569c98c348eba1e0
  [10]: https://serverless.com/framework/docs/providers/aws/guide/serverless.yml/
  [11]: https://github.com/postlight/mercury-parser-api/issues/9
  [12]: https://gist.github.com/ServerlessBot/7618156b8671840a539f405dea2704c8
