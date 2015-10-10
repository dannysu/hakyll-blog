---
date: 2012-05-08 22:21:02
title: iOS folding effect
tags: Apple, Mad Coding, iOS, iPhone, ObjC
---
The designer I work with passed around some videos of the deal in iPhone app on
Monday. This particular video demonstrates a kind of fold effect as the user
pan horizontally across the screen:
[http://vimeo.com/41495357](http://vimeo.com/41495357)

I was curious about how difficult this is to do on iOS and had a little fun
project last night. In summary, to achieve the same effect you simply need to
do the follow 4 things:

1. Convert UIView into an image
1. Split resulting image into 2 vertical halves
1. Tint the right image slightly
1. Use Core Animation to rotate left image one way anchored on left side and
   rotate right image the other way but anchored on the right side

Here's a video of my results: [foldeffect][1]

Code below. I had help from this [Flipboard example][2] showing how to use Core
Animation.

<pre><code class="objc">
- (void)pan:(UIPanGestureRecognizer *)gesture {
    if (gesture.state == UIGestureRecognizerStateBegan) {
        // Grab a screenshot of the current table view
        UIGraphicsBeginImageContext(self.view.bounds.size);
        [self.view.layer renderInContext:UIGraphicsGetCurrentContext()];
        UIImage *viewImage = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();

        // The size after splitting the screenshot in two halves down the middle
        CGSize size = CGSizeMake(viewImage.size.width / 2, viewImage.size.height);

        // A solid colour image
        UIGraphicsBeginImageContextWithOptions(size, NO, 0.0);
        CGContextRef context = UIGraphicsGetCurrentContext();
        CGContextSetFillColorWithColor(context, [[UIColor blackColor] CGColor]);
        CGContextFillRect(context, CGRectMake(0, 0, size.width, size.height));
        UIImage *solidColorImage = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();

        // The left half of the image
        UIGraphicsBeginImageContextWithOptions(size, NO, 0.0);
        [viewImage drawAtPoint:CGPointMake(0, 0) blendMode:kCGBlendModeNormal alpha:1.0];
        UIImage *leftImage = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();

        // The right half of the image
        UIGraphicsBeginImageContextWithOptions(size, NO, 0.0);
        [viewImage drawAtPoint:CGPointMake(-size.width, 0) blendMode:kCGBlendModeNormal alpha:1.0];
        [solidColorImage drawAtPoint:CGPointMake(0, 0) blendMode:kCGBlendModeMultiply alpha:0.1];
        UIImage *rightImage = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();

        [self.detailViewController.image1 setImage:leftImage];
        self.detailViewController.image1.frame = CGRectMake(self.detailViewController.image1.frame.origin.x, self.detailViewController.image1.frame.origin.y, size.width, size.height);

        [self.detailViewController.image2 setImage:rightImage];
        self.detailViewController.image2.frame = CGRectMake(leftImage.size.width, self.detailViewController.image2.frame.origin.y, size.width, size.height);
    }
    else if((gesture.state == UIGestureRecognizerStateChanged) ||
       gesture.state == UIGestureRecognizerStateEnded){

        CGPoint leftTranslation = [gesture translationInView:self.detailViewController.image1];
        [self rotateView:self.detailViewController.image1 degrees:leftTranslation.x/6 leftView:nil];
        [self rotateView:self.detailViewController.image2 degrees:leftTranslation.x/6 leftView:self.detailViewController.image1];
    }
}

- (void)rotateView:(UIView *)view degrees:(float)degrees leftView:(UIView *)leftView {
    if (leftView != nil) {
        // for right side, rotate the same amount as left side but in opposite direction
        degrees = -1 * degrees;
    }

    CATransform3D rotationAndPerspectiveTransform = CATransform3DIdentity;
    rotationAndPerspectiveTransform.m34 = 1.0 / -1000;
    rotationAndPerspectiveTransform = CATransform3DRotate(rotationAndPerspectiveTransform, [self convertToRadians:degrees], 0.0f, 1.0f, 0.0f);

    float ypos = view.layer.position.y;

    if (leftView == nil) {
        view.layer.anchorPoint = CGPointMake(0.0, 0.5);
        view.layer.position = CGPointMake(100, ypos);
    } else {
        view.layer.anchorPoint = CGPointMake(1.0, 0.5); // anchor on right edge
        view.layer.position = CGPointMake(100 + leftView.frame.size.width * 2, ypos);
    }
    view.layer.transform = rotationAndPerspectiveTransform;

    [view setNeedsDisplay];
}

</code></pre>

  [1]: /files/videos/foldeffect.mov
  [2]: https://github.com/rbreve/Flipboard-3D-Transform-Effect-Example
