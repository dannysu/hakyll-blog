---
date: 2012-05-12 09:03:20
title: iOS grid view comparison
tags: Apple, Mad Coding, GMGridView, iOS, iPhone, KKGridView, ObjC, SSCollectionView, SSToolkit
---
This is my experience using some open-source iOS grid view implementations. I
tested out [KKGridView](https://github.com/kolinkrewinkel/KKGridView),
[GMGridView](https://github.com/gmoledina/GMGridView), and SSCollectionView
from [SSToolkit](http://sstoolk.it/).

### **KKGridView**

Pros:
	
- Has sections support
- Claims to have good performance
- Acts like UITableView

Cons:
	
- Buggy
- With animation enabled, adding items to list causes weird animation. With
  animation is disabled, adding item to a section has an issue where all items
  in other sections don't refresh. See
  [video](/files/videos/18.mp4).

- Deleting a cell then tap a cell at the end of a section causes some weird
  animation happening.
  See [video](/files/videos/19.mp4).

### **GMGridView**

Pros:
	
- Allows drag & drop to re-arrange items in grid view
- Very easy to use your own view. Just do cell.contentView = yourOwnView.

Cons:
	
- No sections support
- Always registers its own gesture recognizer even without delegates provided
  by consumer

### **SSCollectionView**

Pros:

- Has sections support
- Acts like UITableView

Cons:
	
- More restrictive cell view implementation (You're forced to use
  SSCollectionViewItem that has specific subviews in there. It's possible to
  use your own view but more troublesome than others.)

