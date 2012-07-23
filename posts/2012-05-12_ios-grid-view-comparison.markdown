---
title: iOS grid view comparison
tags: Apple, Mad Coding, GMGridView, iOS, iPhone, KKGridView, ObjC, SSCollectionView, SSToolkit
---
This is my experience using some open-source iOS grid view implementations. I tested out <a href="https://github.com/kolinkrewinkel/KKGridView" target="_blank">KKGridView</a>, <a href="https://github.com/gmoledina/GMGridView" target="_blank">GMGridView</a>, and SSCollectionView from <a href="http://sstoolk.it/" target="_blank">SSToolkit</a>.
<h3><strong>KKGridView</strong></h3>
Pros:
<ul>
	<li>Has sections support</li>
	<li>Claims to have good performance</li>
	<li>Acts like UITableView</li>
</ul>
Cons:
<ul>
	<li>Buggy</li>
	<li>With animation enabled, adding items to list causes weird animation. With animation is disabled, adding item to a section has an issue where all items in other sections don't refresh. See <a href="http://www.dannysu.com/wp-content/uploads/2012/05/18.mp4">video</a>.</li>
	<li>Deleting a cell then tap a cell at the end of a section causes some weird animation happening. See <a href="http://www.dannysu.com/wp-content/uploads/2012/05/19.mp4">video</a>.</li>
</ul>
&nbsp;
<h3><strong>GMGridView</strong></h3>
Pros:
<ul>
	<li>Allows drag &amp; drop to re-arrange items in grid view</li>
	<li>Very easy to use your own view. Just do cell.contentView = yourOwnView.</li>
</ul>
Cons:
<ul>
	<li>No sections support</li>
	<li>Always registers its own gesture recognizer even without delegates provided by consumer</li>
</ul>
&nbsp;
<h3><strong>SSCollectionView</strong></h3>
Pros:
<ul>
	<li>Has sections support</li>
	<li>Acts like UITableView</li>
</ul>
Cons:
<ul>
	<li>More restrictive cell view implementation (You're forced to use SSCollectionViewItem that has specific subviews in there. It's possible to use your own view but more troublesome than others.)</li>
</ul>
