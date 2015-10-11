---
date: 
title: ListView inside a ListView inside a ListView on Android
tags: Google, Android, programming, mobile, Mad Coding
---
Suppose you need to produce an UX involving multiple levels of ListViews similar
to the following wireframe. How do you do that on Android?

<img src="//imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg0LEgVpbWFnZRj52HwM" class="centered">

It's not an ideal UX, but let's say that's the requirement and you must make it
work. I dug into it and figured it out. Feel free to jump to the end for the
example project if you just want to see it on a device/emulator.

There are three major steps to making this UX. First, you need to make a
horizontal ListView.

## **Horizontal ListView**

Using a [vertical ListView][1] is fairly easy on Android and there are lots of
resources online for that. However, Google didn't provide an implementation for
horizontal ListView, so people mostly resorted to forking the vertical ListView
and try to get it to do what they want. With the introduction of
[RecyclerView][2] it's now much easier to get a horizontal ListView going.

To use RecyclerView, you need to first add it to the list of dependencies of
your project. Edit your app's build.gradle to add
`com.android.support:recyclerview-v7:+`.

```html
dependencies {
    compile fileTree(dir: 'libs', include: ['*.jar'])
    compile 'com.android.support:appcompat-v7:23.0.0'
    compile 'com.android.support:recyclerview-v7:+'
}
```

Then in your layout file, you would need to add the RecyclerView to where you
want it to show up:

```xml
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout ...>
    <android.support.v7.widget.RecyclerView
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:id="@+id/recyclerView" />
</LinearLayout>
```

Lastly you need to configure the RecyclerView so that it uses a horizontal
LinearLayoutManager.

```java
recyclerView = (RecyclerView)convertView.findViewById(R.id.recyclerView);

// Give it a horizontal LinearLayoutManager to make it a horizontal ListView
recyclerView.setHasFixedSize(true);
LinearLayoutManager layoutManager = new LinearLayoutManager(this.getContext(),
        LinearLayoutManager.HORIZONTAL, false);
recyclerView.setLayoutManager(layoutManager);

recyclerView.setAdapter(adapter);
```

Since the UX we want has a top-level vertical scrolling ListView containing a
bunch of horizontal ListViews, you'd put the code for configuring recyclerView
inside the adapter for the vertical ListView. Each item of the red ListView as
shown in wireframe contains a horizontal ListView. That means the `getView()` of
the adapter driving the red ListView would be configuring the blue horizontal
ListViews.

```java
@Override
public View getView (final int position, View convertView, ViewGroup parent) {

    if (convertView == null) {
        LayoutInflater inflater = (LayoutInflater)getContext()
                .getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        convertView = inflater.inflate(resourceId, parent, false);

        ViewHolder viewHolder = new ViewHolder();
        viewHolder.recyclerView = (RecyclerView)convertView
                .findViewById(R.id.recyclerView);

        // Give it a horizontal LinearLayoutManager to make it a horizontal ListView
        if (viewHolder.recyclerView != null) {
            viewHolder.recyclerView.setHasFixedSize(true);
            LinearLayoutManager layoutManager = new LinearLayoutManager(this.getContext(),
                    LinearLayoutManager.HORIZONTAL, false);
            viewHolder.recyclerView.setLayoutManager(layoutManager);
        }

        convertView.setTag(viewHolder);
    }

    final HorizontalScrollableItem item = getItem(position);
    VerticalScrollableItemAdapter verticalScrollableItemAdapter = item.getAdapter();

    ViewHolder viewHolder = (ViewHolder)convertView.getTag();

    if (viewHolder.recyclerView != null && verticalScrollableItemAdapter != null) {
        viewHolder.recyclerView.setAdapter(verticalScrollableItemAdapter);
    }

    return convertView;
}
```

Once you've got a vertical ListView containing horizontal ListViews working, the
next step is to have each horizontal ListView to also be populated with vertical
ListViews.

## **Third Level (vertical) ListViews**

The third-level ListViews is shown in the wireframe in green colour. It isn't
all that different than the top-level red ListView. The complicated part is
that you need to somehow tell those green ListViews what to show by providing it
with their own adapters. How you do that depends on your application and design.

For my example project, I chose to create four helper classes:

- HorizontalScrollableItemAdapter
- HorizontalScrollableItem
- VerticalScrollableItemAdapter
- VerticalScrollableItem

These classes can be made such that you can reuse them for however many levels
of ListViews you want. I'm not satisfied with the naming, but it's what I have
for now while keeping things generic. If you have a better suggestion, please do
tell! Also, I expect a project specific naming might be more helpful too.

Below is how each class is used in the hierarchy of ListViews.

<a href="//imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRiZ_vcBDA" target="_blank"><img src="//imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRiZ_vcBDA" class="centered"></a>

The top-level red ListView uses `HorizontalScrollableItemAdapter` as its
adapter, which produces a bunch of items capable of being scrolled horizontally
as well.

The `HorizontalScrollableItem` is what's behind each second-level blue
ListViews. Each `HorizontalScrollableItem` also contains a
`VerticalScrollableItemAdapter` which provides it with the items to show.

The `VerticalScrollableItem` is what's behind each third-level green ListViews.
In my example project, it just uses a regular adapter and terminates the
embedding.

These naming are really confusing. For a real project, I'd suggest using names
that makes sense in the context and alias one of the above for better
readability.

## **Disable Touch on Top ListView**

Once you've got all the hierarchy sorted out, one of the issues you'll run into
is when you scroll the green ListViews up and down, the red ListView also wants
to scroll. The behaviour we want is that when scrolling green ListView, red
ListView shouldn't move. To make this happen I created another helper class
named `MultiLevelListView` and swapped instance of ListView with it.

The `MultiLevelListView` simply extends from ListView but maintains a
reference to the higher level ListView. This is so that when the green ListView
is scrolling, the green ListView can tell the red ListView to ignore touch
events. The main gist of how that works is shown below. When green ListView
gets `onTouchEvent`, it disables touch on the higher level ListView. Once the
touch event finishes, then touch on higher level ListView is re-enabled.

```java
private void setTouchInterceptEnabled(boolean value) {
    mEnableTouchIntercept = value;
}

@Override
public boolean onTouchEvent(MotionEvent ev) {

    if (mParentListView != null) {
        if (ev.getAction() == MotionEvent.ACTION_DOWN) {
            // Disable intercepting touch to allow children to scroll
            mParentListView.setTouchInterceptEnabled(false);
        } else if (ev.getAction() == MotionEvent.ACTION_UP ||
                ev.getAction() == MotionEvent.ACTION_CANCEL) {
            // Re-enable after children handles touch
            mParentListView.setTouchInterceptEnabled(true);
        }
    }

    return super.onTouchEvent(ev);
}

@Override
public boolean onInterceptTouchEvent(MotionEvent ev) {
    if (mEnableTouchIntercept) {
        return super.onInterceptTouchEvent(ev);
    }
    return false;
}
```

## **Performance Considerations**

Typically for performance reasons you'll want to offload as much as you can to a
separate thread instead of the UI thread when rendering the items. You typically
also will want to stop doing anything while a ListView is scrolling quickly or
being flicked. While there is no performance issues with the simplistic example
project I made, I expect that you'll need to figure it out for a real project
and deal with propagating those decisions across multiple levels of ListViews.

You want to stop child ListViews' tasks when the top-level ListView is being
flicked for example. That'll be an exercise for another day.

## **Example Project**

See code on [github][3]. The final product looks like this:

<a href="//imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRipzPgBDA" target="_blank"><img src="//imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRipzPgBDA" class="centered"></a>


  [1]: https://developer.android.com/reference/android/widget/ListView.html
  [2]: https://developer.android.com/reference/android/support/v7/widget/RecyclerView.html
  [3]: https://github.com/dannysu/ListListList
