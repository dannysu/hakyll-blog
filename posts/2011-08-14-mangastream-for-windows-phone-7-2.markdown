---
date: 2011-08-14 21:16:18
title: MangaStream for Windows Phone 7
tags: Mad Coding, Microsoft, C#, MangaStream, Silverlight, SQL CE, Windows Phone 7, WP7
---
For the time being I'm done updating the app I wrote during my free time
between Windows Phone 7 releases (after WP7 and after 'Mango'). I also uploaded
the source code to GitHub and started using git for source control. There are
other refactoring and features I want to add such as prefetch of images, but
those will have to wait.

The app is a MangaStream mobile client originally written using the initial
Windows Phone 7 SDK and since been updated using WP7 'Mango' SDK. One thing
that's really awesome about WP7 SDK is that it's quite easy to make an app that
looks modern and minimalistic. I personally love the minimalist look & feel of
Windows Phone 7.

You can click one of the thumbnails below to see the full feature list and more
screenshots:

[![](/images/MangaStream1-150x150.png)][1]
[![](/images/MangaStream2-150x150.png)][1]
[![](/images/MangaStream3-150x150.png)][1]

There were some issues that I had to spend more time debugging through, so here
are my learnings:

<br>

## **1. You might have to flush things you store to IsolatedStorageSettings**

For my app, I implemented a Background Agent so that it will check for new
manga releases and notify me through a toast notification as well as showing
the information on the Live Tile. In order to know whether there are new
releases, I save the latest manga ID in IsolatedStorageSettings after each time
I download the metadata. However, I was facing an issue where the changes done
in my Background Agent was not actually saved the next time my code runs.

Adding a call to Save() allowed the setting to be flushed to wherever it's
actually stored:

<pre><code class="csharp">IsolatedStorageSettings.ApplicationSettings.Save();</code></pre>

The [IsolatedStorageSettings documentation][2] says that "Data written to the
IsolatedStorageSettings object is saved when the application that uses the
class is closed". That statement is true when I used it in my main app to
serialize settings, but I found that I needed explicit Save() in the Background
Agent.

<br>

## **2. Use IsolatedStorageExplorerTool (ISETool) to retrieve and examine things
stored in Isolated Storage**

The [MSDN documentation][3] has the location where you can run the ISE command
line tool. By retrieving files in Isolated Storage, I was able to examine
several things and found bugs I missed.

I found that the values you store to IsolatedStorageSettings is actually stored
to a file in XML format with the name "__ApplicationSettings". Also, using the
ISETool I grabbed the SQL CE file to examine. Lastly, I found out that I wasn't
properly cleaning up icons I downloaded through Background Transfer because
those files are still present in "shared\transfers" folder. Another surprise to
me is that even when Background Transfer attempts to download a nonexistent
file, it'll also create a zero byte file, which I also wasn't cleaning up.

<br>

## **3. You can't store the same model to different Tables using LINQ to SQL**

Using LINQ to SQL is new to me, so I might be missing something.

In my first version of the app, I simply stored cached data to different files
in Isolated Storage. With 'Mango', I decided to move things to SQL CE but ran
into a problem. Initially I had a DataContext similar to the following code
where I attempted to specify two tables using the same model:

<pre><code class="csharp">
public class TestDataContext : DataContext
{
    // Pass the connection string to the base class.
    public TestDataContext(string connectionString)
        : base(connectionString)
    {
    }
    public Table<TestModel> Table1;
    public Table<TestModel> Table2;
}

</code></pre>

The reason I had two conceptual lists is because I have one for recent releases
and one for storing all chapters of all the manga series. Since it is more
bandwidth efficient to only grab recent releases and grab all chapters as
needed, I had these two lists.

My solution to solving this issue is to have 1 table for both and I added
another field to indicate whether an entry is a recent chapter or not.

EricEJ has a nice [listing of 3rd party SQL CE tools][4] from which I found
[SQL CE Browser by Eric Willis][5]. Using SQL CE Browser, I was able to open up
my SDF file and see that LINQ to SQL only created 1 table for the two
Table<TestModel> properties I specified in my DataContext. That kind of
explains why when I queried for things in one I also got things in the other
because it's all stored to the same underlying table.

<br>

## **4. Background Transfer doesn't work from Background Agent code**

I wrote a helper class that dealt with Background Transfer Service (BTS) and
when I initially coded up the Background Agent code I naturally used the same
helper class. However, I was getting InvalidOperationException when trying to
add a new Background Transfer request. I believe the reason is due to
Background Agent being a Headless Host.

I used [ILSpy](http://wiki.sharpdevelop.net/ILSpy.ashx) and took a look at the
Microsoft.Phone.dll on my PC located at C:\\Program Files\\Reference
Assemblies\\Microsoft\\Framework\\Silverlight\\v4.0\\Profile\\WindowsPhone71\\Microsoft.Phone.dll

Turns out that the BackgroundTransferRequest.Submit() code checks for
IsHeadlessHost:

<pre><code class="csharp">
// Microsoft.Phone.BackgroundTransfer.BackgroundTransferRequest
[SecurityCritical]
internal void Submit()
{
    this.ThrowIfReadOnly();
    HostInfo hostInfo = new HostInfo();
    if (hostInfo.IsHeadlessHost)
    {
        throw new InvalidOperationException();
    }

</code></pre>

I ended up using WebClient in my Background Agent instead.

  [1]: /mangastream-for-windows-phone-7/
  [2]: http://msdn.microsoft.com/en-us/library/system.io.isolatedstorage.isolatedstoragesettings.save(v=VS.95).aspx
  [3]: http://msdn.microsoft.com/en-us/library/hh286408(v=vs.92).aspx
  [4]: http://erikej.blogspot.com/2009/04/sql-compact-3rd-party-tools.html
  [5]: http://notes.ericwillis.com/2009/12/sql-ce-browser-v-110/
