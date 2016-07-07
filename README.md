# What is it?

  DataRes is a .NET library that lets you write data manipulation code
  the easy, inefficient way and have it executed like you wrote it the
  hard, efficient way.

  It's implemented in C# and can be used from any .NET language.
  However, it's especially well-suited to usage from F#.

  This document introduces DataRes to you, a curious C# developer.
  By reading it, you'll learn:

  * The motivation behind DataRes
  * How DataRes works (no magic, guaranteed)
  * How you can use DataRes in your next project

# What problem does it solve?

Has *this* ever happened to *you*?

![Infomercial disaster][cheetos]

Wait, no, sorry. Has **this** ever happened to **you**?

![Changing a method that returned a document by its ID to return multiple documents by multiple IDs][bulkifymethod]

Ok, I promise to lay off the gifs. The point is, these are examples of
the contortions we put our code through in order to avoid performance
pitfalls. There are many situations in programming where it is better
to do things in **batches** rather than **one at a time**. And while
we strive not to prematurely optimize, the performance difference here
is so great that it's often unacceptable to do things the slow way.

Here are some familiar cases where this is true:

* Better to make one HTTP API request for answers to 100 questions
  than 100 HTTP requests for one question apiece.

* Better to make one database call for 100 rows than 100 database
  calls for 1 row apiece.

* Better to open a file once to write 1000 lines to it, than to open
  it 1000 times to append one line at a time.

* Better to load required information about a user once for 100
  actions that user will undertake, than to load it redundantly for
  each action.

As the animations above illustrated, a common way to handle batching
is to do it explicitly. We can write methods that take a list of
`Foo`s instead of a single `Foo`, and return a list or dictionary of
results. However, this approach is error-prone and often hard to
follow, obscuring the simple sequence of steps that we're trying to
perform on each `Foo`.

Let's work through an example to get an in-depth understanding of the
problem.

## The document approval example

Suppose we're working on an application for collaboratively authoring
and organizing documents. These are **Very Important Documents**, so
they must go through an approval process. Users can *stamp* documents
with one of three states:

```csharp
public enum StampState
{
    Draft,
    ReadyForApproval,
    Approved,
}
```

The stamp state of a document isn't just a field that we update. We
need to keep track of all the times a document is stamped, and who did
the stamping. Here's the database model we'll be using:

![Document has many Stamps which each have a User][documentdbmodel]

**Figure 1:** *The database model for the document stamping example*

We're tasked with writing a method `void Approve(int approvingUserId,
int documentId)`, which will contain the logic to add an `Approved`
stamp. Here's the interface this method can use to access the
database.

```csharp
public interface IDatabase
{
    // Get a user by their ID.
    User GetUser(int userId);

    // Insert a new stamp and return its ID.
    int InsertStamp(Stamp stamp);

    // Update the `LatestStampId` pointer of the document in question.
    void UpdateLatestStamp(int documentId, int latestStampId);
}
```

And here's a first pass at the method's implementation.

```csharp

public void Approve(int approvingUserId, int documentId)
{
    // First, check the user's permissions.
    var user = Database.GetUser(approvingUserId);
    if (!user.IsApprover)
    {
        throw new SecurityException("User is not allowed to approve!");
    }

    // Next, create and insert the new stamp.
    var stampId = Database.InsertStamp(new Stamp
    {
        DocumentId = documentId,
        StampedByUserId = stampingUserId,
        StampedUtc = DateTime.UtcNow,
        State = StampState.Approved,
    });

    // Finally, update the document's `LatestStampId` to
    // point to the new stamp.
    Database.UpdateLatestStamp(documentId, stampId);
}

```

This is very clear and straightforward, and it works well for a while.
Then a new feature is added to the UI. Users can select multiple
documents and stamp them all as approved with a single click.
Initially, the logic is implemented by simply calling our method in a
loop, like this:

```csharp
foreach (var documentId in selectedDocumentIds)
{
    Approve(approvingUserId, documentId);
}
```

But of course, the accursed users select and stamp hundreds of
documents at a time, and there's a noticable lag when stamping those
large selections. How can we make this faster?

We can't get significant improvements in our little method's
performance without changing the interface. Perhaps we could add
caching for `Database.GetUser(stampingUserId)`, but for the insert and
update we're still going to be making two round trips to the database
for every document being approved.

To make fewer round trips, we could extend our database interface to
support doing those actions in bulk.

```csharp
public interface IBulkDatabase : IDatabase
{
    // Insert multiple stamps and return their IDs.
    List<int> InsertStamps(IEnumerable<Stamp> stamps);

    // Update the latest stamps of multiple documents.
    // Takes a set of DocumentId/LatestStampId pairs.
    void UpdateLatestsStamps(IEnumerable<KeyValuePair<int, int>> idPairs);
}
```

Now we can rewrite the approve method to work efficiently in bulk,
too.

```csharp

public void Approve(int approvingUserId, List<int> documentIds)
{
    // First, check the user's permissions.
    var user = Database.GetUser(approvingUserId);
    if (!user.IsApprover)
    {
        throw new SecurityException("User is not allowed to approve!");
    }

    // Next, create and insert new stamps for all the documents.
    var stamps = documentIds.Select(documentId => new Stamp
    {
        DocumentId = documentId,
        StampedByUserId = stampingUserId,
        StampedUtc = DateTime.UtcNow,
        State = StampState.Approved,
    });
    var stampIds = Database.InsertStamps(stamps);

    // Finally, update all the documents to point at the new stamps.
    var documentStampPairs = documentIds
        .Select((documentId, i) =>
            new KeyValuePair<int, int>(documentId, stampIds[i]));
    Database.UpdateLatestsStamps(documentStampPairs);
}

```


[cheetos]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/cheetos.gif

[bulkifymethod]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/add-list-method.gif

[documentdbmodel]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/document-db-model.gv.png
