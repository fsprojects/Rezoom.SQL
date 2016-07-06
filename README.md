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
they must go through an approval process. Users can stamp documents
with one of three approval states:

```csharp
public enum ApprovalState
{
    Draft,
    ReadyForApproval,
    Approved,
}
```

We'll be keeping track of all the times a document is stamped, and who
did the stamping. Here's the database model we'll be using:

![Document has many Stamps which have a User][documentdbmodel]

And here's the interface we're dealing with for accessing the database:

```csharp
public interface IDocumentDatabase
{
    // Get one document by its ID.
    Document GetDocument(int documentId);
    // Get multiple documents by their IDs.
    List<Document> GetDocuments(List<int> documentIds);

    // Insert a new stamp and return its ID.
    int InsertStamp(Stamp stamp);
    // Insert multiple stamps and return their IDs.
    List<int> InsertStamps(List<Stamp> stamps);

    // Update the `LatestStampId` pointer of the document in question.
    void UpdateLatestStamp(int documentId, int latestStampId);
    // Bulk version of above; takes a list of documentId/latestStampId pairs.
    void UpdateLatestsStamps(List<KeyValuePair<int, int>> idPairs);
```

[cheetos]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/cheetos.gif

[bulkifymethod]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/add-list-method.gif

[documentdbmodel]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/document-db-model.gv.png
