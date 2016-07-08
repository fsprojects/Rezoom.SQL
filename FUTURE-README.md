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
results.

```csharp
public Foo Combobulate(Bar bar);
```

*... becomes ...*

```csharp
public List<Foo> Combobulate(List<Bar> bars);
```

*... or perhaps ...*

```csharp
public Dictionary<Bar, Foo> Combobulate(List<Bar> bars);
```

However, the code implementing these batch methods is more error-prone
and harder to follow, often obscuring the simple sequence of steps
that we're trying to perform on each `Bar`. The method will be
sprinkled with extra code to correlate the `Foo`s to the `Bar`s and
various intermediate values along the way.

It also composes poorly, since for the method's caller to make ideal
use of it, they must also expose a batch API (and go through the same
contortions).

# How does DataRes solve the problem?

The code we would like to write works with one entity at a time. It
can't batch its expensive operations because it doesn't know that
there are other entities being worked on in the same `for` loop at a
higher layer of your application.





[cheetos]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/cheetos.gif

[bulkifymethod]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/add-list-method.gif

