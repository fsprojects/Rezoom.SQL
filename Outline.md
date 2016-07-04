# What is it?

  Brief overview.

# What problem does DataRes solve?

  Describe the problem and provide some StampDocumentReviewed
  examples.

## What's wrong with the obvious solution?

   Describe the obvious solution and provide StampDocument examples.

   Say it sucks.

### It doesn't compose well.

    Show how calling the bulk methods from higher levels is ugly and
    doesn't take full advantage of the possiblities.

### It is hard to change to meet requirements.

    Show how adding a StampState parameter per-document complicates
    things.

# How does DataRes solve the problem?

  Go back to the code we'd like to write if performance was so good we
  didn't have to care.

  Show how it executes with round-trips marked.

  Show how it executes in bulk (loop).

  Show how it could be executed in bulk more efficiently.

## How could we run code like this?

   Introduce hypothetical interface for 
