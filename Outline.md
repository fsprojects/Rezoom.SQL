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

   Introduce hypothetical interface for stepping through task.

## Ok, but how would we implement the interface?

   Show example of how it could be written and used in C#.

## This seems like a pain to use. Is there a better way?

   Show how F# makes it better.

# How can I use DataRes?

  Explain that as an example, we'll be using a simple web API.

## The IP-API.com geolocation API

   Go over the API, describing first the single-address version, then
   the batch version.

## Implementing a C# wrapper for the API

   Go over how we'll write the core part of it (without any DataRes
   dependency).

## Integrating with DataRes

   Start with the Errand implementation, leaving the Prepare()
   method blank for now.

   Then cover the step-local batch class.

   Finally, complete the Errand implementation and add a static
   wrapper method converting it to a Plan.

## Using the integration

   Show some F# and C# examples of code using the API.