# Intro

  DataRes is a .NET library that lets you write data manipulation code
  the easy, inefficient way and have it executed like you wrote it the
  hard, efficient way.

  It's implemented in C# and can be used from any .NET language.
  However, it's especially well-suited usage from F#.

  This document introduces DataRes to you, a curious C# developer.
  By reading it, you'll learn:

  * The motivation behind DataRes
  * How DataRes works (no magic, guaranteed)
  * How you can use DataRes in your next project

# Motivation

Has *this* ever happened to *you*?

![Infomercial disaster][cheetos]

Wait, no, sorry. Has **this** ever happened to **you**?

![Changing a method that returned a document by its ID to return multiple documents by multiple IDs][bulkifymethod]

Or maybe this:

![Changing a method to require caching on a permission check and move a SaveChanges() to the caller to support bulk updates if called in a loop][stampdocument]

Ok, I promise to lay off the gifs. The point is, these are examples of
the contortions we put our code through in order to avoid performance
pitfalls. The code we want to write to bake a cake looks like this:

```csharp

public Cake BakeCake()
{
    Oven.PreheatOven(degreesF: 350);

    var bowl = new Bowl();
    using (var cupboard = Kitchen.OpenCupboard())
    using (var fridge = Kitchen.OpenFridge())
    {
        var sugar = cupboard.GetSugar(cups: 1);
        var butter = fridge.GetButter(cups: 0.5);

        bowl.Mix(sugar, butter);

        var eggs = fridge.GetEggs(2);
        var milk = fridge.GetMilk(cups: 0.5);
        var flour = cupboard.GetFlour(cups: 1.5);
        var bakingPowder = cupboard.GetBakingPowder(tsp: 1.75);

        bowl.Mix(eggs, milk, flour, bakingPowder);
    }

    var pan = new Pan();
    bowl.PourInto(pan);
    var baked = Oven.BakePan(TimeSpan.FromMinutes(30), pan);
    using (var cupboard = Kitchen.OpenCupboard())
    {
        var frosting = cupboard.GetFrosting(Frosting.Chocolate);
        frosting.ApplyTo(baked);
    }
    return new Cake(baked);
}

public List<Cake> BakeCakes(int count)
{
    return Enumerable.Range(0, count)
        .Select(_ => BakeCake())
        .ToList();
}

```

This works great for one or two cakes. But of course, the accursed
users try to bake hundreds of cakes at a time. The system crawls, and
the users complain. So we end up having to go back and rewrite it to
this:

```csharp

public Cake BakeCake()
{
    return BakeCakes(1).Single();
}

public List<Cake> BakeCakes(int count)
{
    Oven.PreheatOven(degreesF: 350);

    // Mix enough ingredients for everything into one big bowl.
    var bowl = new Bowl();
    using (var cupboard = Kitchen.OpenCupboard())
    using (var fridge = Kitchen.OpenFridge())
    {
        var sugar = cupboard.GetSugar(cups: 1 * count);
        var butter = fridge.GetButter(cups: 0.5 * count);

        bowl.Mix(sugar, butter);

        var eggs = fridge.GetEggs(2 * count);
        var milk = fridge.GetMilk(cups: 0.5 * count);
        var flour = cupboard.GetFlour(cups: 1.5 * count);
        var bakingPowder = cupboard.GetBakingPowder(tsp: 1.75 * count);

        bowl.Mix(eggs, milk, flour, bakingPowder);
    }
    // We can bake 4 pans at a time in our big oven.
    const int batchSize = 4;
    var allBaked = new List<BakedBatter>();
    for (var i = 0; i < count; i += batchSize)
    {
        var batchPans = new List<Pan>();
        // Up to 4 pans, unless that would overrun the overall count...
        for (var j = i; j < count && j < i + batchSize; j++)
        {
            var pan = new Pan();
            bowl.PourInto(pan);
            batchPans.Add(pan);
        }
        allBaked.AddRange(Oven.BakePans(TimeSpan.FromMinutes(30), batchPans));
    }
    // Frost all our cakes -- we only need to get the frosting once for this.
    using (var cupboard = Kitchen.OpenCupboard())
    {
        var frosting = cupboard.GetFrosting(Frosting.Chocolate);
        foreach (var baked in allBaked)
        {
            frosting.ApplyTo(baked);
        }
    }
    return allBaked.Select(b => new Cake(b)).ToList();
}

```

We solved a performance problem at a cost to maintainability. The
simple steps required to bake a cake haven't changed, but they're a
little harder to figure out from reading the new code. There are also
traps for future changes: if you want to add some vanilla extract to
the recipe, don't forget to multiply by the `count` of cakes!

Also, since we didn't foresee this more complex version being
necessary, we wasted time writing the simple first version, which had
to be almost completely rewritten.

Finally, our efforts to optimize this function are still limited to
its scope. What do I mean by that? Well, in order to optimize these
methods, we had to move the implementation logic to a higher scope.

Before, the actual logic was in the method isolated to a single cake.
No matter how well we wrote that method, we couldn't get the speed
improvements we needed, because that method has blinders on. It only
sees its own job; it can't know about all the other cakes that need to
be made. It's a factory worker on an assembly line, doing one thing
and doing it as well as it can (doesn't that sound like an ideal we're
supposed to strive for in software architecture?).

In the new version, we moved the logic up to a level at which we know
how many cakes need to be made. You might say it's not a factory
worker on an assembly line anymore, it's the foreman, overseeing the
whole cake-making division. But that analogy doesn't quite work,
because it has to handle all the details too. It has to reach down and
actually do the work of assembling the cakes, and it's harder to do
from all the way up there.

So, the logic is at a higher scope, and it's got more visibility. But
it's still limited to that scope. `BakeCakes()` seems pretty smart when
we're making 200 cakes, but it seems pretty dumb when we need a cake,
a steak, and a bowl of cereal -- even though those tasks could likely
share resources like opening the fridge, getting the milk out, and so
on.

[cheetos]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/cheetos.gif

[bulkifymethod]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/add-list-method.gif

[stampdocument]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/stamp-document.gif


# How it works
# How you can use it
