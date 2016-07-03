# Intro

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

# Motivation

Has *this* ever happened to *you*?

![Infomercial disaster][cheetos]

Wait, no, sorry. Has **this** ever happened to **you**?

![Changing a method that returned a document by its ID to return multiple documents by multiple IDs][bulkifymethod]

Or maybe this:

![Changing a method to require caching on a permission check and move a SaveChanges() to the caller to support bulk updates if called in a loop][stampdocument]

Ok, I promise to lay off the gifs. The point is, these are examples of
the contortions we put our code through in order to avoid performance
pitfalls. The code we want to write to bake a cake --

> Please, no. This is going to be like those `public class Cat : Animal` examples of object oriented programming in CS101, right?
> Can we not do this?

Yeah, it is going to be a bit like that. I'm hoping that using an
arbitrary, unrealistic example like cakes will keep us from getting
bogged down in the details for now, though. We'll do a real world
example by the end of this. Anyway, here's how we'd bake a cake:

```csharp

public Cake BakeCake()
{
    Oven.PreheatOven(degreesF: 350);

    var bowl = new Bowl();
    using (var cupboard = Kitchen.OpenCupboard())
    using (var fridge = Kitchen.OpenFridge())
    {
        bowl.Mix(cupboard.GetCakeIngredients(forCakes: 1));
        bowl.Mix(fridge.GetCakeIngredients(forCakes: 1));
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
        bowl.Mix(cupboard.GetCakeIngredients(forCakes: count));
        bowl.Mix(fridge.GetCakeIngredients(forCakes: count));
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
little harder to figure out from reading the new code.

Also, since we didn't foresee this more complex version being
necessary, we wasted time writing the simple first version, which had
to be almost completely replaced.

Finally, our efforts to optimize this function are still limited to
its scope. What do I mean by that? Well, in order to optimize these
methods, we had to move the implementation logic to a higher scope.

Before, the actual logic was in the method isolated to a single cake.
No matter how well we wrote that method, we couldn't get the speed
improvements we needed, because that method has blinders on. It only
sees its own job; it can't know about all the other cakes that need to
be made. It's a factory worker on an assembly line, doing one thing
and doing it as well as it can.

(wait, doesn't that sound like an ideal we're supposed to strive for
in software architecture?)

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

> It doesn't seem so bad to me.

Ok, fair enough. So far, it seems manageable. Let's take a look at two
things that make this type of optimization painful.

## It doesn't compose well

Suppose we've got a higher level method to throw a birthday
celebration for somebody.

```csharp

public void Birthday(Person jollyGoodFellow)
{
    InstallDecorations(jollyGoodFellow.Home);
    var cake = BakeCake();
    for (var i = 0; i < jollyGoodFellow.Age; i++)
    {
        cake.AddCandle();
    }
    cake.LightCandles();
    SingHappyBirthdayTo(jollyGoodFellow);
    jollyGoodFellow.BlowCandles(cake);
    var slice = cake.CutSlice();
    jollyGoodFellow.Eat(slice);
}

```

Seem OK? What if we have multiple people with birthdays?

```csharp

foreach (var person in people)
{
    Birthday(person);
}

```

We might be baking quite a few cakes, but we're not taking advantage
of all that code we wrote to make cake-baking efficient. To do so,
we'd have to write something like this:

```csharp

public void Birthdays(List<Person> jollyGoodFellows)
{
    // Bake enough cakes for everybody.
    var cakes = BakeCakes(jollyGoodFellows.Count);
    var cakeIndex = 0;
    // Have a birthday for each person, giving them a cake from our pre-baked batch.
    foreach (var jollyGoodFellow in jollyGoodFellows)
    {
        InstallDecorations(jollyGoodFellow.Home);
        var cake = cakes[cakeIndex++];
        for (var i = 0; i < jollyGoodFellow.Age; i++)
        {
           cake.AddCandle();
        }
        cake.LightCandles();
        SingHappyBirthdayTo(jollyGoodFellow);
        jollyGoodFellow.BlowCandles(cake);
        var slice = cake.CutSlice();
        jollyGoodFellow.Eat(slice);
    }
}

```

And of course, that's assuming that only cake-baking can be batched.
If we also have more efficient bulk operations for
`InstallDecorations`, `LightCandles`, and so on, this could get even
uglier. The point is, the effort required to perform tasks in batches
keeps getting shoved upward to the caller, making every layer more
complicated, up until the point where performance is sufficient and we
can skip the optimization effort.

Where that point is depends on your use cases and requirements, which
we all know are established at the beginning of each project and
*never change thereafter*.

## It makes some simple changes annoyingly difficult

Customers are getting bored of all these plain cakes with chocolate frosting.
New requirements have come in. For each cake, users should be able to specify:

* Whether to include strawberry puree in the batter
* The flavor of frosting to use (chocolate or vanilla)

The frontend team will pass us this info as a `CakeParameters` object:

```csharp
public enum BatterType
{
    Plain,
    Strawberry,
}
public class CakeParameters
{
    public Frosting Frosting { get; set; }
    public BatterType Batter { get; set; }
}
```

In our original, one-cake-at-a-time code, this is trivial to handle:

```csharp
public Cake BakeCake(CakeParameters parameters)
{
    Oven.PreheatOven(degreesF: 350);

    var bowl = new Bowl();
    using (var cupboard = Kitchen.OpenCupboard())
    using (var fridge = Kitchen.OpenFridge())
    {
        bowl.Mix(cupboard.GetCakeIngredients(forCakes: 1));
        bowl.Mix(fridge.GetCakeIngredients(forCakes: 1));

        // Add strawberry puree to the batter if desired.         // NEW CODE
        if (parameters.Batter == BatterType.Strawberry)           // ...
        {                                                         // ...
            var puree = Mixer.Puree(fridge.GetStrawberries(6));   // ...
            bowl.Mix(puree);                                      // ...
        }                                                         // ...
    }

    var pan = new Pan();
    bowl.PourInto(pan);
    var baked = Oven.BakePan(TimeSpan.FromMinutes(30), pan);
    using (var cupboard = Kitchen.OpenCupboard())
    {
        // Use the desired flavor of frosting.                     // NEW CODE
        var frosting = cupboard.GetFrosting(parameters.Frosting);  // ...
        frosting.ApplyTo(baked);
    }
    return new Cake(baked);
}
```

But in the bulk version... horror of horrors! Mixing one big bowl of
batter ain't gonna cut it anymore -- not if some cakes need
strawberries and others don't. We have to start grouping by batter and
frosting type to batch things effectively.

```csharp

private List<Cake> BakeCakes(List<CakeParameters> parameters)
{
    Oven.PreheatOven(degreesF: 350);

    // Mix separate bowls for each type of batter.

    Dictionary<BatterType, Bowl> batterBowls;
    using (var cupboard = Kitchen.OpenCupboard())
    using (var fridge = Kitchen.OpenFridge())
    {
        var batterGroups = parameters.GroupBy(p => p.Batter);
        batterBowls =
            batterGroups.Select(group =>
            {
                var bowl = new Bowl();
                bowl.Mix(cupboard.GetCakeIngredients(forCakes: count);
                bowl.Mix(fridge.GetCakeIngredients(forCakes: count);
                if (group.Key == BatterType.Strawberry)
                {
                    var puree = Mixer.Puree(fridge.GetStrawberries(6));
                    bowl.Mix(puree);
                }
                return new { Batter = group.Key, Bowl = bowl };
            })
            .ToDictionary(g => g.Batter);
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
            var bowl = bowls[parameters[j].Batter]; // Look up the bowl by the batter type.
            bowl.PourInto(pan);
            batchPans.Add(pan);
        }
        allBaked.AddRange(Oven.BakePans(TimeSpan.FromMinutes(30), batchPans));
    }
    // Frost all our cakes -- we only need to get the frosting once for this.
    using (var cupboard = Kitchen.OpenCupboard())
    {
        var frostings =
            parameters
                .GroupBy(p => p.Frosting)
                .ToDictionary(g => g.Key, g => cupboard.GetFrosting(g.Key));
        for (var i = 0; i < parameters.Count; i++)
        {
            var baked = allBaked[i];
            var pars = parameters[i];
            frosting[pars.Frosting].ApplyTo(baked);
        }
    }
    return allBaked.Select(b => new Cake(b)).ToList();
}

```


[cheetos]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/cheetos.gif

[bulkifymethod]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/add-list-method.gif

[stampdocument]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/stamp-document.gif


# How it works
# How you can use it
