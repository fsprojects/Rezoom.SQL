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

Or maybe this:

![Changing a method to require caching on a permission check and move a SaveChanges() to the caller to support bulk updates if called in a loop][stampdocument]

Ok, I promise to lay off the gifs. The point is, these are examples of
the contortions we put our code through in order to avoid performance
pitfalls. There are many situations in programming where it is better
to do things in **batches** rather than **one at a time**. And while
we strive not to prematurely optimize, the performance difference here
is so great that it's often unacceptable to do things the slow way.

Here are some familiar examples:

* Better to make one HTTP API request for answers to 100 questions
  than 100 HTTP requests for one question apiece.

* Better to make one database call for 100 rows than 100 database
  calls for 1 row apiece.

* Better to open a file once to write 1000 lines to it, than to open
  it 1000 times to append one line at a time.

A related problem is that of shared dependencies. To call `void
MassageFoo(int userId, int fooId)` we need to check the user's
permissions. After all, their administrator may not have granted them
massaging privileges.

```csharp
void MassageFoo(int userId, int fooId)
{
    var permissions = PermissionStore.GetPermissions(userId);
    if (!permissions.CanMassage)
        throw new SecurityException("Stop, criminal!");

    ... // massage the Foo
}
```

But massaging a Foo doesn't change those permissions, so if we're
going to massage 100 Foos, it's unnecessary to keep re-checking the
permissions in each call.

```csharp
foreach (var fooId in fooIds)
{
    // oh no, so many redundant permission checks!
    MassageFoo(Session.CurrentUserId, fooId);
}
```

To fix this, we might mangle our API to make it the caller's
responsibility to load permissions for the user and pass them in.

```csharp
                            // | pass permissions |
                            // V                  V
void MassageFoo(int userId, Permissions permissions, int fooId)
{
    ... // massage the Foo
}
```

```csharp
var permissions = PermissionStore.GetPermissions(Session.CurrentUserId);
foreach (var fooId in fooIds)
{
    MassageFoo(Session.CurrentUserId, permissions, fooId);
}
```

This has the issue of leaving it up to the caller to give us the right
`Permissions` object. This is our internal API, so we're not concerned
about outside attackers, but a misguided caller could still give us
`Permissions` for a completely different user.

Preferably, we might add a caching layer around the permission-getting
logic, so that we can write the below and know that only the first
permission check will hit the database, while the others will be
pulled from the cache.

```csharp
void MassageFoo(int userId, int fooId)
{
                      // | use a cached copy if present |
                      // V                              V
    var permissions = PermissionCache.GetPermissions(userId);
    if (!permissions.CanMassage)
        throw new SecurityException("Stop, criminal!");

    ... // massage the Foo
}
```

Of course, it takes extra effort to write that caching layer, and it
can be a source of bugs -- especially if we forget to invalidate the
cache when a user's permissions change.

> There are only two hard things in Computer Science: cache
> invalidation, naming things, and off-by-one errors.
> -- <cite>Phil Karlton/Leon Bambrick</cite>

So, we have these two problems:

* How do we handle APIs that are best accessed in bulk?

* How do we handle resources that only need to be (expensively)
  retrieved once to work with *n* entities?

## What's wrong with the obvious solution?

A temptingly simple answer is this: always expose *our own* APIs in
"bulk" form!

Instead of:

```csharp
void MassageFoo(int userId, int fooId)
{
    ... // check user permissions

    ... // massage the Foo
}
```

We'll have:

```csharp
void MassageFoos(int userId, List<int> fooIds)
{
    ... // check user permissions

    ... // massage *all* the Foos!
}
```

This seems to solve both our problems. We can make a single permission
check for any number of foos without resorting to a caching layer, and
we can call any batch APIs needed to massage multiple Foos
efficiently.

However, having used this approach in real projects, I find it shows
ugly cracks in the harsh light of day. Here's why.

### It doesn't compose well.

`MassageFoos()` can be more efficient than `MassageFoo()` because it
micro-manages low-level work from a slightly higher level. The single
`MassageFoo()` doesn't know -- *can't* know -- that we are dealing
with 100 Foos at a time. It only knows enough to massage the one Foo
that it's working on. It is blind to the world around it.

Meanwhile, `MassageFoos()` knows exactly how much Foo-massaging work
must be done, and can plan accordingly. It knows it only needs to
check permissions once. It even knows that it can run bulk `SELECT`
statements on the database to load the Foos to be massaged, and so on.

This makes `MassageFoos()` seem very smart in its little domain. But
it's far from the highest level in your application, and its capacity
for intelligence ends here. Just one layer up, I might want to write a
method like this:

```csharp
void DoStuff(int userId, List<int> fooIds, List<int> barIds)
{
    BrushFoos(userId, fooIds);
    MassageFoos(userId, fooIds);
    MassageBars(userId, barIds);
}
```

`MassageFoos(userId, fooIds)` and `MassageBars(userId, barIds)` both
need to check that the user's permissions, but can't share that check.
Without a caching layer, they'll make two checks, one redundant.

`BrushFoos(userId, fooIds)` needs to load the same Foos from the
database as `MassageFoos(userId, fooIds)`, but again, they don't know
that. They'll blindy make the same query in two round-trips to the
database.

So, the performance improvements only chain up as high as we're
willing to manage low level details. Usually, this is sufficient and
we can eat the cost of inefficiency after a certain point -- after
all, at least this is example only making two permission checks
instead of *n* checks.

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

    // Mix up the batter
    var bowl = new Bowl();
    using (var cupboard = Kitchen.OpenCupboard())
    using (var fridge = Kitchen.OpenFridge())
    {
        bowl.Mix(cupboard.GetCakeIngredients(forCakes: 1));
        bowl.Mix(fridge.GetCakeIngredients(forCakes: 1));
    }

    // Put the batter in a pan
    var pan = new Pan();
    bowl.PourInto(pan);

    // Bake it
    var baked = Oven.BakePan(TimeSpan.FromMinutes(30), pan);

    // Frost it
    using (var cupboard = Kitchen.OpenCupboard())
    {
        var frosting = cupboard.GetFrosting(Frosting.Chocolate);
        frosting.ApplyTo(baked);
    }

    // And we're done!
    return new Cake(baked);
}

public List<Cake> BakeCakes(int count)
{
    // We'll bake as many cakes as you need
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

## Manual batching doesn't compose well

Suppose we've got a higher level method to throw a birthday
celebration for somebody.

```csharp

public void Birthday(Person jollyGoodFellow)
{
    InstallDecorations(jollyGoodFellow.Home);
    var cake = BakeCake();
    // Add enough candles for your age
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

## Manual batching makes simple changes annoyingly difficult

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

So, it seems that we are faced with a choice between simple inefficient code, and complex efficient code.
Worse, if we choose the latter, the complexity will bubble up through the layers of our application.

[cheetos]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/cheetos.gif

[bulkifymethod]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/add-list-method.gif

[stampdocument]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/stamp-document.gif

# How does DataRes solve the problem?

Let's return to the code that we want to write: the simple code that
works with one cake (or document, or widget, or account) at a time.

What does this code actually do?

![Linear execution with spots where we make round-trips to the cupboard, fridge, etc.][linearexecsingle]

And if we run it in a `foreach` loop, we go through all the steps in order _n_ times.

![Linear execution repeated three times][linearexecmulti]

The inefficiency is clear. What we want is to be able to execute it more like this:

[linearexecsingle]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/linear-exec-single.gv.png
[linearexecmulti]: https://raw.githubusercontent.com/rspeele/Data.Resumption/master/Documentation/resources/linear-exec-multi.gv.png

# How you can use it
