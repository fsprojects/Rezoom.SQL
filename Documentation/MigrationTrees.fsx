(**

# Migration trees

If you're working alone on a project, it usually makes sense to have a linear migration sequence. So next time you need
a migration, you'll write `V3.nextfeature.sql`, then `V4.etc.sql`, and so on.

However, if you're working on a team, strictly ordered migrations can be a real pain. Say for example you're working on
a feature where articles are tagged in categories like `f#` or `tutorial`, so you create `V3.tags.sql`.
But at the same time, some guy named Robert is trying to add a feature that lets users add articles to their favorites.
He doesn't know about your migration, so he names his `V3.favorites.sql`.

When you merge, you have two different V3 migrations. This is a problem! Now you'll need to get together and decide
which one should be V3 and which should be V4, and one of you will have to manually fix up the migration history on
your development database instance.

To avoid this situation, you can take advantage of the fact that most migrations just add things to the schema,
they don't remove things. This means they can run in any order, as long as their dependencies are satisfied.

Rezoom.SQL can identify these "non-destructive" migrations, and will allow you to define them as add-ons to the current
major version number instead of bumping to the next major version.

*)
