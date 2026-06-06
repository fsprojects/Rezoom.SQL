# AI Agent Policy for Rezoom and Rezoom.SQL projects

The Rezoom and Rezoom.SQL library code shall be 100% human-written.

This is not a statement about the quality of AI code.

AI code is getting better all the time, but there is no level of quality that would alter the policy for this specific project.

It is an explicit goal of this project to remain a human-designed, human-written creative work.

Many hours of my time have been spent on this and my fingerprints are all over it.

To wipe them away with an onslaught of AI code now would be like taking a CNC machine to a hand-carved relief.

The same rule applies just as strongly for the documentation.

Humans who take the time to read documentation deserve another human taking the time to write it.

# What AI agents *can* be used for

Agents are extremely useful!

AI agents may be used to *critique* code and docs, give feedback, bounce ideas around, research questions, and to try to repro/hunt down bugs.

They can be used to create build / packaging / CI scripts. They can be used to assist with infrastructure housekeeping e.g. "Update all the project files to add a .NET 12.0 packaging target".

I *do* allow AI-written unit tests and sample projects (TypeProviderUsers) that exercise the code. This saves tons of human effort in an area that is more chore than creativity.

This includes generating test fixture data.

I do not consider this a compromise of the policy. The tests are "just" tools we use to confirm the library's correctness.

However, the user's prompt to create tests should be fairly precise on what invariants they want to test for. Not a lazy open-ended "add tests for the new feature".

This should hopefully keep each test meaningful and not clutter with a bunch of do-nothing-important tests.

AI-written tests should keep comments to a minimum. The test name in F# can be nearly a full sentence; this is often adequate for self-documentation of the
test's purpose and does not need a big redundant comment block.

If a comment *is* deemed valuable, AI agents should remember:

* DO NOT over-explain. Less than 20 words is a good goal for a comment.
* DO NOT use Unicode characters. Keep it to 7-bit ASCII!
* DO NOT refer to chat-local ephemeral context like "regression for bug found in pass 2" where a future reader will go "WTF are you talking about?"

In the absence of specific requests like "write a test for xyz", agents should default to discussion mode.

# Projects where AI is NOT allowed to write nor directly suggest "paste this in" code

* Rezoom
* Rezoom.SQL.Annotations
* Rezoom.SQL.Mapping
* Rezoom.SQL.Compiler
* Rezoom.SQL.Provider

# Projects where AI is allowed to write code directly, at the user's request

* Rezoom.Test
* Rezoom.SQL.Test
* Rezoom.SQL.Test.UserTypes
* Rezoom.SQL.Test
* Rezoom.SQL.Provider.Test
* The Rezoom.SQL e2e test/sample projects under the TypeProviderUsers/ folder

# MD file litter

Agents should not litter the repository with their own .md files / working notes.

If the programmer requests to save a conceptual note or roadmap of something they are working on, and that file will be
agent-generated, it should be stored above the repository so it doesn't end up littered into git history by mistake.










