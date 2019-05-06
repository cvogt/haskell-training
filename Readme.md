Demo project for Haskell training

There are many ways to learn Haskell. We take a very hands-on, practical approach aiming for basic reading and writing fluency. Usefulness is one of the biggest drivers in learning. So let's build something useful I always wanted to have. We can work on it together during the training and maybe flesh it out more afterwards.

Haskell is a great language for all programming tasks. Less well known is that it is also great for simple bash-like scripting. So let's build a command line client for circleci, starting with showing the last few workflow runs for the current git branch and allowing to trigger new ones. Maybe after the course we can even make it display the workflow graph.

The `projected-outcome` branch in this repository contains one possible outcome we could reach together after a few sessions.

There is no api to fetch information about entire workflows. The following url allows to fetch the most recent builds (as in steps in a workflow), which come with a workflow id we can group them by.

https://circleci.com/gh/<user>/workflows/<project>/tree/<branch-name>?limit=100[&offset=...]&circlecitoken=...

We can trigger workflows using an http post as described here: https://circleci.com/docs/api/v1-reference/#new-project-build and `curl -X POST --header "Content-Type: application/json" -d '{"branch":"staging"}' https://circleci.com/api/v1.1/project/github/<user>/<project>/build?circle-token=...`

In order to make this happen in Haskell, we'll have to:
- learn how to learn in Haskell land
- use Haskells IO Monad
- take arguments from the command line
- read environment variables
- deal with Haskell's different string types
- write Haskell type signatures
- write Haskell data types
- interface with git to find out the current branch of a repo, I suggest using a process call
- use an http client library
- parse json into haskell data types
- process data sequences to find what we need
- generate strings from the relevant data
- write to standard output
- learn about some cases where Haskell is unsafe
