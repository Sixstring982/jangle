**_Jangle_**

# Summary

_Jangle_ is a Haskell starter project for Advent of Code.

## Feature overview

- Automatic downloading + caching of puzzle inputs
- CLI for running puzzles
- Automatic puzzle detection + registration

# Quick-start

## Project setup

First, clone or fork this repo and change directories into the repo.

> [!NOTE]  
> If you're a nix user you can skip the following steps and instead run:
> `$ nix develop`

Use Cabal to build and run this project.

## Configure authentication

_Jangle_ needs a session token from `adventofcode.com` in order to download
your puzzle inputs and submit your answers.

Start by logging in to `adventofcode.com`, then browsing to one of the puzzle
dashboards (e.g. https://adventofcode.com/2015).

Open your developer tools and reload the page. This should issue a GET request,
e.g. `GET https://adventofcode.com/2015`. Look for the `Cookie: ` request
header, which should contain a value that looks like (where `x` is a hex value):

```
session=5xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx5
```

This value is your session token. You'll need to store it as an environment
variable called `AUTH_TOKEN`. One convenient way of doing this is to use a tool
like [direnv](https://direnv.net/), e.g.:

`.envrc`:

```shell
export AUTH_TOKEN="5xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx5"
```

> [!TIP]
> If you don't want to configure authentication, you will manually need to create your input directories and files.
> This can be done by creating the following directory structure from the project root:
> ```shell 
> $ mkdir inputs/{year}/{day}.txt
> ```
> where Day 01 of 2023 would look like:
> ```shell 
> $ mkdir inputs/2023/01.txt
> ```


## Working on problems

Each problem module needs to expose two functions -- `runProblemOne` and
`runProblemTwo` -- both of type `Text -> Text`.

Problem module names be of the format "Problem_<year>_<day>.hs" -- e.g.
"Problem_2023_01.hs". The numbers in the module name are parsed at compile time,
and registered in the `Problems.All` module.

### Running problems

Once you've added your problem, you can test your solution by running it with 
`cabal`.  This will output your answer to the terminal:

```shell
$ cabal run jangle-exe -- \
  --year=2023 \
  --day=1 \
  --part=1

  # output 
  your_answer
```

# Tips and tricks

## Testing against other inputs

Advent of Code typically provides smaller inputs, in order to check that your
code works. I tend to allow _Jangle_ to download the puzzle input first, then
I can replace the puzzle input with whatever input I'd like to test.

I can then revert back to the official puzzle input by deleting the file
(_Jangle_ will download a fresh copy when I run it again).

