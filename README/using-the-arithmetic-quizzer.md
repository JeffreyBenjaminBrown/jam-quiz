Notice that when you run `cabal repl` it gripes about a module that it can't find. I don't know what's going on there, but once GHCI starts you can import it with `import Quiz_58`, as illustrated below.


```
jeff@jbb-dell:jam-quiz$ cabal repl
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - haskell-0.1.0.0 (lib) (first run)
Preprocessing library for haskell-0.1.0.0..
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help

<no location info>: error:
    Could not find module ‘Quiz_58’
    It is not a module in the current program, or in any known package.
Loaded GHCi configuration from /home/jeff/code/music/jam-quiz/.ghci
[1 of 3] Compiling Formulas         ( haskell/Formulas.hs, interpreted )
[2 of 3] Compiling Paths_haskell    ( /home/jeff/code/music/jam-quiz/dist-newstyle/build/x86_64-linux/ghc-8.10.7/haskell-0.1.0.0/build/autogen/Paths_haskell.hs, interpreted )
[3 of 3] Compiling Quiz_58          ( haskell/Quiz_58.hs, interpreted )
Ok, three modules loaded.
> import Quiz_58
> quiz_sums 31 -- (This quizzes for 31 edo.)
What is -4 + -7 modulo 31?

20
What is -30 + -24 modulo 31?

8
What is -22 + 16 modulo 31?

25
What is -20 + 27 modulo 31?
^CInterrupted.
>
[0] 0:bash*
```
