# Re `quiz_number_language.py`

This is for quizzing myself on a system that allows me to state numbers from 0 to 23 extremely quickly, and relate them to the guitar fretboard.

See node `yeh45hgqGnZiBsiZ` in smsn-public for more detail.


# On (I guess?) `haskell/Formulas.hs`: Interpreting scale names

Scale names are printed preceded by an integer in 0 to 11, indicating the scale's root. It is a number of halfsteps, measured relative to something. It's up to you whether to interpret it as relative to a fixed pitch or relative to the previous scale's root.

The five scales that have two names are expressed using both names.

I ordinarily call 0 1 3 4 6 8 9 the loc b4b7 scale, but I realized that was ambiguous, because b7 ordinarily means 10 rather than 9. I call it that because the locrian scale already has a b7, so to flat the 7 in a locrian scale ought to mean to drop it from 10 to 9. But just to be explicit I called that one loc b4bb7.

I believe there are no other ambiguities.
