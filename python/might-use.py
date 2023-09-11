# Unfinished. Not sure what I want this to do,
# but if I need conditions to hold on the random choices,
# this (via a list comprehension) is a more natural way to do it
# than rawing random numbers, testing it,
# discarding a result that fails and repeating the draw.
def quizTriads ( edo : int ):
  triads = [ (a,b,c)
             for a in range ( 0, edo )
             for b in range ( 0, edo )
             for c in range ( 0, edo )
             if a < b & b < c ]
