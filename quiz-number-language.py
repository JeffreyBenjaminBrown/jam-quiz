# Write two dictionaries, one for 0-9, the other for 0-6.
# Pick something from either, and generate a (word,number) pair.
# Show one, ask for the other, then show the other after a pause.

import time
import random
from typing import Tuple, List


### ###
### ### The interface
### ###

def numPrompt_forWord ():
  ofe = oneFromEach()
  print("How would you say ", nums( ofe ), "?")
  input()
  print( "Answer: " + word( ofe ) + "\n" )
  numPrompt_forWord() # recurse

def wordPrompt_forNum ():
  ofe = oneFromEach()
  print("What is the meaning of ", word( ofe ), "?")
  input()
  print( "Answer: ", nums( ofe ), "\n" )
  wordPrompt_forNum() # recurse

def guitarPrompt_forNum ( guitarSharp_24edo ):
  s = randomGuitarString()
  f = randomFret()
  n = stringAndFret_to24edo_aboveA ( s, f, guitarSharp_24edo )
  pic = stringAndFret_toPic ( s, f )
  print( "Guitar is sharp: " + str(guitarSharp_24edo/2) + "\\12" )
  print( "How many steps of 12edo above the note A is the X?" )
  for row in pic: print(row)
  input()
  print( "Answer: ", n/2, "\n" )
  guitarPrompt_forNum ( guitarSharp_24edo ) # recurse

def numPrompt_forGuitar ( guitarSharp_24edo ):
  s = randomGuitarString()
  f = randomFret()
  n = stringAndFret_to24edo_aboveA ( s, f, guitarSharp_24edo )
  pic = stringAndFret_toPic ( s, f )
  print( "Guitar is sharp: " + str(guitarSharp_24edo/2) + "\\12" )
  print( "Where on string " + str(s[0])
         + " is the note " + str(n/2) + "\\12 above A?" )
  input()
  print( "Answer:\n" )
  for row in pic: print(row)
  numPrompt_forGuitar ( guitarSharp_24edo ) # recurse


### ###
### ### The internals
### ###

###
### Speaking (series of) numbers quickly
###

vowels = [ ("oh"     , 0)  ,
           ("uh"     , 1)  ,
           ("oo"     , 2)  ,
           ("ee"     , 3)  ,
           ("aw<On>" , 4)  ,
           ("l"      , 5)  ,
           ("i"      , 6)  ,
           ("eh"     , 7)  ,
           ("r"      , 8)  ,
           ("n"      , 9)  ,
           ("a<At>"  , 10) ,
           ("ö"      , 11) ]

firstThreeConsonants = [ ("_", 0) ,
                         ("p", 1) ,
                         ("k", 2) ,
                         ("t", 3) ]

# All consonants should be unvoiced.
lastThreeConsonants = [ ("_",  0) ,
                        ("f",  1) ,
                        ("sh", 2) ,
                        ("s",  3) ]

def oneFromEach () -> Tuple[ Tuple[ str, int ],
                             Tuple[ str, int ],
                             Tuple[ str, int ] ]:
  a = random.choice( firstThreeConsonants )
  b = random.choice( vowels )
  c = random.choice( lastThreeConsonants )
  return (a,b,c)

def word (a : Tuple[ Tuple[ str, int ],
                     Tuple[ str, int ],
                     Tuple[ str, int ] ]
          ) -> str:
  return " ".join( [ a[0] [0],
                     a[1] [0],
                     a[2] [0] ] )

def nums (a : Tuple[ Tuple[ str, int ],
                     Tuple[ str, int ],
                     Tuple[ str, int ] ]
          ) -> Tuple[ int, int, int ]:
  return ( a[0] [1] ,
           a[1] [1] ,
           a[2] [1] )


###
### The guitar
###

# The 24edo values are written as 2*x
# because it's easier for me to think in 12edo.
guitarStrings_24edo = [
  (1, 2 * 0 ) , # string 1 = E
  (2, 2 * 7 ) , # string 2 = B = 7 12edo steps above E
  (3, 2 * 3 ) , # string 3 = G = 3 12edo steps above E
  (4, 2 * 10) , # etc.
  (5, 2 * 5 ) ,
  (6, 2 * 0 ) ]

def randomGuitarString() -> Tuple[ int, int ]:
  return random.choice( guitarStrings_24edo )

def randomFret() -> int:
  return random.choice( list( range( 0, 10 ) ) )

def stringAndFret_to24edo_aboveA (
    guitarString : Tuple[ int, int ],
    fret : int,
    guitarSharp_24edo : float = 0
) -> int:
  openNoteRelativeToE = guitarString[1]
  return ( ( openNoteRelativeToE + fret
             + guitarSharp_24edo + 14 ) # +14 because E is 14\24 above A
           % 24 )

def test_stringAndFret_to24edo_aboveA():
  assert stringAndFret_to24edo_aboveA ( (1,0*2), 0 )     == 0
  assert stringAndFret_to24edo_aboveA ( (2,7*2), 0 )     == 2 * 7
  assert stringAndFret_to24edo_aboveA ( (2,7*2), 1 )     == 2 * 7.5
  assert stringAndFret_to24edo_aboveA ( (2,7*2), 1, -1 ) == 2 * 7

def stringAndFret_toPic ( guitarString : Tuple[ int, int],
                          fret : int
                         ) -> List[ str ]:
  row = "| : | : | " # Each solid bar is an even-numbered fret.
                     # The first solid bar represents the nut.
  strings = [ list( row ) for i in range(0,6) ]
  strings [ guitarString[0]-1 ] [ fret ] = "X"
    # The -1 is to handle Python's 0-indexing of lists.
    # (It might be nice if guitarists called the 1st string the 0th,
    # but they don't, and I'm adhering to their convention
    # in the definition of `guitarStrings_24edo`.)
  return [ "".join( s ) for s in strings ]
