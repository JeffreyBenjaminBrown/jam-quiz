# import python.random_metronomic_instructions as rmi

import time
import random


dictats = {
  "scale" : [
    "maj", "dor", "phr", "lyd", "mix", "aeol", "loc",
    "dor #7 = maj b3", "phr #6 = dor b2", "lyd #5", "mix #4 = lyd b7", "aeol #3 = mix b6", "loc #2 = aeol b5", "loc b4",
    "aeol #7", "loc #6", "maj #5", "dor #4", "phr #3", "lyd #2", "loc b4bb7",
    "maj b6", "dor b5", "phr b4", "lyd b3", "mix b2", "lyd #2#5", "loc b7",
    "whole", "dim up", "dim down", "aug up", "aug down"
  ],
  "pitches in use" : ["dense", "sparse"],
  "dynamics" : ["loud(er)", "soft(er)", "more (complex)", "less (complex)"],
  "rhythm" : ["fast(er)", "slow(er)", "more (complex)", "less (complex)"],
  "tempo" : ["accel", "ritard"],
  "meter" : () # note: weird
}

def randomMeter():
  length = random.choice([1,2,3,4])
  parts = []
  if length < 2: parts.append( random.choice([3,4,5,6,7]) )
  else:
    for i in range(length):
      parts.append( random.choice( [2,3,4] ) )
  return parts

def dictator ():
  while True:
    k = random.choice( list( dictats.keys() ) )
    if k == "meter": v = str( randomMeter() )
    else: v = random.choice( list( dictats[k] ) )
    if k == "scale": v = str( random.randint(0,11) ) + " " + v
    print( k + ": " + v )
    time.sleep(1)

# TODO : Should
def quizOutside ( edo : int ):
  pass

def quizPairs ( edo : int ):
  a = random.randint ( 1 , edo-1 )
  b = random.randint ( 1 , edo-1 )
  c = random.randint ( 0, 1 )
  if c > 0:
    (q, a) = (str(a) + " + " + str(b), (a+b) % edo)
  else:
    (q, a) = (str(a) + " - " + str(b), (a-b) % edo)
  print( q, "?" )
  input()
  print( str(a) )
  quizPairs(edo)

# TODO : Unfinished. Not sure what I want this to do,
# but if I need conditions to hold on the random choices,
# this (via a list comprehension) is a more natural way to do it
# than trying random stuff and regenerating it if it fails.
def quizTriads ( edo : int ):
  triads = [ (a,b,c)
             for a in range ( 0, edo )
             for b in range ( 0, edo )
             for c in range ( 0, edo )
             if a < b & b < c ]
