import time
import random

data = {
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

while True:
  k = random.choice( list( data.keys() ) )
  if k == "meter": v = str( randomMeter() )
  else: v = random.choice( list( data[k] ) )
  if k == "scale": v = str( random.randint(0,11) ) + " " + v
  print( k + ": " + v )
  time.sleep(1)
