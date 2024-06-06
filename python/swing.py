from   dataclasses import dataclass
import pygame
import random
from   time import sleep


dirt_samples : str = "/home/jeff/.local/share/SuperCollider/downloaded-quarks/Dirt-Samples"

pygame.mixer.init()

hat   = pygame.mixer.Sound (
  "/".join ( [ dirt_samples,
               "hc/001_VoodooHihat.wav" ] ) )
snare = pygame.mixer.Sound (
  "/".join ( [ dirt_samples,
               "sn/ST0T0S3.wav" ] ) )

@dataclass
class Question:
  after_snare  : float # A proportion.
                       # Classic triplets swing = 0.66.

  after_hat    : float # A proportion.
                       # Classic triplets swing = 0.33.

  # after_snare and after_hat will always sum to 1,
  # so after_hat could be inferred always rather than recorded,
  # but this is simpler.

  bar_duration : float # measured in seconds

# PITFALL: Global variable.
# I wouldn't need this if I could
# get multiprocessing to work with pygame;
# see the files called
#   swing_multiproc_*_fail.py
Q = Question ( 1,2,3 )

def play_forever ( q : Question ):
  after_snare = q.bar_duration * q.after_snare
  after_hat   = q.bar_duration * q.after_hat
  while True:
    snare.play ()
    sleep      (after_snare)
    hat.play   ()
    sleep      (after_hat)

# execute a new quiz
def n ( bar_duration : float = 1 ) -> Question:
  global Q
  after_snare = random.choice( [0.45, 0.5, 0.55] )
  after_hat = 1 - after_snare
  Q = Question ( after_snare = after_snare,
                 after_hat = after_hat,
                 bar_duration = bar_duration )
  print ( after_snare )
  play_forever (Q)
