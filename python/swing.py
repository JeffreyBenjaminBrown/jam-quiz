# PURPOSE:
# Test your ability to hear different amounts of swing.
#
# USAGE:
# First be sure you have `pygame` installed for Python.
# On Debian systems (including Ubuntu) `pip install pygame`
# probably does it.
# If you use Nix, you can find a Nix shell script for it here:
#   https://github.com/JeffreyBenjaminBrown/jam-quiz/blob/master/shell.nix
#
# Then customize the values in the section called
# "CUSTOMIZE THESE VALUES".
#
# Then load the code into a Python shell.
# I like to just copy the whole text and paste it into iPython.
# (Less heavyweight Python shells sometimes choke
# on multi-line pastes.)
#
# To start a new quiz, type `n()`, close your eyes, and hit Return.
# The answer will be displayed on the screen.
# Listen until you think you know the answer, then open your eyes,
# and press Ctrl-C to stop the drum loop.
# You can optionally include a numeric argument to `n()`
# that dictates the duration of a complete cycle in seconds.
# That value defaults to 1 second.
#
# To replay the last quiz, type `r().

from   dataclasses import dataclass
import pygame
import random
from   time import sleep


swing_possibilities = [
  # CUSTOMIZE THIS LIST.
  # These are the swing values, as percentages,
  # that the quizzer chooses between.
  # A value of 0.5 gives straight time --
  # the same duration from snare to hat as from hat to snare.
  # An elementary school swing value would be 0.666 --
  # the snare two-thirds of the way from the preceding snare
  # to the subsequent one.
  0.45, 0.5, 0.55]

pygame.mixer.init()

snare = pygame.mixer.Sound (
  # CUSTOMIZE THIS PATH.
  # Point it to an audio file you like, to use as the snare.
  "/home/jeff/.local/share/SuperCollider/downloaded-quarks/Dirt-Samples/sn/ST0T0S3.wav" )

hat   = pygame.mixer.Sound (
  # CUSTOMIZE THIS PATH.
  # Point it to an audio file you like, to use as the hat.
  "/home/jeff/.local/share/SuperCollider/downloaded-quarks/Dirt-Samples/hc/001_VoodooHihat.wav" )

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
# for failed attempts, see the files called
#   swing_multiproc_*_fail.py
# Strangely, those subprocesses will print things,
# but they refuse to play sounds.
Q = Question ( 1,2,3 )

def play_forever ( q : Question ):
  after_snare = q.bar_duration * q.after_snare
  after_hat   = q.bar_duration * q.after_hat
  while True:
    snare.play ()
    sleep      (after_snare)
    hat.play   ()
    sleep      (after_hat)

# Execute a new quiz
def n ( bar_duration : float = 1 ):
  global Q
  after_snare = random.choice( swing_possibilities )
  after_hat = 1 - after_snare
  Q = Question ( after_snare = after_snare,
                 after_hat = after_hat,
                 bar_duration = bar_duration )
  print ( after_snare )
  play_forever (Q)

# Repeat the last quiz
def r ():
  global Q
  play_forever(Q)
