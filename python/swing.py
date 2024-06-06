import pygame
import random
from   time import sleep


dirt_samples : str = "/home/jeff/.local/share/SuperCollider/downloaded-quarks/Dirt-Samples"

# PITFALL: global variables
AFTER_SNARE    = 0.5  # a proportion
AFTER_HAT      = None # constantly redefined in terms of snare
BAR_DURATION   = 1    # measured in seconds

pygame.mixer.init()

hat   = pygame.mixer.Sound (
  "/".join ( [ dirt_samples,
               "hc/001_VoodooHihat.wav" ] ) )
snare = pygame.mixer.Sound (
  "/".join ( [ dirt_samples,
               "sn/ST0T0S3.wav" ] ) )

def one_bar ():
  snare.play()
  sleep(AFTER_SNARE)
  hat.play()
  sleep(AFTER_HAT)

def play_bars ():
  global AFTER_SNARE
  print ( AFTER_SNARE )
  while True:
    one_bar ()

# play a new test
def n ( bar_duration : float = 1 ):
  global AFTER_SNARE, AFTER_HAT, BAR_DURATION
  AFTER_SNARE = random.choice( [0.46, 0.5, 0.54] )
  AFTER_HAT = 1 - AFTER_SNARE
  BAR_DURATION = bar_duration
  play_bars ()

# Repeat the last test's swing,
# but maybe change other aspects of it.
def r ( bar_duration : float = 1 ):
  global BAR_DURATION
  BAR_DURATION = bar_duration
  play_bars ()
