import random
import pygame
import numpy as np


pygame.mixer.init(size=32)

root = 150 # Would be the lowest possible note, in Hz,
           # except negative notes are possible.

def note_in_hz ( edo : int,
              note_edo : int
             ) -> float:
  return root * 2 ** (note_edo / edo)

def random_edo_val ( edo : int ) -> int:
  # TODO ? Give a range of more than one octave.
  return random.choice (
    range ( edo ) )

# Super inefficient but it works fine.
def squarish_buffer ( freq : float ):
  seconds = 2
  return (
    np.round (
      np.sin ( 2 * np.pi * freq
               * np.arange (seconds * 44100)
               / 44100) )
    . astype (np.float32) )

def quiz (edo : int):
  interval_in_edo = random_edo_val ( edo )
  low_in_edo      = random_edo_val ( edo )
  high_in_edo     = low_in_edo + interval_in_edo
  low_in_hz       = note_in_hz ( edo = edo, note_edo = low_in_edo )
  high_in_hz      = note_in_hz ( edo = edo, note_edo = high_in_edo )
  ( pygame.mixer.Sound
    ( ( squarish_buffer ( low_in_hz ) +
        squarish_buffer ( high_in_hz ) )
      / 2 )
    . play () )
  print (interval_in_edo)

def play (
    edo : int, interval_in_edo : int,
    low_in_edo    : int = 0 # PITFALL:
      # I'm using 0 here as if it was None,
      # because Python no longer allows that.
      # This is ugly; a Maybe ("Optional" in Python?)
      # would be prettier.
):
  if not low_in_edo:
    low_in_edo = random_edo_val ( edo )
  high_in_edo  = low_in_edo + interval_in_edo
  low_in_hz    = note_in_hz ( edo = edo, note_edo = low_in_edo )
  high_in_hz   = note_in_hz ( edo = edo, note_edo = high_in_edo )
  ( pygame.mixer.Sound
    ( ( squarish_buffer ( low_in_hz ) +
        squarish_buffer ( high_in_hz ) )
      / 2 )
    . play () )
  print (interval_in_edo)

def q()        : quiz(58)
def p(i,l = 0) : play(58,i,l)
