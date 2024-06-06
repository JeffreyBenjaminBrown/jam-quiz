import random
import pygame
import numpy as np


pygame.mixer.init(size=32)

def note_hz ( root : float, # Hz
              edo : int,
              note_edo : int
             ) -> float:
  return root * 2 ** (note_edo / edo)

def random_edo_val ( edo : int ) -> int:
  return random.choice (
    range ( edo ) )

# Super inefficient but it works fine.
def squarish_buffer ( freq : float ):
  seconds = 2
  return (
    np.round (
      np.sin ( 2 * np.pi
               * np.arange (seconds * 44100)
               * freq / 44100) )
    . astype (np.float32) )

def q (edo : int):
  root = 150
  ev1 = random_edo_val ( edo )
  ev2 = random_edo_val ( edo )
  n1 = note_hz ( root = 150, edo = edo, note_edo = ev1 )
  n2 = note_hz ( root = 150, edo = edo, note_edo = ev1 + ev2 )
  ( pygame.mixer.Sound
    ( ( squarish_buffer ( n1 ) + squarish_buffer ( n2 ) )
      / 2 )
    . play () )
  print(ev2)
