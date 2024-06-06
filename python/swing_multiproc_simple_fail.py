# I was sticking this onto the end of swing.py
from   multiprocessing import Process

def drumloop ( after_snare : float,
               after_hat : float ):
  while True:
    print("hi!")
    snare.play()
    sleep(after_snare)
    hat.play()
    sleep(after_hat)
    print("bye!")

def go():
  after_snare = random.choice ( [0.45, 0.5, 0.55] )
  drumProc = Process ( target = drumloop,
                       args = ( after_snare,
                                1 - after_snare ) )
  drumProc.start ()
  _ = input ()
  drumProc.terminate ()
  print ( "after snare: ", after_snare )
