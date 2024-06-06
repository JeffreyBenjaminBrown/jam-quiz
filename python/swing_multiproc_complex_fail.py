class Drumloop (Process):
  after_snare : float
  after_hat   : float
  def __init__ (self, after_snare, after_hat):
    self.after_snare = after_snare
    self.after_hat = after_hat
  def run(self):
    while True:
      snare.play()
      sleep(after_snare)
      hat.play()
      sleep(after_hat)

def go():
  after_snare = random.choice ( [0.45, 0.5, 0.55] )
  loop = Drumloop ( after_snare, 1 - after_snare )
  loop.start ()
  _ = input()
  loop.terminate ()
  print ( "after snare: ", after_snare )
