import random
from   util import edoRatioToCents


def quizPairs ( edo : int ):
  a = random.randint ( 1 , edo-1 )
  b = random.randint ( 1 , edo-1 )
  c = random.randint ( 0, 1 )
  (sign, signSymbol) = (
    (lambda x: x, "+", )
    if c > 0 else
    (lambda x: -x, "-") )
  question = " ".join ( [ str(a), signSymbol, str(b) ] )
  answer = (a + sign ( b ) ) % edo
  print( question, "?" )
  input()
  print( " ".join ( [ str(question),
                      "=",
                      str(answer) ] ) )
  print ( " ".join ( [ str ( edoRatioToCents ( a/edo ) ),
                       signSymbol,
                       str ( edoRatioToCents ( b/edo ) ),
                       "=",
                       str ( edoRatioToCents ( answer/edo ) ),
                       "(in cents)"
                      ] ) )
  print()
  quizPairs(edo)
