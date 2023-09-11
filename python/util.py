# Since 18 steps of 31-edo is about 697 cents,
# edoRatioToCents ( 18/31 ) = 697.
def edoRatioToCents ( r : float
                     ) -> float:
  return round ( 1200 * r )
