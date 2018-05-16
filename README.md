# Yahtzee
This code can solve for the Yahtzee with out upper section bonus rules.

In order to use this code, please run all code before line 326.

The function "decision(b,d,r)" can return the optimal strategy

In function decision(b,d,r):
  b -- a vector with length 13 discribing the number of boxes opening. Let 1 be Aces,2 be Two, 3 be Threes, 4 be Fours, 5 be Fives, 6 be Sixes 7 be full house, 8 be small straight, 9 be large straight, 10 be Yahtzee 11  be Three of A kind, 12 be Four of A kind, 13 be chance. If the ith boxes are opening, b[i]=1; otherwise b[i]=0.
  
  d -- a vector with length 6 discribing the current dice. If currently x dice have the number i, d[i]=x.
  
  r -- the number of re-rolls.
  
  The function will return: the dice you need to keep(using a vector with length 6) if r is not 0; the boxes you need to score if r is 0.
 
 The function "simulationgamen(n,b,d,r,sc)" can do simulations of the game.
 
 In the function simulationgamen(n,b,d,r,sc):
  n -- the number of simulations.
  
  b -- a vector with length 13 discribing the number of boxes opening.
  
  d -- a vector with length 6 discribing the current dice.
  
  r -- the number of re-rolls. At the beginning of the round, please use r = 3.
  
  sc -- the number discribing the current score. Default value is 0.
  
  
