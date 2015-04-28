amount.bet = 0
amount.won = 0
for (k in 1:50) {
  probs = df.pred[k,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")]
  odds = df.pred[k,c("Odds_1","Odds_X","Odds_2")]
  ret = odds*probs - (1-probs)
  which.bet = which.max(ret)
  which.won = c("1"=1,"X"=2,"2"=3)[df.pred[k,"Outcome"]]
  if (ret[which.bet]>0) {
    if (which.bet == which.won) amount.won = amount.won + odds[which.bet]
    amount.bet = amount.bet + 1
  }
}

amount.diff = amount.won - amount.bet