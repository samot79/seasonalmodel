# Säsongsberäkning genom direkt simulering av matcher.

# Score är matris var kolumner är simulerade poängfördelningar
score = matrix(data=0,nrow=16,ncol=direct.nsims)
rownames(score) = teamnames
for (i in 1:nrow(df$New)) {
  hteam = df$New[i,"Home"]
  ateam = df$New[i,"Away"]
  # Simulerat matchresultat:
  if (is.na(df$New[i,"Outcome"])) {
    outcome = apply(rmultinom(n=ncol(score),size=1,prob=df$New[i,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")]),
                    2,function(x)which(1==x))
  } else {
    outcome = which(c("1","X","2")==df$New[i,"Outcome"])
  }
  score[hteam,] = score[hteam,] + 3*(outcome==1)
  score[hteam,] = score[hteam,] + 1*(outcome==2)
  score[ateam,] = score[ateam,] + 1*(outcome==2)
  score[ateam,] = score[ateam,] + 3*(outcome==3)
}