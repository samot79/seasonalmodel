### Outrightberäkning
## Modellering av Outrightodds. Simulering samt normalapproximation testas.

ranking = apply(score,2,function(x)17-rank(x,ties.method="random"))

winner = 1/apply(ranking,1,function(x)mean(x==1))

# before: matris som innehåller sannolikheten att ett visst lag kommer före ett annat.
# kan beräknas exakt (utan simulering) vid normalapproximation.
before = matrix(nrow=16,ncol=16)
rownames(before) = rownames(score)
colnames(before) = rownames(score)
if (normal.sim) {
  for (rn in rownames(before)) {
    for (cn in colnames(before)) {
      before[rn,cn] = 1-pnorm(mean=mu[rn]-mu[cn],
                              sd=sqrt(Sigma[rn,rn]+Sigma[cn,cn]-2*Sigma[rn,cn]),
                              q=0)
    }
  }
} else {
  for (rn in rownames(score)) {
    for (cn in rownames(score)) {
      before[rn,cn] = mean(apply(ranking,2,function(x)x[rn]<x[cn]))
    }
  }
}

# Rankingsannolikheter: sannolikheten att lag i kommer på plats j.
ranking.prob = matrix(nrow=16,ncol=16)
rownames(ranking.prob) = rownames(ranking)
for (rn in rownames(ranking.prob)) {
  for (j in 1:16) ranking.prob[rn,j] = mean(ranking[rn,]==j)
}

if (this.year == "2015") {
  # best-in-region-odds
  region = list()
  region$Stockholm = c("AIK","HAIF","DIF")
  region$West = c("IFKG","IFE","BKH","FFF","HBK")
  region$Rest = c("ÖSK","IFKN","GIFS","GIF","ÅFF")
  best = list()
  for (n in names(region)) {
    y=apply(ranking,2,function(x)which.min(x[region[[n]]]))
    best[[n]] = 1/sapply(1:length(region[[n]]),function(x)mean(x==y))
    names(best[[n]]) = region[[n]]
  }
  without.malmo = apply(apply(ranking,2,function(x)(x==1 & x["MFF"]!=1) | (x==2 & x["MFF"]==1)),1,mean)
}

relegated =  1/apply(apply(ranking,2,function(x)x == 15 | x==16),1,mean)

top3 = rowMeans(apply(ranking,2,function(x)x<=3))

kval = 1/rowMeans(ranking==14)
