### Outrightberäkning
## Modellering av Outrightodds. Simulering samt normalapproximation testas.

ranking = matrix(0,nrow=16,ncol=ncol(score))
rownames(ranking) = rownames(score)

for (k in 1:ncol(score)) {
  ranking[,k] = 17-rank(score[,k],ties.method="random")
}

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
      before[rn,cn] = apply(ranking,2,function(x)x[rn]>x[cn])
    }
  }
}

# Rankingsannolikheter: sannolikheten att lag i kommer på plats j.
ranking.prob = matrix(nrow=16,ncol=16)
rownames(ranking.prob) = rownames(ranking)
for (rn in rownames(ranking.prob)) {
  for (j in 1:16) ranking.prob[rn,j] = mean(ranking[rn,]==j)
}

maxrank = apply(ranking.prob,1,which.max)

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
  
  without.malmo = apply(apply(ranking[2:nrow(ranking),],2,rank)==1,1,mean)
}


relegated =  1/apply(apply(ranking,2,function(x)x %in% c(15,16)),1,mean)
names(relegated) = rownames(ranking)