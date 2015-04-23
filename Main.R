require("nnet")
require("MASS")
require("ggplot2")
require("plyr")
require("RColorBrewer")
require("gdata")

setwd("~/Documents/Kurser/Örebro Universitet/C-Uppsats/Kod/")

fname_df = "matchinfo_2014.csv"
fname_df.pred = "matchinfo_2015_2.csv"
last.year = "2014"
this.year = "2015"

df = read.csv(fname_df, stringsAsFactors=F)
df.pred = read.csv(fname_df.pred, stringsAsFactors=F)

# Konverterar matchresultat på formen "A-B" till 1,X eller 2, ex. 1-1 -> X, 3-1 -> 2.
conv = function(s) {
  sp = as.numeric(strsplit(as.character(s),"–")[[1]])
  if (length(sp)!=2) return(NA)
  diff = sp[2]-sp[1]
  if (diff < 0) return("1")
  if (diff == 0) return("X")
  return("2")
}
# Funktion för att transformera oddsen till en giltig styrkeparameter för regression.
odds.transform = function(x) -log(x-1)

df$Outcome = factor(sapply(df$Result,conv),levels=c("1","X","2"))

df$StrengthHome = odds.transform(df$OutrightHome)
df$StrengthAway = odds.transform(df$OutrightAway)
df$StrengthDiff = df$StrengthHome - df$StrengthAway
df.pred$StrengthHome = odds.transform(df.pred$OutrightHome)
df.pred$StrengthAway = odds.transform(df.pred$OutrightAway)
df.pred$StrengthDiff = df.pred$StrengthHome - df.pred$StrengthAway

### Skattning av matchsannolikheter
# Olika modeller testas

reg0 = multinom(Outcome ~ 1, df)
reg1 = multinom(Outcome ~ StrengthDiff,df)
reg2 = multinom(Outcome ~ StrengthHome + StrengthAway-1, df)
reg3 = multinom(Outcome ~ StrengthHome + StrengthAway, df)
reg4 = polr(Outcome ~ StrengthDiff,df)
reg5 = polr(Outcome ~ StrengthHome + StrengthAway, df)

reg = reg2

est = predict(reg,df,"probs")
pred = predict(reg,df.pred,"probs")

df[,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")] = est
df.pred[,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")] = pred
df.pred[,c("EstimatedOdds_1","EstimatedOdds_X","EstimatedOdds_2")] = 1/pred

get.odds = function(h,a)df.pred[df.pred$Home==h & df.pred$Away==a,][c("Home","Away","EstimatedOdds_1","EstimatedOdds_X","EstimatedOdds_2")]

### Outrightberäkning
## Modellering av Outrightodds. Simulering samt normalapproximation testas.
col = getPalette = colorRampPalette(brewer.pal(9,"Set1"))(16)

## Simulering:
# score = matrix(data=0,nrow=16,ncol=1e4)
# rownames(score) = rownames(odds.pred)
# for (j in 1:ncol(score)) {
#   for (i in 1:nrow(game)) {
#     hteam = game.pred[i,"Home"]
#     ateam = game.pred[i,"Away"]
#     outcome = which(1==rmultinom(n=1,size=1,
#                                      prob=game.pred[i,c("prob_1","prob_X","prob_2")]))
#     if (outcome==1) {score[hteam,j]=score[hteam,j]+3}
#     if (outcome==2) {score[c(hteam,ateam),j]=score[c(hteam,ateam),j]+1}
#     if (outcome==3) {score[ateam,j]=score[ateam,j]+3}
#   }
# }
# 
# score.df = data.frame()
# score.df$Team = character()
# score.df$Val = numeric()
# 
# k = 1
# for (tn in rownames(score)) {
#   score.df[k:(k+ncol(score)-1),"Val"] = score[tn,]
#   score.df[k:(k+ncol(score)-1),"Team"] = tn
#   k = k+ncol(score)
#  }
# 
# 
# p1=ggplot(score.df)+
#   geom_density(aes(Val,color=Team))+
#   scale_x_continuous(limits=c(0,90))+
#   scale_y_continuous(limits=c(0.00,0.08))+
#   labs(x="score",title=paste(this.year,"Sim"))+
#   scale_color_manual(values=col)

### Normal modell:
# dessa tre matriser innehåller sannolikheterna för alla utfall i alla 240 matcher, för  i 1,X respektive 2.
p = list()
teamnames = unique(df.pred$Home)
for (x in c("1","X","2")) {
  p[[x]] = matrix(data=0,nrow=16,ncol=16)
  rownames(p[[x]]) = teamnames
  colnames(p[[x]]) = teamnames
}

for (i in 1:nrow(df.pred)) {
  t.h = df.pred[i,"Home"]
  t.a = df.pred[i, "Away"]
  p[["1"]][t.h,t.a] = df.pred[i,"EstimatedProb_1"]
  p[["X"]][t.h,t.a] = df.pred[i,"EstimatedProb_X"]
  p[["2"]][t.h,t.a] = df.pred[i,"EstimatedProb_2"]
}

# mu är vektorn av förväntade poäng
mu = rowSums(p[["X"]]+3*p[["1"]]) + colSums(p[["X"]]+3*p[["2"]])
sigma2 = rowSums(p[["X"]] + 9*p[["1"]] - (p[["X"]]+3*p[["1"]])^2) +
  colSums(p[["X"]] + 9*p[["2"]] - (p[["X"]]+3*p[["2"]])^2)

# Sigma är variansmatrisen
Sigma = p[["X"]] - (p[["X"]]+3*p[["1"]])*(p[["X"]]+3*p[["2"]])
Sigma = Sigma + t(Sigma)
diag(Sigma) = sigma2

score.norm = mvrnorm(n=1e6,mu=mu,Sigma=Sigma)
score.norm = t(score.norm)

score.norm.df = data.frame()
score.norm.df$Team = character()
score.norm.df$Val = numeric()

k = 1
for (tn in rownames(score.norm)) {
  score.norm.df[k:(k+ncol(score.norm)-1),"Val"] = score.norm[tn,]
  score.norm.df[k:(k+ncol(score.norm)-1),"Team"] = tn
  k = k+ncol(score.norm)
}



before = matrix(nrow=16,ncol=16)
rownames(before) = rownames(score.norm)
colnames(before) = rownames(score.norm)
for (rn in rownames(before)) {
  for (cn in colnames(before)) {
    before[rn,cn] = 1-pnorm(mean=mu[rn]-mu[cn],
                            sd=sqrt(Sigma[rn,rn]+Sigma[cn,cn]-2*Sigma[rn,cn]),
                            q=0)
  }
}

ranking = matrix(0,nrow=16,ncol=ncol(score.norm))
rownames(ranking) = rownames(before)

for (k in 1:ncol(score.norm)) {
  ranking[,k] = 17-rank(score.norm[,k])
}

# rankingsannolikheter: sannolikheten att lag i kommer på plats j.
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


p1=ggplot(score.norm.df)+
  geom_density(aes(Val,color=Team))+
  scale_x_continuous(limits=c(0,90))+
  scale_y_continuous(limits=c(0.00,0.08))+
  labs(x="score",title=this.year)+
  scale_color_manual(values=col)

p2 = ggplot(df,aes(x=StrengthHome,y=StrengthAway, color=Outcome)) + 
      geom_point(shape=1) + 
      scale_color_manual(values=c("1"="#66FF00","X"="#0066CC","2"="#FF3333")) +
      ggtitle(paste("Result",last.year))