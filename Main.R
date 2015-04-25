require("nnet")
require("MASS")
require("ggplot2")
require("RColorBrewer")

setwd("~/Documents/Kurser/Örebro Universitet/C-Uppsats/Kod/")

fname_df = "matchinfo_2014.csv"
fname_df.pred = "matchinfo_2015_2.csv"
last.year = "2014"
this.year = "2015"

# Avgör om vi ska använda normalsimulering eller ej.
normal.sim = T

# df innehåller mathinfo för tidigare år. df.pred för det år vi vill göra prediktion på.
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
df.pred$Outcome = factor(sapply(df.pred$Result,conv),levels=c("1","X","2"))

# Lägger in styrkeparametern
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

reg = reg4

# Skattade matchsannolikheter + odds beräknas.
est = predict(reg,df,"probs")
pred = predict(reg,df.pred,"probs")
df[,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")] = est
df.pred[,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")] = pred
df.pred[,c("EstimatedOdds_1","EstimatedOdds_X","EstimatedOdds_2")] = 1/pred

# Färgtema för grafer.
col = getPalette = colorRampPalette(brewer.pal(9,"Set1"))(16)

teamnames = unique(df.pred$Home)

source("DirectSim.R")
source("NormalSim.R")
source("Seasonal.R")

# Density-plot av marginalfördelningar
p2=ggplot(score.df)+
  geom_density(aes(Val,color=Team))+
  scale_x_continuous(limits=c(0,90))+
  scale_y_continuous(limits=c(0.00,0.08))+
  labs(x="score",title=this.year)+
  scale_color_manual(values=col)

# Spridningsdiagram
p3 = ggplot(df,aes(x=StrengthHome,y=StrengthAway, color=Outcome)) + 
      geom_point(shape=1) + 
      scale_color_manual(values=c("1"="#66FF00","X"="#0066CC","2"="#FF3333")) +
      ggtitle(paste("Result",last.year))