require("nnet")
require("MASS")
require("ggplot2")
require("RColorBrewer")
require("xlsx")

setwd("~/Documents/Kurser/Örebro Universitet/C-Uppsats/Kod/")

fname = "SeasonModel2015.xlsx"
last.year = "2014"
this.year = "2015"

# Avgör om vi ska använda normalsimulering eller ej.
normal.sim = T
normal.nsims = 1e5
direct.nsims = 1e3

# df innehåller mathinfo för tidigare år. df.pred för det år vi vill göra prediktion på.
df = list()
outright = list()
df$Old = read.xlsx(fname, sheetName="LastYearResult")
df$New = read.xlsx(fname, sheetName="ThisYearResult")
outright$Old = read.xlsx(fname, sheetName="LastYearOutright")
outright$New = read.xlsx(fname, sheetName="ThisYearOutright")

# Funktion för att konvertera matchresultat på formen "A-B" till 1,X eller 2, ex. 1-1 -> X, 3-1 -> 2.
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

for (t in c("Old","New")) {
  df[[t]]$Home = as.vector(df[[t]]$Home)
  df[[t]]$Away = as.vector(df[[t]]$Away)
  rownames(outright[[t]]) = outright[[t]]$Name
  outright[[t]]$Odds = as.numeric(as.vector(outright[[t]]$Odds))
  outright[[t]]$Name = as.vector(outright[[t]]$Name)
  for (k in 1:nrow(df[[t]])) {
    hname = df[[t]][k,"Home"]
    aname = df[[t]][k,"Away"]
    df[[t]][k,"OutrightHome"] = outright[[t]][hname,"Odds"]
    df[[t]][k,"OutrightAway"] = outright[[t]][aname,"Odds"]
    df[[t]][k,"StrengthHome"] = odds.transform(df[[t]][k,"OutrightHome"])
    df[[t]][k,"StrengthAway"] = odds.transform(df[[t]][k,"OutrightAway"])
    df[[t]][k,"StrengthDiff"] = df[[t]][k,"StrengthHome"] - df[[t]][k,"StrengthAway"]
    df[[t]][k,"Outcome"] = conv(df[[t]][k,"Result"])
  }
  df[[t]]$Outcome = factor(df[[t]]$Outcome,levels=c("1","X","2"))
  
}

source("Regression.R")

# Färgtema för grafer.
col = getPalette = colorRampPalette(brewer.pal(9,"Set1"))(16)

teamnames = unique(df$New$Home)

if (normal.sim) {
  source("NormalSim.R")
} else {
  source("DirectSim.R")
}
source("Seasonal.R")

# plot av poängfördelningen
p1=ggplot(score.df)+
  geom_density(aes(Val,color=Team))+
  scale_x_continuous(limits=c(0,90))+
  scale_y_continuous(limits=c(0.00,0.08))+
  labs(x="score",title=paste(this.year,"Simulation"))+
  scale_color_manual(values=col)

# Spridningsdiagram
p2 = ggplot(df$Old,aes(x=StrengthHome,y=StrengthAway, color=Outcome)) + 
      geom_point(shape=1) + 
      scale_color_manual(values=c("1"="#66FF00","X"="#0066CC","2"="#FF3333")) +
      ggtitle(paste("Result",last.year))