for (t in c("Old","New")) {
  teamnames = unique(as.vector(df[[t]]$Home))
  df[[t]]$OutrightHome = numeric(nrow(df[[t]]))
  df[[t]]$OutrightAway = numeric(nrow(df[[t]]))
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
  }
  df[[t]]$Outcome = ordered(factor(df[[t]]$Outcome,levels=c("1","X","2")))
}

ptm0 = proc.time() # tiddagning
df$New$StrengthHome = odds.transform(df$New$OutrightHome)
df$New$StrengthAway = odds.transform(df$New$OutrightAway)
df$New$StrengthDiff = df$New$StrengthHome - df$New$StrengthAway

# Avgör om vi ska använda normalsimulering eller ej.
normal.sim = T

normal.nsims = 1e5
direct.nsims = 1e4

# Funktion för att sätta ny outright

source("Regression.R")

# Färgtema för grafer.
col = getPalette = colorRampPalette(brewer.pal(9,"Set1"))(16)

teamnames = unique(df$New$Home)

# Om matchutfallen är kända, ändra matchsannolikheterna. (implementerar delvis spelade säsonger)
v = list("1"=c(1,0,0),"X"=c(0,1,0),"2"=c(0,0,1))
for (k in 1:nrow(df$New)) {
  if (!is.na(df$New[k,"Outcome"]))
    df$New[k,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")] = v[[df$New[k,"Outcome"]]]
}

if (normal.sim) {
  source("NormalSim.R")
} else {
  source("DirectSim.R")
}

ptm1 = proc.time()-ptm0

source("Seasonal.R")
source("Plot.R")

ptm2 = proc.time() - ptm0