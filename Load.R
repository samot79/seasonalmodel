require("gdata")
require("ggplot2")
require("RColorBrewer")
require("VGAM")

ROOT = "/home/william/Dropbox/Kurser/Örebro Universitet/C-Uppsats/Kod"

setwd(ROOT)
fname = "SeasonModel2015.xlsx"
last.year = "2014"
this.year = "2015"

# Funktion för att transformera oddsen till en giltig styrkeparameter för regression.
odds.transform = function(x) -log(x-1)

df = list()
outright = list()
df$Old = read.xls(fname, sheet="LastYearResult")
df$New = read.xls(fname, sheet="ThisYearResult")
outright$Old = read.xls(fname, sheet="LastYearOutright")
outright$New = read.xls(fname, sheet="ThisYearOutright")

# Funktion för att konvertera matchresultat på formen "A-B" till 1,X eller 2, ex. 1-1 -> X, 3-1 -> 2.
conv0 = function(s) {
  sp = as.numeric(strsplit(as.character(s),"[–-]")[[1]])
  if (length(sp)!=2) return(NA)
  diff = sp[1]-sp[2]
  #if (diff > 0) return (1)
  #if (diff == 0) return (0)
  #if (diff < 0) return (-1)
  return (diff)
}

conv = function(s) {
  d = conv0(s)
  if (is.na(d)) return(NA)
  if (d>0) return ("1")
  if (d<0) return ("2")
  if (d==0) return ("X")
}

load = function() {source("Load.R")}
run = function() (source("Main.R"))
edit.outright = function() {outright$New <<- edit(outright$New)}
edit.result = function() {df$New[,c("Home","Away","Outcome")] <<- edit(df$New[,c("Home","Away","Outcome")])}

df$Old$Outcome = ordered(factor(sapply(df$Old$Result,conv),levels=c("1","X","2")))
df$New$Outcome = ordered(factor(sapply(df$New$Result,conv),levels=c("1","X","2")))