
# score.df är dataframe på den form som ggplot behöver.
score.df = data.frame()
score.df$Team = character()
score.df$Val = numeric()
k = 1
for (tn in rownames(score)) {
  score.df[k:(k+ncol(score)-1),"Val"] = score[tn,]
  score.df[k:(k+ncol(score)-1),"Team"] = tn
  k = k+ncol(score)
}

score.df$Team = factor(score.df$Team,levels=names(sort(mu)))

score.df$Lag = score.df$Team
# plot av poängfördelningen
p1=ggplot(score.df)+
  geom_density(aes(Val,color=Lag))+
  scale_x_continuous(limits=c(0,90))+
  scale_y_continuous(limits=c(0.00,0.08))+
  labs(x="Poäng",y="Täthet",title=paste("Simulering",this.year))+
  scale_color_manual(values=col[c(1,8,15,6,13,4,11,2,9,16,7,14,5,12,3,10)])+
  theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(2)))+
  theme(axis.ticks = element_line(size = rel(2)))+
  theme(legend.text=element_text(size=14))+
  theme(legend.title=element_text(size=16))


# Spridningsdiagram
p2 = ggplot(df$Old,aes(x=StrengthHome,y=StrengthAway, color=Outcome)) + 
  geom_point(shape=1) + 
  scale_color_manual(values=c("1"="#66FF00","X"="#0066CC","2"="#FF3333")) +
  ggtitle(paste("Result",last.year))

# överensstämmelse mellan proportionell och generell
p3 = ggplot(agreement,aes(x=Proportionell,y=Generell)) + geom_point(aes(color=Oddstyp)) +
  scale_x_continuous(limits=c(0.00,1.00)) + scale_y_continuous(limits=c(0.00,1.00))
p4  = ggplot(agreement,aes(x=Proportionell,y=Spelbolag)) + geom_point(aes(color=Oddstyp)) +
  scale_x_continuous(limits=c(0.00,1.00)) + scale_y_continuous(limits=c(0.00,1.00))