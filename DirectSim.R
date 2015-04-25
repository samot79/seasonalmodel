# Säsongssimulering genom direkt simulering av matcher.

if (!normal.sim) {

# Score är matris var kolumner är simulerade poängfördelningar
score = matrix(data=0,nrow=16,ncol=1e5)
rownames(score) = teamnames
for (j in 1:ncol(score)) {
  for (i in 1:nrow(df.pred)) {
    hteam = df.pred[i,"Home"]
    ateam = df.pred[i,"Away"]
    # Simulerat matchresultat:
    outcome = which(1==rmultinom(n=1,size=1,
                                 prob=df.pred[i,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")]))
    
    # Poängallokering
    if (outcome==1) {score[hteam,j]=score[hteam,j]+3}
    if (outcome==2) {score[c(hteam,ateam),j]=score[c(hteam,ateam),j]+1}
    if (outcome==3) {score[ateam,j]=score[ateam,j]+3}
  }
}

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

p1=ggplot(score.df)+
  geom_density(aes(Val,color=Team))+
  scale_x_continuous(limits=c(0,90))+
  scale_y_continuous(limits=c(0.00,0.08))+
  labs(x="score",title=paste(this.year,"Sim"))+
  scale_color_manual(values=col)

}