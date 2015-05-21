### Skattning av matchsannolikheter

# Noll-model, för framtagning av Pseudo-R2.
reg0 = vglm(Outcome ~ 1,
            data=df$Old,
            family=cumulative(parallel=T))

# Den primära modellen
reg1 = vglm(formula=Outcome ~ StrengthDiff,
           data=df$Old,
           family=cumulative(parallel=T))
# Ickeparallell modell För test av "proportional odds assumption"
reg2 = vglm(formula=Outcome ~ StrengthDiff,
            data = df$Old,
            family=cumulative(parallel=F))


p = 1/df$Old[,c("Odds_1","Odds_X","Odds_2")]
p = t(apply(p,1,function(x)x/sum(x))) # Tar bort skatt ur sannolikheterna.
res = c("1"=1,"X"=2,"2"=3)[df$Old$Outcome]
LL3 = sum(log(sapply(1:240,function(x)p[x,res[x]]))) # Likelihood för spelbolagens odds.

reg = reg1
pred = predictvglm(reg,df$New,type="response")


df$New[,c("EstimatedOdds_1","EstimatedOdds_X","EstimatedOdds_2")] = 1/pred
df$New[,c("EstimatedProb_1", "EstimatedProb_X","EstimatedProb_2")] = pred

fit1 = c(t(predictvglm(reg1, df$Old,type="response")))
fit2 = c(t(predictvglm(reg2, df$Old,type="response")))
fit3 = 1/t(as.matrix(df$Old[,c("Odds_1","Odds_X","Odds_2")]))
fit3 = c(apply(fit3,2,function(x)x/sum(x)))
agreement = data.frame(Proportionell=c(fit1),Generell=c(fit2),Spelbolag=fit3)
agreement$Oddstyp = rep(c("1","X","2"),240)
df$Reduced = df$New[,c("Home","Away","Outcome","EstimatedOdds_1","EstimatedOdds_X","EstimatedOdds_2")]