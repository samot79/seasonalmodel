### Skattning av matchsannolikheter
# Olika modeller testas

# Noll-model, för framtagning av Pseudo-R2.
reg0 = vglm(Outcome ~ 1,
            data=df$Old,
            family=cumulative(parallel=T))

reg1 = vglm(formula=Outcome ~ StrengthDiff,
           data=df$Old,
           family=cumulative(parallel=T))
# Ickeparallell modell För test av "proportional odds assumption"
reg2 = vglm(formula=Outcome ~ StrengthDiff,
            data = df$Old,
            family=cumulative(parallel=F))

reg = reg1

pred = predictvglm(reg,df$New,type="response")

df$New[,c("EstimatedOdds_1","EstimatedOdds_X","EstimatedOdds_2")] = 1/pred
df$New[,c("EstimatedProb_1", "EstimatedProb_X","EstimatedProb_2")] = pred

df$Reduced = df$New[,c("Home","Away","Outcome","EstimatedOdds_1","EstimatedOdds_X","EstimatedOdds_2")]
