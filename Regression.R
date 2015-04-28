### Skattning av matchsannolikheter
# Olika modeller testas

#df = df[1:50,]

reg0 = multinom(Outcome ~ 1, df$Old)
reg1 = multinom(Outcome ~ StrengthDiff,df$Old)
reg2 = multinom(Outcome ~ StrengthHome + StrengthAway-1, df$Old)
reg3 = multinom(Outcome ~ StrengthHome + StrengthAway, df$Old)
reg4 = polr(Outcome ~ StrengthDiff,df$Old)

reg = reg4

# Skattade matchsannolikheter + odds beräknas.
est = predict(reg,df$Old,"probs")
pred = predict(reg,df$New,"probs")
df$Old[,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")] = est
df$New[,c("EstimatedProb_1","EstimatedProb_X","EstimatedProb_2")] = pred
df$New[,c("EstimatedOdds_1","EstimatedOdds_X","EstimatedOdds_2")] = 1/pred
