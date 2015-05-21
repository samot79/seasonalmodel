### Normal modell:
# p[[*]] är matriser som innehåller sannolikheterna för olika matchutfall.

p = list()
for (x in c("1","X","2")) {
  p[[x]] = matrix(data=0,nrow=16,ncol=16)
  rownames(p[[x]]) = teamnames
  colnames(p[[x]]) = teamnames
}

for (i in 1:nrow(df$New)) {
  t.h = df$New[i,"Home"]
  t.a = df$New[i, "Away"]
  p[["1"]][t.h,t.a] = df$New[i,"EstimatedProb_1"]
  p[["X"]][t.h,t.a] = df$New[i,"EstimatedProb_X"]
  p[["2"]][t.h,t.a] = df$New[i,"EstimatedProb_2"]
}

# mu är vektorn av förväntade poäng
mu = rowSums(p[["X"]]+3*p[["1"]]) + colSums(p[["X"]]+3*p[["2"]])
sigma2 = rowSums(p[["X"]] + 9*p[["1"]] - (p[["X"]]+3*p[["1"]])^2) +
  colSums(p[["X"]] + 9*p[["2"]] - (p[["X"]]+3*p[["2"]])^2)

# Sigma är kovariansmatrisen
Sigma = p[["X"]] - (p[["X"]]+3*p[["1"]])*(p[["X"]]+3*p[["2"]])
Sigma = Sigma + t(Sigma)
diag(Sigma) = sigma2

# score:s kolumner är simulerade säsongsresultat
#score = mvrnorm(n=normal.nsims,mu=mu,Sigma=Sigma)
#score = t(score)

# Sampling ur multivariat normal
N = length(mu)
es = eigen(Sigma)
U = es$vectors
D = diag(es$values)
A = U%*%sqrt(D)
score = matrix(data=rnorm(n=N*normal.nsims),nrow=N,ncol=normal.nsims)
score = mu + A%*%score
rownames(score)  = names(mu)