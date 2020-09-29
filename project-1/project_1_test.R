x = matrix(c(runif(100),runif(100)),ncol = 2)
y = rpois(100,lambda = exp(3  + 2*x[,1] - 2*x[,2]))
df = data.frame(y=y,x=x)
plot(df)
formula = y~x
mf = model.frame(formula = formula, data = df)
X = model.matrix(attr(mf, "terms"), data = mf)
y = model.response(mf)
if (anyNA(NA)){
  beta = numeric(ncol(X))
}
new_beta = 100 + beta
tmp = norm(as.matrix(new_beta - beta))
tau = 0.004
while (tau < tmp){
  Sb = t(X)%*%(y-exp(X%*%beta)) 
  Fb = t(X)%*%as.matrix(as.data.frame(X)*exp(X%*%beta))
  iFb = solve(Fb)
  new_beta = beta + iFb%*%Sb
  tmp =  norm(as.matrix(new_beta - beta),type = "2")/norm(as.matrix(beta),type = "2")
  print(tmp)
  beta = new_beta
}
log(y)
r_coefficients = cbind(new_beta,sqrt(diag(iFb)))
colnames(r_coefficients) = c("mean", "sd")
i_rem = as.numeric(names(y[!is.infinite(log(y))]))
r_deviance = 2*sum(y[i_rem]*(log(y[i_rem]) - X[i_rem,]%*%new_beta) - y[i_rem] + exp(X[i_rem,]%*%beta))
r_vcov = iFb
