# computes the Anderson-Darling test statistic for given sample X and CDF F
ad.test.statistic <- function(X, F) {
  n <- length(X)
  Y <- sort(X)
  
  W <- seq(from=1, by=2, length.out=n)
  S <- 1/n * sum(W * (log(F(Y)) + log(1 - F(rev(Y)))))
  return (-n - S)
}

# builds 1000 samples from given distribution R, computes
# Anderson-Darling statistic for each sample against theoretical CDF P
# then computes ECDF for the generated sample from Anderson-Darling distribution
ad.ecdf.n <- function(n, R, P) {
  ad_res <- numeric(1000)
  
  for (k in 1:1000) {
    ad_res[k] <- ad.test.statistic(R(n), P)
  }
  
  return(ecdf(ad_res))
}

f_1 <- ad.ecdf.n(10, rnorm, pnorm)
f_2 <- ad.ecdf.n(100, rnorm, pnorm)
f_3 <- ad.ecdf.n(1000, rnorm, pnorm)
f_4 <- ad.ecdf.n(10000, rnorm, pnorm)

BLUE <- "dodgerblue"

layout(matrix(c(1, 2, 5, 3, 4, 5, 6, 7, 9, 8, 0, 9), 4, 3, byrow=TRUE))
plot.stepfun(f_1, main="Standard normal (n = 10)", col=BLUE, lwd=3)
plot.stepfun(f_2, main="Standard normal (n = 100)", col=BLUE, lwd=3)
plot.stepfun(f_3, main="Standard normal (n = 1000)", col=BLUE, lwd=3)
plot.stepfun(f_4, main="Standard normal (n = 10000)", col=BLUE, lwd=3)
plot.stepfun(f_1, main="Combined plots (plot for n = 10000 highlighted)", col="yellow", lwd=2)
plot.stepfun(f_2, add=TRUE, col="pink", lwd=2)
plot.stepfun(f_3, add=TRUE, col="orange", lwd=2)
plot.stepfun(f_4, add=TRUE, col="red", lwd=3)
legend('bottomright', c("n=10", "n=100", "n=1000", "n=10000"), lty=c(1,1), lwd=c(2.5,2.5),col=c("yellow","pink", "orange", "red"))

g_1 <-f_3
g_2 <- ad.ecdf.n(1000, runif, punif)
g_3 <- ad.ecdf.n(1000, rexp, pexp)

plot.stepfun(g_1, main="Limiting CDF - standard normal", col=BLUE, lwd=3)
plot.stepfun(g_2, main="Limiting CDF - U[0, 1]", col=BLUE, lwd=3)
plot.stepfun(g_3, main="Limiting CDF - Exp(1)", col=BLUE, lwd=3)
plot.stepfun(g_1, main="Combined limiting CDFs", col='red', lwd=2)
plot.stepfun(g_2, add=TRUE, col='orange', lwd=2)
plot.stepfun(g_3, add=TRUE, col='yellow', lwd=2)
legend('bottomright', c("Std normal", "Uniform", "Exp"), lty=c(1,1), lwd=c(2.5, 2.5), col=c("red", "orange", "yellow"))