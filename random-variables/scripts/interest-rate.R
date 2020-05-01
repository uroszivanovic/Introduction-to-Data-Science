n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02 
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#Monte Carlo:
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

#CLT:
n*(p*loss_per_foreclosure + (1-p)*0)
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

#interest rate:
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

loss_per_foreclosure*p + x*(1-p)

#then, total expected profit:
n*(loss_per_foreclosure*p + x*(1-p)) 


#Running Monte Carlo simulation to check the theoretical aproximation:
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

mean(profit<0)
