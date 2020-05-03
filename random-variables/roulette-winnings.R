n <- 1000
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
  sum(X)
}
S <- replicate(B, roulette_winnings(n))

pbinom(n/2-1, size = n, prob = 10/19)#the way this also could be done

#observed values (Monte Carlo simulation):
mean(S)
sd(S)
mean(S < 0)

#theoretical values:
n * (20-18)/38 
sqrt(n) * 2 * sqrt(90)/19 

#Using CLT instead of Monte Carlo simulation:
mu <- n * (20-18)/38
se <-  sqrt(n) * 2 * sqrt(90)/19 
pnorm(0, mu, se)
