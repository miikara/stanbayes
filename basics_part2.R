### Ex. 1
# a)
thetas <-seq(0, 1, by = 0.01)
plot(NULL, xlim = c(0,1), ylim = c(0,4), ann = FALSE)
title(main="Beta prior distributions", xlab = "Theta", ylab = "")
lines(thetas, dbeta(x = thetas, shape1 = 0.01, shape2 = 0.01), col = "green")
lines(thetas, dbeta(x = thetas, shape1 = 0.5, shape2 = 0.5), col = "blue")
lines(thetas, dbeta(x = thetas, shape1 = 1, shape2 = 1), col = "red")
lines(thetas, dbeta(x = thetas, shape1 = 7, shape2 = 7), col = "purple")
legend(x = 0.7, y = 4, legend = c("a=0.01,b=0.01", "a=0.5,b=0.5", "a=1,b=1", "a=7,b=7"), fill = c("green", "red", "blue", "purple"), cex = 0.7)
abline(v = 1/2, lty=2)

# b)
# First three tosses with two successes
# MLE is argmax y | theta which follows binomial distribution = trues / n
trues <-2
falses <-1
mle <-(trues)/(trues+falses)

thetas <-seq(0, 1, by = 0.01)
plot(NULL, xlim = c(0,1), ylim = c(0,4), ann = FALSE)
title(main="Beta posterior distributions (n=3)", xlab = "Theta", ylab = "")
lines(thetas, dbeta(x = thetas, shape1 = 0.01 + trues, shape2 = 0.01 + falses), col = "green")
lines(thetas, dbeta(x = thetas, shape1 = 0.5 + trues, shape2 = 0.5 + falses), col = "blue")
lines(thetas, dbeta(x = thetas, shape1 = 1 + trues, shape2 = 1 + falses), col = "red")
lines(thetas, dbeta(x = thetas, shape1 = 7 + trues, shape2 = 7 + falses), col = "purple")
legend(x = 0.7, y = 4, legend = c("a=0.01,b=0.01", "a=0.5,b=0.5", "a=1,b=1", "a=7,b=7"), fill = c("green", "red", "blue", "purple"), cex = 0.7)
abline(v = mle, lty=2)

# probabilities when n = 3
# We get fairly different probabilities and different posteriors 
# This is because the curves have different priors and a very small sample for the sampling distribution
# The difference between the lowest probabality and the highest is over 15 percentage units
green_p = pbeta(q = 0.5, shape1 = 0.01 + trues, shape2 = 0.01 + falses, lower.tail = TRUE)
blue_p = pbeta(q = 0.5, shape1 = 0.5 + trues, shape2 = 0.5 + falses, lower.tail = TRUE)
red_p = pbeta(q = 0.5, shape1 = 1 + trues, shape2 = 1 + falses, lower.tail = TRUE)
purple_p = pbeta(q = 0.5, shape1 = 7 + trues, shape2 = 7 + falses, lower.tail = TRUE)
n_3_probabilities <-c(green_p, blue_p, red_p, purple_p)
print(n_3_probabilities)


# Then 25 tosses with ten successes
trues_2 <-10
falses_2 <-15
mle_2 <-(trues_2)/(trues_2+falses_2)

thetas <-seq(0, 1, by = 0.01)
plot(NULL, xlim = c(0,1), ylim = c(0,6), ann = FALSE)
title(main="Beta posterior distributions (n=25)", xlab = "Theta", ylab = "")
lines(thetas, dbeta(x = thetas, shape1 = 0.01 + trues_2, shape2 = 0.01 + falses_2), col = "green")
lines(thetas, dbeta(x = thetas, shape1 = 0.5 + trues_2, shape2 = 0.5 + falses_2), col = "blue")
lines(thetas, dbeta(x = thetas, shape1 = 1 + trues_2, shape2 = 1 + falses_2), col = "red")
lines(thetas, dbeta(x = thetas, shape1 = 7 + trues_2, shape2 = 7 + falses_2), col = "purple")
legend(x = 0.7, y = 4, legend = c("a=0.01,b=0.01", "a=0.5,b=0.5", "a=1,b=1", "a=7,b=7"), fill = c("green", "red", "blue", "purple"), cex = 0.7)
abline(v = mle_2, lty=2)

# c)
# When there are more observations used the curve are more similar
# The probabilities do not fluctuate as much and the difference is at most 5 percentage units
# This is natural is the posteriors have not more "in common" than when they only shared 3 observations
# The priors do not matter as much anymore (apart from perhaps where alpha = beta = 7)
green_p_2 = pbeta(q = 0.5, shape1 = 0.01 + trues_2, shape2 = 0.01 + falses_2, lower.tail = TRUE)
blue_p_2= pbeta(q = 0.5, shape1 = 0.5 + trues_2, shape2 = 0.5 + falses_2, lower.tail = TRUE)
red_p_2 = pbeta(q = 0.5, shape1 = 1 + trues_2, shape2 = 1 + falses_2, lower.tail = TRUE)
purple_p_2 = pbeta(q = 0.5, shape1 = 7 + trues_2, shape2 = 7 + falses_2, lower.tail = TRUE)
n_3_probabilities_2 <-c(green_p_2, blue_p_2, red_p_2, purple_p_2)
print(n_3_probabilities_2)

### Ex. 2
# install.packages('extraDistr')
# a)
# Based on page 15 we know that the posterior predictive distribution is beta-binomial
# The priors are beta and marginal likelihoods are beta-binomial
# Marginal likelihoods (prior predictive dist.) are computed before any observations are made (no change to alpha/beta) 
library(extraDistr)
ys <-c(0:10)
plot(NULL, xlim = c(0,10), ylim = c(0,0.8), ann = FALSE)
title(main="Marginal likelihoods", xlab = "Successes in 10 throws", ylab = "")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 0.01, beta = 0.01), type = "b", col = "green")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 0.5, beta = 0.5), type = "b", col = "blue")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 1, beta = 1), type = "b", col = "red")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 7, beta = 7), type = "b", col = "purple")
legend(x = 7, y = 0.8, legend = c("a=0.01,b=0.01", "a=0.5,b=0.5", "a=1,b=1", "a=7,b=7"), fill = c("green", "red", "blue", "purple"), cex = 0.7)

# b)
# It is beta-binomial
# The posterior predictive with prior alpha = 7 & beta = 7 is closest to the binomial
# This is due to its prior suggesting more that the true theta is around 0.5 instead of potentially being zero etc.
ys <-c(0:10)
plot(NULL, xlim = c(0,10), ylim = c(0,0.4), ann = FALSE)
title(main="Posterior predictive distributions, n=3", xlab = "Successes in 10 throws", ylab = "")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 0.01 + trues, beta = 0.01 + falses), type = "b", col = "green")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 0.5 + trues, beta = 0.5 + falses), type = "b", col = "blue")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 1 + trues, beta = 1 + falses), type = "b", col = "red")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 7 + trues, beta = 7 + falses), type = "b", col = "purple")
lines(x = ys, dbinom(x = ys, size = 10, prob = mle), type = 'b', col = 'black')
legend(x = 0, y = 0.4, legend = c("a=0.01,b=0.01", "a=0.5,b=0.5", "a=1,b=1", "a=7,b=7", "binomial"), fill = c("green", "red", "blue", "purple", "black"), cex = 0.7)

# c)
# As we have more observations, the importance of the prior decreases and distributions become more alike
plot(NULL, xlim = c(0,10), ylim = c(0,0.4), ann = FALSE)
title(main="Posterior predictive distributions, n=25", xlab = "Successes in 25 throws", ylab = "")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 0.01 + trues_2, beta = 0.01 + falses_2), type = "b", col = "green")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 0.5 + trues_2, beta = 0.5 + falses_2), type = "b", col = "blue")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 1 + trues_2, beta = 1 + falses_2), type = "b", col = "red")
lines(x = ys, dbbinom(x = ys, size = 10, alpha = 7 + trues_2, beta = 7 + falses_2), type = "b", col = "purple")
lines(x = ys, dbinom(x = ys, size = 10, prob = mle), type = 'b', col = 'black')
legend(x = 0, y = 0.4, legend = c("a=0.01,b=0.01", "a=0.5,b=0.5", "a=1,b=1", "a=7,b=7", "binomial"), fill = c("green", "red", "blue", "purple", "black"), cex = 0.7)

# d)
# Function for calculating distinct probability based on mass functions
distinct_bbinom_probability <-function(successes, n, given_alpha, given_beta){
  upper_bound <-pbbinom(successes, n, alpha = given_alpha, beta = given_beta, lower.tail = TRUE)
  lower_bound <-pbbinom((successes-1), n, alpha = given_alpha, beta = given_beta, lower.tail = TRUE)
  probability <-upper_bound - lower_bound
  return(probability)
}

alpha_betas <-c(0.01, 0.5, 1, 7)
posterior_predictive_probabilities <-vector(mode = "list", length = 4)
names(posterior_predictive_probabilities) <-alpha_betas
for (i in 1:length(alpha_betas)) {
  prob <-distinct_bbinom_probability(successes = 4, n = 10, given_alpha = (i + trues_2), given_beta = (i + falses_2))
  posterior_predictive_probabilities[i] <-prob
}

print(posterior_predictive_probabilities)

# The probabilities are slightly above 20% using any of the four priors with only small differences
