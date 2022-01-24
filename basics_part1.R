### Ex. 1
# a)
# From written exercise 2 we know that the posteriors are proportional to gamma
# The likelihood part is: Theta ^ (n/2) * e ^ (- Theta * n * y_mean / 2)
# This can be regarded as gamma(alpha_1, beta_1) where alpha_1 = n/2 + 1 and beta_1 = n * y_mean / 2

calculate_hyperparameters <-function(sample_input, y_mean_input) {
  shape <-sample_input / 2 + 1
  rate <-sample_input * y_mean_input / 2
  hyperparameter_vector <- c(shape, rate)
  return(hyperparameter_vector)
}

y_mean <-500
n <-3

hyperparameters <-calculate_hyperparameters(n, y_mean)
thetas <- seq(0, 0.01, by = 0.0001) 
plot(NULL, xlim = c(0,0.01), ylim = c(0,1000), ann = FALSE)
title(main="Likelihood and MLE (n = 3)", xlab = "Theta", ylab = "")
lines(thetas, dgamma(thetas, shape = hyperparameters[1], rate = hyperparameters[2]), col = "blue")
abline(v = 1 / y_mean, lty=2)

# b)
ns <-c(3,5,25)
hyperparameters_1 <-calculate_hyperparameters(5, y_mean)
hyperparameters_2 <-calculate_hyperparameters(25, y_mean)

plot(NULL, xlim = c(0,0.01), ylim = c(0,1000), ann = FALSE)
title(main="Likelihood and MLE (multiple samples)", xlab = "Theta", ylab = "")
lines(thetas, dgamma(thetas, shape = hyperparameters[1], rate = hyperparameters[2]), col = "blue")
lines(thetas, dgamma(thetas, shape = hyperparameters_1[1], rate = hyperparameters_1[2]), col = "red")
lines(thetas, dgamma(thetas, shape = hyperparameters_2[1], rate = hyperparameters_2[2]), col = "purple")
legend(x = 0.008, y = 800, legend = c("n=3", "n=5", "n=25"), fill = c("red", "blue", "purple"), cex = 0.7)
abline(v = 1 / y_mean, lty=2)

# c)
# Same plot but with prior included
plot(NULL, xlim = c(-0.0001,0.01), ylim = c(0,1000), ann = FALSE)
title(main="Likelihoods, MLE and prior", xlab = "Theta", ylab = "")
lines(thetas, dgamma(thetas, shape = 0.02, rate = 0.02), col = "green")
lines(thetas, dgamma(thetas, shape = hyperparameters[1], rate = hyperparameters[2]), col = "blue")
lines(thetas, dgamma(thetas, shape = hyperparameters_1[1], rate = hyperparameters_1[2]), col = "red")
lines(thetas, dgamma(thetas, shape = hyperparameters_2[1], rate = hyperparameters_2[2]), col = "purple")
legend(x = 0.008, y = 800, legend = c("prior", "n=3", "n=5", "n=25"), fill = c("green", "red", "blue", "purple"), cex = 0.7)
abline(v = 1 / y_mean, lty=2)

# The uncertainty decreases as the sample size grows

### Ex. 2
# a) b) and c)
# We can observe the posterior (and its rate and shape hyperparameters) from exercise 2 as the product of the prior and the likelihood
given_alpha <-0.02
given_beta <-0.02

calculate_posterior_parameters <-function(given_alpha, given_beta, sample_input, y_mean_input){
  shape <-given_alpha + sample_input / 2
  rate <-given_beta + sample_input / 2 * y_mean_input
  parameter_vector <- c(shape, rate)
  return(parameter_vector)
}

posterior_parameters <-calculate_posterior_parameters(0.02, 0.02, ns[1], y_mean)
posterior_parameters_1 <-calculate_posterior_parameters(0.02, 0.02, ns[2], y_mean)
posterior_parameters_2 <-calculate_posterior_parameters(0.02, 0.02, ns[3], y_mean)

plot(NULL, xlim = c(-0.0001,0.01), ylim = c(0,1000), ann = FALSE)
title(main="Posteriors", xlab = "Theta", ylab = "")
lines(thetas, dgamma(thetas, shape = 0.02, rate = 0.02), col = "green")
lines(thetas, dgamma(thetas, shape = posterior_parameters[1], rate = posterior_parameters[2]), col = "blue")
lines(thetas, dgamma(thetas, shape = posterior_parameters_1[1], rate = posterior_parameters_1[2]), col = "red")
lines(thetas, dgamma(thetas, shape = posterior_parameters_2[1], rate = posterior_parameters_2[2]), col = "purple")
legend(x = 0.008, y = 800, legend = c("prior", "n=3", "n=5", "n=25"), fill = c("green", "red", "blue", "purple"), cex = 0.7)
abline(v = 1 / y_mean, lty=2)

# Again, it can be observed that the uncertainty decreases when the sample size increases.


