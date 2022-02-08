### Ex. 2
# install.packages("foreign")
# install.packages("tidyverse")
# install.packages("VGAM")
library(foreign)
library(tidyverse)
library(VGAM)

# a)
result_data <-read.csv("2008ElectionResult.csv")
polling_data <-read.dta("pew_research_center_june_elect_wknd_data.dta")

# By grouping polling data by state, we can see that Alaska has no polls and Hawaii has only 1
state_poll_volumes <- polling_data %>% 
  group_by(state, .drop = FALSE) %>% 
  summarise(
    poll_volume = n()
  ) %>% 
  arrange(poll_volume)

head(state_poll_volumes, 10)

# Makes sense to remove both although Hawaii has 1 poll response
result_data <- filter(result_data, state != "Alaska" & state != "Hawaii") %>% 
  droplevels()
polling_data <-filter(polling_data, state != "alaska" & state != "hawaii")  %>% 
  droplevels()

# b)
# add polls to vector
liberal_polling_data <-filter(polling_data, ideo == "very liberal")
liberal_polls_per_state <-as.data.frame(table(liberal_polling_data$state))

liberal_polls_per_state$poll_volume <- as.integer(table(polling_data$state))
liberal_polls_per_state <-liberal_polls_per_state %>% 
  rename(
    state = Var1,
    liberal_poll_volume = Freq)

liberal_polls_per_state <-mutate(liberal_polls_per_state,
       liberal_poll_share = liberal_poll_volume / poll_volume
)

liberal_polls_per_state

# c)
states  <-as.data.frame(state.abb[state.abb != 'AK' & state.abb != 'HI'])
print(states)
# Need to add Washington DC as DW after Delaware
washington <-c("DW")
all_states <-rbind(states, washington)
all_states <-c(all_states$`state.abb[state.abb != "AK" & state.abb != "HI"]`)
all_states <-sort(all_states)

liberal_polls_per_state$state_code <-all_states

# Add Obama results
liberal_polls_per_state$obama_result_share <- result_data$vote_Obama_pct / 100

liberal_polls_per_state

# Plot
plot(liberal_polls_per_state$liberal_poll_share,
     liberal_polls_per_state$obama_result_share,
     ylim = c(0,1),
     type = "n",
     ann = FALSE)
text(liberal_polls_per_state$liberal_poll_share,
     liberal_polls_per_state$obama_result_share,
     labels = liberal_polls_per_state$state_code,
     cex = 0.5)
title(main="Liberal poll % vs Obama vote %", xlab = "Liberal poll %", ylab = "Obama vote share")

# d)
plot(liberal_polls_per_state$poll_volume,
     liberal_polls_per_state$liberal_poll_share,
     type = "n",
     ann = FALSE)
text(liberal_polls_per_state$poll_volume,
     liberal_polls_per_state$liberal_poll_share,
     labels = liberal_polls_per_state$state_code,
     cex = 0.5)
title(main="Poll size vs liberal %", xlab = "poll volume", ylab = "liberal %")

## Ex 3.
# a) Fixing the beta distribution parameters leads to prior predictive distribution 
# that is beta-binomial with parameters n, alpha = 1, beta = 1 or any fixed value
# This is because the prior is beta distributed and the sampling is binomial

# b)
# When calculating a_hat and b_hat we know their rough range so we can set a = 10 and b = 100 for the optimizer's initial values
# I use these instead of provided a = 1 & b = 10
neg_log_likelihood <- function(a, b){-sum(dbetabinom.ab(x = liberal_polls_per_state$liberal_poll_volume,
                     size = liberal_polls_per_state$poll_volume,
                     shape1 = a, 
                     shape2 = b,
                     log = TRUE, 
                     limit.prob = 0.5))
}

mles <- mle(minuslogl = neg_log_likelihood, start = list(a = 10, b = 100), method = "L-BFGS-B")
mles
# Coefficients are 15.6 and 317.5
a_hat <-15.6
b_hat <-317.5

# c)
# From R exercise 2.1 we know that the posterior is beta(a+y, b+n-y)
# (polling prior (observed parameters) is beta distributed since it models P)
# Beta models probability of success with a-1 representing successes and b-1 representing failures
# That means the CDF's area under the curve represents the probability that a probability is something
# Binomial models number of successes

# Compute posterior means for share of liberals for each state with polling
# Mean of beta distribution is a / (a + b)
liberal_polls_per_state$posterior_mean <- (a_hat + liberal_polls_per_state$liberal_poll_volume)/ (a_hat + liberal_polls_per_state$liberal_poll_volume +
            b_hat + liberal_polls_per_state$poll_volume -liberal_polls_per_state$liberal_poll_volume)

plot(liberal_polls_per_state$poll_volume,
     liberal_polls_per_state$posterior_mean,
     type = "n",
     ann = FALSE)
text(liberal_polls_per_state$poll_volume,
     liberal_polls_per_state$posterior_mean,
     labels = liberal_polls_per_state$state_code,
     cex = 0.5)
