### Ex. 2
# install.packages("foreign")
# install.packages("tidyverse")
library(foreign)
library(tidyverse)

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