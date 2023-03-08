# ---- Estimating Population Size with a Capture-Mark-Recapture Analysis ----

# Load packages ----
library(FSA)
library(dplyr)
library(ggplot2)
# Use install.packages() to install these packages if you don't alreayd have them

# Source custom function ----
source("confint_mod.R")

# Import data ----
# Replace filename.csv with your catch data set
data <- read.csv("filename.csv")

# Summary statistics ----
# How many fish did you catch using this gear type?

# How many tagged fish did you catch using this gear type?

# What was the average total length?

# What was the average total length, by age?

# Build a summary table ----
# For each sampling event (cell_id), find
## n     : the number of fish caught
## m     : the number of tagged fish caught
# Store this table in an object called event_table
event_table <- data %>%
  group_by(cell_id) %>%
  summarise(n = n(),
            m = sum(tagged))

# Add a column called M that will represent the number of tagged
# fish in the population
event_table <- event_table %>% mutate(M = 2125)

# Generate population estimate ----
# Store these results in an object called pop_estimate
pop_estimate <- mrClosed(n = event_table$n,
                   m = event_table$m,
                   M = event_table$M,
                   method = "Schnabel")

# Use summary() to find the overall population estimate
summary(pop_estimate)

# Calculate confidence intervals using one of the two methods below
confint(popest)
confint_mod(popest)

# Sensitivity analysis ----
# Does increased sampling  = increased confidence in estimates?
est <- NULL
low <- NULL
high <- NULL
for(i in 1:nrow(event_table)) {
  est.in <- mrClosed(n = event_table$n[1:i],
           m = event_table$m[1:i],
           M = event_table$M[1:i],
           method = "Schnabel")
  est <- c(est, est.in %>% summary())
  low.in <- confint(est.in)[1]
  high.in <- confint(est.in)[2]
  low <- c(low, low.in)
  high <- c(high, high.in)
}
results <- data.frame(event = 1:nrow(event_table),
           est,
           low,
           high)

ggplot(results) +
  geom_point(aes(x = event, y = est)) +
  geom_errorbar(aes(x = event, ymin = low, ymax = high)) +
  theme_bw() +
  labs(x = "Sampling Event", y = "Population Estimate") +
  geom_hline(yintercept = 16350, linetype = "dashed", col = 'red') +
  geom_hline(yintercept = summary(pop_estimate), linetype = "dashed", col = 'blue')



# Length histogram ----
ggplot(data) + 
  geom_histogram(aes(x = total_length_cm, fill = gear)) +
  theme_bw() +
  labs(x = "Total Length (cm)")
