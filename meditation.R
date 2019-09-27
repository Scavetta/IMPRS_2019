# Meditation Case-study
# Rick Scavetta
# 27 Sept 2019

# Clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(Hmisc)

# Import the data:
medi <- read.delim("Expression.txt")

# Tidy data-set
medi.t <- gather(medi, key, value)
glimpse(medi.t)

medi.t %>% 
  separate(key, c("treatment", "gene", "time"), "_") -> medi.t

# Exercise 16.3 (Calculate Statistics)
medi.t %>%
  filter(!is.na(value)) %>% 
  group_by(treatment, gene, time) %>% 
  summarise(avg = mean(value),
            stdev = sd(value),
            n = n(),
            SEM = stdev/sqrt(n),
            lower95 = smean.cl.normal(value)[2],
            upper95 = smean.cl.normal(value)[3]) -> medi.summary

# from Hmisc:
# smean.cl.normal(1:100)[2]
# smean.cl.normal(1:100)[3]

# plot means and the 95% CI:
ggplot(medi.summary, aes(as.numeric(time), avg, color = treatment)) +
  geom_pointrange(aes(ymin = lower95, ymax = upper95)) +
  geom_line() +
  scale_x_continuous(breaks = 1:2, 
                     label = c("8:00", "16:00"), 
                     limits = c(0.8, 2.2)) +
  labs(x = "Time of day", 
       y = "Average fold-change gene expression (95CI)",
       color = "Treatment type") +
  facet_grid(cols = vars(gene)) +
  theme_classic()













