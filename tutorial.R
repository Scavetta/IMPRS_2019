# Intro to R
# Rick Scavetta
# 25 Sept 2019
# DAwR workshop for IMPRS-LS

# Clear workspace
rm(list = ls())

# Load packages (each session)
# First install (only once)
library(tidyverse)

# R syntax
n <- log2(8) # the log2 of 8

# The PlantGrowth dataset
# Built-in:
PlantGrowth

# First, make it a "tibble"
# not necessary, but makes for nice output
PlantGrowth <- as_tibble(PlantGrowth)

# now, print to the screen again:
PlantGrowth

# Descriptive stats:
# What are the groups?
levels(PlantGrowth$group)

# What is the (global) mean?
mean(PlantGrowth$weight)

# What are the group-wise means?
# use dplyr functions in tidyverse
# use shift + ctrl + m to get %>%
# "the pipe operator", say "... and then ..."
PlantGrowth %>% 
  group_by(group) %>% 
  summarise(avg = mean(weight),
            stdev = sd(weight)) -> PG_summary

# Transformation: group-wise z-score
PlantGrowth %>% 
  group_by(group) %>% 
  mutate(z_score = scale(weight))

# Data viz:
# use ggplot2 from tidyverse
# 3 essential layers
# 1 - The data (PlantGrowth)
# 2 - Aesthetic mappings (variables mapped onto scales)
#   - use aes() to define
# 3 - Geometry (how the plot will actually look)
#   - all begin with geom_

# "dotplot"
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_jitter(width = 0.1, alpha = 0.6)

# boxplot
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot()

#########################################################
########################### Exercise ####################
# Plot the individual points AND mean and sd:
ggplot(PlantGrowth, aes(group, weight)) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  geom_pointrange(stat = aes(y = avg, 
                      ymin = avg - stdev, 
                      ymax = avg + stdev), 
                  PG_summary,
                  color = "red")

# Inferential stats:
# t-tests:
# Typically, use t.test(), but here, we'll use
# another method because of the structure of our data
# First, define a linear model
PG_lm <- lm(weight ~ group, PlantGrowth)
PG_lm

# t-tests and ANOVA are visible here:
summary(PG_lm)
# p-values are in the Pr(>|t|) column

# get only ANOVA:
anova(PG_lm)

#########################################################
########################### Exercise ####################
# Perform ALL pair-wise t-tests:
# i.e. Tukey's (Honest significant differences) post hoc test 
PG_aov <- aov(weight ~ group, PlantGrowth)
TukeyHSD(PG_aov)

# Two ways to do an ANOVA:
# anova(lm(y ~ x, data))
# summary(aov(y ~ x, data))
summary(PG_aov)

# Element 2: Functions (verbs)
# Everything that happens, is because of a function

# Arithmetic operators
34 + 6

# BEDMAS - order of operations
# Brackets, expon, div, mult, add, sub
2 - 3/4 # 1.25
(2 - 3)/4 # -0.25

# use objects in place of numbers
n <- 34
p <- 6
n + p

# Generic functions have form:
# fun_name(fun_args)

# call args by name or position
log2(8) # short, position
log2(x = 8) # short, name
log(x = 8, base = 2) # long, names
log(8, 2) # long, position
log(8, base = 2) # combo
log(base = 2, x = 8) # long, names
log(base = 2, 8) # confusion :/
log(8, b = 2) # partial name matching :/

# funs can have 0 to many unnamed args
ls()
list.files()
getwd()

# args can be named or unnamed
# e.g. "combine" c()
xx <- c(3, 8, 9, 23)
xx

myNames <- c("healthy", "tissue", "quantity")
myNames

# Make a regular sequence with seq()
seq(from = 1, to = 100, by = 7)
foo1 <- seq(1, 100, 7)
foo1

# Use objects in functions
foo2 <- seq(1, n, p)
foo2

# Regular sequence of 1
# Use the : operator
1:10
# A short cut for
seq(1, 10, 1)

# Two major types of math functions:
# Aggregration functions
# output is a single value (mostly)
# mean, sd, median, IQR, max, min

# Transformation functions
# output is a long as input
# z-score, log, log2, log10, sqrt, +

foo2 + 100 # transformation
# c(foo2, 100)
foo2 + foo2 # transformation
# foo2 %o% foo2
sum(foo2) + foo2 # aggregration, trans
1:3 + foo2 # transformation

########### VECTOR RECYCLING!!!! ###########

# What if:
c(100, 1) + foo2
6:7 + foo2
1:4 + foo2

# Different kinds of messages
# message - just information
# warning - maybe something went wrong
# error - full stop

# just add 100 to the first 4 values
foo2_new <- foo2
foo2_new[1:4] <- foo2_new[1:4] + 100

# just add 100 to non-zero values
foo5 <- c(5,7,0,3,0,6)
foo5[foo5 != 0] <- foo5[foo5 != 0] + 100
foo5

# Element 3: Objects (nouns)
# Anything that exists is an object

# Vectors - 1 dimensional, homogenous
# e.g.
foo1 # 15-element long vector
foo2 # 6 elements
myNames # 3 elements

# Four most common
# user-defined atomic vector types
# logical - TRUE/FALSE, T/F, 1/0 (aka Boolean, binary)
# integers - whole numbers
# double - real numbers (decimal places) (aka float)
# character - anything (aka strings)

# numeric - generic number

typeof(foo2)
typeof(myNames)
foo3 <- c("Liver", "Brain", "Testes", "Muscle",
          "Intestine", "Heart")
typeof(foo3)
foo4 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
typeof(foo4)

# homogenous types:
test <- c(1:10, "bob")
typeof(test)

# you can't do math on chr
mean(test)

# solution: Coercion between types using as.*() functions
test <- as.numeric(test)

# now we can do math:
mean(test, na.rm = TRUE)

# Alternatively, ignore names
test <- c(1:10, "bob")
typeof(test)
test[test != "bob"] # does not automatically coerce type!

# How to find the offending value?
test <- c(1:10, "bob")
test[is.na(as.numeric(test))]

# Lists: 1 dimensional, heterogenous
# e.g.
typeof(PG_lm)

# how many elements
length(PG_lm)

# access object attributes
attributes(PG_lm)

# Each of the 13 elements are names
# access the names attr with the accessor 
# function
names(PG_lm)

# Any named element can be called with $
PG_lm$residuals # vector
PG_lm$model # dataframe

# class tells R functions what to do 
# to an object
class(PG_lm)
summary(PG_lm) # get t-tests and ANOVA summary

class(PlantGrowth)
summary(PlantGrowth)

# Dataframe - 2 dimensional, heterogenous
# A special class of type list
typeof(PlantGrowth)
# Rows == observations
# Columns == variables
# Every element is a vector of the same length!
# create a data frame from scratch
foo_df <- data.frame(foo4, foo3, foo2)
foo_df

# change metadata
names(foo_df) <- myNames
foo_df

# Basic functions:
dim(foo_df) # rows cols
str(foo_df)
glimpse(foo_df) # nicer output than str
summary(foo_df)

# how to access a specific column
foo_df$quantity # A numeric vector

# Element 4: Logical expressions
# Asking and combining TRUE/FALSE questions
# Relational operators: asking

# == equivalency
# != non-equivalency
# >, <, >=, <=
# !x, where x is a logical vector, give the negation of x

# Logical operators: combing
# & AND - EVERY question is TRUE
# | OR - at LEAST ONE question is TRUE
# %in% WITHIN

###########################!!!!!!!!
########################### ALWAYS get a logical vector!
# examples with foo_df
# logical data
# All healthy observations
foo_df %>% 
  filter(healthy)

# All unhealthy observations
foo_df %>% 
  filter(!healthy)

# numeric data (double or integer)
# below 10
foo_df %>% 
  filter(quantity < 10)

# tails (beyond 10 and 20)
foo_df %>% 
  filter(quantity < 10 | quantity > 20)

# impossible
foo_df %>% 
  filter(quantity < 10 & quantity > 20)

# middle (between 10 and 20)
foo_df %>% 
  filter(quantity > 10 & quantity < 20)
foo_df %>% 
  filter(quantity > 10, quantity < 20)
foo_df %>% 
  filter(between(quantity, 10, 20))

# meaningless
foo_df %>% 
  filter(quantity > 10 | quantity < 20)

# What's really happening?
foo_df$quantity < 10
foo_df$quantity > 20

# character
# No Pattern Matching!
# only heart samples
foo_df %>% 
  filter(tissue == "Heart")

# only the heart and liver samples
# basic:
foo_df %>% 
  filter(tissue == "Heart" | tissue == "Liver")

# Using a vector
foo_df %>% 
  filter(tissue %in% c("Heart", "Liver"))
foo_df %>% 
  filter(tissue %in% c("Liver", "Heart"))

# WRONG - NEVER do this!
foo_df %>% 
  filter(tissue == c("Heart", "Liver"))
foo_df %>% 
  filter(tissue == c("Liver", "Heart"))

# Heart or liver, low quantity
foo_df %>% 
  filter(tissue %in% c("Heart", "Liver"), quantity < 10)

# make labels consistent:
foo_df %>% 
  filter(tolower(tissue) %in% c("liver", "heart"))

# Element 5: Indexing
# finding information by position using []

# Vectors (1 dimensional)
foo1
foo1[6] # The 6th element
foo1[p] # The pth element
foo1[3:p] # from the 3rd to the pth element
foo1[p:length(foo1)] # the pth to the last element
foo1[p:which(foo1 == 78)] # pth to whereever 78 occurs
foo1[-(11:15)] # exclude a value with - (i.e. remove 11 - 15th)

# the pth to the last element, but exclude 11-15
# caution when combining
foo1[-(11:15)][p:length(foo1)] 
# Use two steps
foo1a <- foo1[-(11:15)]
foo1a[p:length(foo1a)] 

# Remove NAs
foo6 <- c(56, 7,85, NA, 34,12)
foo6[!is.na(foo6) & foo6 > 50]

# Use integers, objects and functions
# BUT... the exciting part is...
# LOGICAL VECTORS

# e.g. all values less than 45
foo1[foo1 < 45]

# the negation
foo1[!(foo1<45)]

# Data frames (2 dimensional)
# use [ rows , cols ]
foo_df[3,] # The 3rd row, all columns
foo_df[,3] # The 3rd column, all rows (VECTOR)
foo_df[3] # The 3rd column, all rows (DATAFRAME)

# tibble prevent conversion to a vector:
foo_df <- as_tibble(foo_df)
foo_df[,3] # The 3rd column, all rows (DATAFRAME)
foo_df[3] # The 3rd column, all rows (DATAFRAME)


foo_df[3:p,"quantity"] # 3rd to the pth row, only quantity
foo_df[3:p,3] # 3rd to the pth row, only quantity

# tissues of low quantity (< 10)
foo_df[ foo_df$quantity < 10 , "tissue" ]

foo_df %>% 
  filter(quantity < 10) %>% 
  select(tissue)

# low quantity, tissue and quantity
foo_df[ foo_df$quantity < 10 , -1 ] # remove healthy
foo_df[ foo_df$quantity < 10 , c("tissue", "quantity") ]

foo_df %>% 
  filter(quantity < 10) %>% 
  select(-healthy)

# to use position in tidyverse
foo_df %>% 
  slice(3:p) %>% 
  select(-healthy)

foo_df %>% 
  filter(quantity < 10 & healthy == TRUE)

# Examples with arrange()
foo_df %>% 
  arrange(tissue)

foo_df %>% 
  arrange(desc(tissue))

# Element 6: Factor variables (with "levels")
# categorical variable (aka qualitative, discrete)
# with "groups"

# e.g.
PlantGrowth$group

typeof(PlantGrowth$group)
class(PlantGrowth$group)
# factor is a special class of type integer

# you can see this more clearly with:
str(PlantGrowth)

# Caution!
# R likes to switch characters to factors in a data frame:
foo3
foo_df$tissue
str(foo_df)

# problem with contamination
xx <- c(23:29, "bob")
xx # character

test <- data.frame(xx)
test$xx # factor

# to do math, coerce to a numeric:
as.numeric(test$xx) # this just gives the integers

# first get characters:
as.numeric(as.character(test$xx))

# Element 7: Tidy data
# make some play data
source("PlayData.R")

# Let's see the scenarios we discussed in class:
# Scenario 1: Transformation height & width
# Just stick with messy data:
PlayData$height/PlayData$width

# For the other scenarios we need tidy data
# To get tidy data, use gather() from tidyr
# 4 arguments:
# 1 - data
# 2,3 - key,value pair - names for OUTPUT
# 4 - the ID or MEASURE variables

# using ID variables (exclude with -)
gather(PlayData, key, value, -c(type, time))
foo_df[, -3] # exclude the 3rd column

# using MEASURE variables (default)
PlayData_t <- gather(PlayData, key, value, c(height, width))

# Scenario 2: Trans time1 & time2
# one solution: "spread" the data
# Use mutate to apply a transformation function
PlayData_t %>% 
  spread(time, value) %>% 
  mutate(timediff = `2` - `1`)

# How about if I wanted to standardize all values
# for each type and key by dividing time 1
PlayData_t %>% 
  group_by(type, key) %>% 
  mutate(stand = value/value[time == 1])

# Scenario 3: Trans typeA & typeB
PlayData_t %>% 
  spread(type, value) %>% 
  mutate(typediff = A/B)

# Element 8: Split-Apply-Combine with dplyr
# The pipe operator %>%,
# The five verbs of dplyr:
# filter(),
# arrange(),
# select(),
# mutate(),
# summarise(), and
# group_by()

# Scenario 1: Aggregration across height & width
# i.e. "group" according to type and time
PlayData_t %>% 
  group_by(type, time) %>% 
  summarise(avg = mean(value))

# Scenario 2: Aggregration across time1 & time2
# i.e. "group" according to type and key
PlayData_t %>% 
  group_by(type, key) %>% 
  summarise(avg = mean(value))

# Scenario 3: Aggregration across typeA & typeB
# i.e. "group" according to time and key
PlayData_t %>% 
  group_by(time, key) %>% 
  summarise(avg = mean(value))

# can I keep the raw data *and* the aggregration value?
PlayData_t %>% 
  group_by(time, key) %>% 
  mutate(avg = mean(value))

