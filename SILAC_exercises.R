# SILAC Analysis (Stable Isotope Labelling in Active Cells)
# Rick Scavetta
# 26 Sept 2019
# Case study for workshop

# clear workspace
rm(list = ls())

# Load packages
# Includes dplyr, ggplot2 and other packages
library(tidyverse)

# Read in the data:
protein.df <- read.delim("Protein.txt")

# Examine the data:
summary(protein.df)
str(protein.df)
glimpse(protein.df)

# print protein.df to screen (way too much information)
protein.df

# Before proceeding, convert to a tibble
protein.df <- as_tibble(protein.df)

# not try to print again (better!)
protein.df

# Transformations
# log10 of intensities
protein.df$Intensity.H <- log10(protein.df$Intensity.H)
protein.df$Intensity.M <- log10(protein.df$Intensity.M)
protein.df$Intensity.L <- log10(protein.df$Intensity.L)

# Add log10 values
protein.df$Intensity.H.M <- protein.df$Intensity.H + protein.df$Intensity.M
protein.df$Intensity.M.L <- protein.df$Intensity.M + protein.df$Intensity.L

# log2 of ratios
protein.df$Ratio.H.M <- log2(protein.df$Ratio.H.M)
protein.df$Ratio.M.L <- log2(protein.df$Ratio.M.L)

protein.df

# Chapter 9 Exercises:
# Exercise 9.1
# How many contaminants are present in the data-set?
protein.df %>% 
  filter(Contaminant == "+") %>% 
  nrow()

summary(protein.df$Contaminant)
table(protein.df$Contaminant)

sum(protein.df$Contaminant == "+")

# What fraction of the total do they represent?
summary(protein.df$Contaminant)/length(protein.df$Contaminant)
summary(protein.df$Contaminant)/nrow(protein.df)

# Remove all contaminants (i.e. filter for valid proteins)
protein.df %>% 
  filter(Contaminant != "+") -> protein.df

# Exercise 9.2 (Find protein values)
searchProt <-  c("GOGA7", "PSA6", "S10AB")

protein.df %>% 
  filter(Uniprot %in% paste0(searchProt, "_MOUSE")) %>% 
  select(Uniprot, Ratio.H.M, Ratio.M.L)

# Exercise 10.1 (Find protein values)
# using []
protein.df[protein.df$Uniprot %in% paste0(searchProt, "_MOUSE"),
           c("Uniprot", "Ratio.H.M", "Ratio.M.L")]

# Exercise 10.4 (Find top 20 values)
# See "10.5 Ordering functions" and use arrange()
protein.df %>% 
  arrange(desc(Ratio.H.M)) %>% 
  slice(1:20)

protein.df %>% 
  top_n(20, Ratio.H.M) -> topHM

protein.df %>% 
  top_n(20, Ratio.M.L) -> topML

# Exercise 10.5 (Find intersections)
# See "10.6 Intersection functions"
intersect(topML, topHM)
union(topML, topHM)
setdiff(topML, topHM)
setdiff(topHM, topML)

xx <- 1:10
yy <- 6:15
setdiff(xx, yy)
setdiff(yy, xx)
intersect(xx, yy)
union(xx, yy)
