# ------------------------------------------------------------------------
# R for Data Science: Chapter 5: EDA
#
# Link: 
#
# Location:
# First created: 22:17 - Tuesday 26 February 2019
# Last modified: 22:17 - Tuesday 26 February 2019
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Install Packages
# ------------------------------------------------------------------------
library(conflicted)
library(skimr)
library(tidyverse)

# ------------------------------------------------------------------------
# Visualising Distributions
# ------------------------------------------------------------------------
diamonds %>% 
  ggplot() +
  geom_bar(aes(x = cut))

diamonds %>% 
  count(cut)

diamonds %>% 
  ggplot() +
  geom_histogram(aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

smaller <- 
  diamonds %>% 
  dplyr::filter(carat < 3)

smaller %>% 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.1)


