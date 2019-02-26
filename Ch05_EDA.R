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

smaller %>% 
  ggplot(aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

smaller %>%
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

diamonds %>% 
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 0.5) + 
  coord_cartesian(ylim = c(0, 50))

unusual <-
  diamonds %>% 
  dplyr::filter(y < 3 | y > 20) %>% 
  arrange(y)

diamonds %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 1000)

diamonds %>% 
  count(cut_width(price, 1000))

diamonds %>% 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(0.9, 1.1))

diamonds %>% 
  ggplot() +
  geom_count(aes(x = cut, y = color))

diamonds %>%
  count(color, cut)
  