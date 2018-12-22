# ------------------------------------------------------------------------
# R for Data Science: Visualisation
#
# Link:
#
# Location:
# First created:
# Last modified:
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "Sat Dec 22 11:33:55 2018")

# ------------------------------------------------------------------------
# Install Packages
# ------------------------------------------------------------------------
library(conflicted)
library(skimr)
library(tidyverse)

# ------------------------------------------------------------------------
# Chapter 1
# ------------------------------------------------------------------------
mpg
skim(mpg)

mtcars
skim(mtcars)

# Basic plot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# Exercises: The mpg data frame
#1. 
ggplot(data = mpg)

#2. 
nrow(mtcars)
ncol(mtcars)

#3. 
? mpg

#4. 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

# Aesthetic mappings
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, size = class))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, alpha = class))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, shape = class))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy), colour = "blue")

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, colour = displ < 5))

ggplot(mtcars) +
  geom_point(aes(x = wt, y = hp))

ggplot(mtcars) +
  geom_histogram(aes(x = drat))

ggplot(diamonds, aes(carat)) +
  geom_histogram()

# From: https://www.r-bloggers.com/how-to-make-a-histogram-with-ggplot2/

chol <- read.table(url("http://assets.datacamp.com/blog_assets/chol.txt"), header = TRUE)

ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram(breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count") + 
  xlim(c(18,52)) + 
  ylim(c(0,30))

ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram(boundary = 0)

ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram(breaks=seq(20, 50, by = 1))

sum(chol$AGE < 22)

tmp <- 
  dplyr::filter(chol, AGE <= 22) %>% 
  arrange(AGE)

ggplot(tmp) +
  geom_histogram(aes(x = AGE), bins = 5)

ggplot(tmp) +
  geom_histogram(aes(x = AGE), bins = 30)

ggplot(tmp) +
  geom_histogram(aes(x = AGE), breaks = seq(18, 22, by = 1))
