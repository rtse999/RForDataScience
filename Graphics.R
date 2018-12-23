# ------------------------------------------------------------------------
# R for Data Science: Visualisation
# Focus on geom_histogram(...)
#
# Link: https://ggplot2.tidyverse.org/reference/geom_histogram.html
#
# Location:
# First created: 19:36 - Saturday 22 December 2018
# Last modified: 19:36 - Sunday 23 December 2018
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

skim(chol)

ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram(breaks=seq(18.5, 50.5, by = 1), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count") + 
  xlim(c(18,52)) + 
  ylim(c(0,25))

# Defaults to 30 bins
ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram() 

# 1 bin per year
ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram(bins = (max(chol$AGE) - min(chol$AGE)) + 1) 

# Bins = square root of number of records rounded 
ggplot(data = chol, aes(chol$AGE)) +
  geom_histogram(bins = round((nrow(chol)^0.5))) 

# Bins = square root of number of records rounded up
ggplot(data = chol, aes(chol$AGE)) +
  geom_histogram(bins = ceiling((nrow(chol)^0.5)))

ggplot(data = chol) +
  geom_histogram(aes(x = AGE), breaks = seq(min(chol$AGE), max(chol$AGE)+1, by = 1))

ggplot(data = chol) +
  geom_histogram(aes(x = AGE), binwidth = 1)

ggplot(data = chol) +
  geom_histogram(aes(x = AGE), binwidth = 5)

ggplot(data = chol) +
  geom_freqpoly(aes(x = AGE), binwidth = 1)

ggplot(data = chol) +
  geom_histogram(aes(x = AGE), breaks = seq(17.5, 22.5, by = 1))

ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram(breaks=seq(17.5, 50.5, by = 1))

sum(chol$AGE < 22)

ggplot(data = chol) +
  geom_histogram(aes(x = AGE, fill = SMOKE), binwidth = 1)

ggplot(data = chol) +
  geom_histogram(aes(x = AGE, fill = WEIGHT), binwidth = 1)

ggplot(data = chol) +
  geom_histogram(aes(x = AGE, fill = BLOOD), binwidth = 1)

ggplot(data = chol) +
  geom_histogram(aes(x = AGE, fill = MORT), binwidth = 1)
