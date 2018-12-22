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

# Basic plot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# Exercises 
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

