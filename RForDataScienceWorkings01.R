# ------------------------------------------------------------------------
# Worked through examples from "R for Data Science" book
#
# Exercise Solutions and Notes for “R for Data Science”: https://jrnold.github.io/e4qf/
#
# Location: /Users/raymondtse/Dropbox/Analysis/Books/RForDataScience/RForDataScienceWorkings01.r
# First created: 08:11 - Thursday 6 July 2017
# Last modified: 16:27 - Sunday 17 December 2017
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Install Packages
# ------------------------------------------------------------------------
#install.packages("hexbin")
library(dplyr)
library(forcats)
library(gapminder)
library(ggstance)
library(hexbin)
library(lubridate)
library(lvplot)
library(magrittr)
library(MASS)
library(modelr)
library(nycflights13)
library(purrr)
library(splines)
library(stringr)
library(tidyverse)

options(na.action = na.warn)
devtools::session_info()

# ------------------------------------------------------------------------
# ggplot
# ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=cyl, y=hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=class, y=drv))

# ------------------------------------------------------------------------
# Aesthetics
# ------------------------------------------------------------------------
ggplot (data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, size=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, alpha=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy), colour="blue")

ggplot (data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=cyl))

ggplot (data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, size=cty))

ggplot (data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape=cyl))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, stroke=cyl))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, colour=cyl < 5))

# ------------------------------------------------------------------------
# Facets
# ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_wrap(~ class, nrow = 3)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_wrap(~ cyl, nrow = 3)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_wrap(~ cty, nrow = 3)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(. ~ cty)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(drv ~ .)

# ------------------------------------------------------------------------
# Geometric objects
# ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy, linetype=drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy, colour=drv), show.legend = FALSE)

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy)) +
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(colour=class)) +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(colour=class)) +
  geom_smooth(data = filter(mpg, class=="subcompact"), se=FALSE)

# ------------------------------------------------------------------------
# Statistical transformations
# ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut))

ggplot(data = diamonds) +
  stat_count(mapping = aes(x=cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, y=..prop.., group=1))

ggplot(data = diamonds) + 
  stat_summary( mapping = aes(x=cut, y=depth),
                fun.ymin = min,
                fun.ymax = max,
                fun.y = median
                )

ggplot(data = diamonds) +
  geom_pointrange(mapping = aes(x=cut, y=median(depth), ymin=min(depth), ymax=max(depth), group=1))

# Most geoms and stats come in pairs that are almost always used in concert. 
# Read through the documentation and make a list of all the pairs. 
# What do they have in common?
#
# geom_abline - NA
# geom_area - identity
# geom_bar - count
# geom_bin2d - bin2d
#

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, y=..prop.., group=1))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, colour=cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, fill=cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity))

ggplot(data = diamonds, mapping = aes(x=cut, fill=clarity)) +
  geom_bar(alpha=1/5, position="identity")

ggplot(data = diamonds, mapping = aes(x=cut, colour=clarity)) +
  geom_bar(fill=NA, position="identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut, fill=clarity), position="fill")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut, fill=clarity), position="dodge")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position = "jitter")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_count()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "This Box Plot")

# ------------------------------------------------------------------------
# Chapter 3: Data transformation with dplyr
# ------------------------------------------------------------------------
flights

filter(flights, month == 1, day == 1)
filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11,12))
filter(flights, arr_delay <= 120, dep_delay <= 120)

filter(flights, arr_delay >= 120)
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, carrier %in% c("UA", "AA", "DL"))
filter(flights, month %in% c(7,8,9))
filter(flights, dep_delay <= 0, arr_delay >= 120)
filter(flights, dep_delay >= 60, arr_delay <= dep_delay - 30)
filter(flights, dep_time >= 0, dep_time <= 600)

filter(flights, between(dep_time, 0, 600))

filter(flights, is.na(dep_time))

arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
arrange(flights, dep_time)
arrange(flights, air_time)

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())
select(flights, air_time, time_hour, air_time)
select(flights, contains("TIME"))

flights_sml <- select(flights, year:day,
                      ends_with("delay"),
                      distance,
                      air_time)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

summarise(flights, delay=mean(dep_delay, na.rm = TRUE))

summarise(group_by(flights, year, month, day), delay = mean(dep_delay, na.rm = TRUE))

delays <- flights %>%
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")

ggplot(data = delays, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size=count), alpha = 1/3) +
  geom_smooth(se = FALSE)

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(bindwidth = 10)

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(first = min(dep_time),
            last = max(dep_time)
  )

not_cancelled %>%
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>% count(dest)

not_cancelled %>% 
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time <500))

daily_summary <- flights %>%
  group_by(year, month, day) %>%
  summarise(
    n = n(),
    prop_cancelled = sum(is.na(dep_time)) / n,
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )
  
ggplot(data = daily_summary, mapping = aes(x = avg_delay, y = prop_cancelled)) +
  geom_point(aes(size = n), alpha = 1/10) +
  geom_smooth(se = FALSE)

flights %>%
  group_by(hour) %>%
  summarise(
    n = n(),
    prop_cancelled = sum(is.na(dep_time)) / n,
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

# ------------------------------------------------------------------------
# Chapter 5: Exploratory Data Analysis
# ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat, binwidth = 0.5))

diamonds %>% count(cut_width(carat, 0.5))

smaller <- diamonds %>% filter(carat <3)
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = smaller) +
  geom_freqpoly(mapping = aes(x = carat, colour = cut), binwidth = 0.2)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y == 0 | y > 30) %>% 
  arrange(y)
unusual

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) +
    geom_freqpoly(
      mapping = aes(colour = cancelled),
      binwidth = 1/4
    )

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = color), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  ) +
  coord_flip()

ggplot(data = diamonds, mapping = aes(x = color, y = price)) +
  geom_point()

ggplot(data = mpg) +
  geom_boxploth(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  )

ggplot(data = diamonds) +
  geom_lv(mapping = aes(x = cut, y = price))

ggplot(data = diamonds) +
  geom_violin(mapping = aes(x = cut, y = price)) +
  geom_boxplot(mapping = aes(x = cut, y=price), width = 0.1)

# ------------------------------------------------------------------------
# Two categorical variables
# ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_count(mapping = aes(x=cut, y=color))

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

# ------------------------------------------------------------------------
# Patterns and Models
# ------------------------------------------------------------------------
ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))

ggplot(data = diamonds2) +
  geom_hex(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) +
  geom_boxplot(mapping = aes(x = cut, y = resid))

# ------------------------------------------------------------------------
# RStudio projects
# ------------------------------------------------------------------------
ggplot(diamonds, aes(carat, price)) +
  geom_hex()
ggsave("diamonds.pdf")

write_csv(diamonds, "diamonds.csv")

# ------------------------------------------------------------------------
# Tidy data with tidyr
# ------------------------------------------------------------------------
table4a <- tribble (~country, ~"1999", ~"2000",
                    #-------|------|-------
                    "Afghanistan", 745, 2666,
                    "Brazil", 37737, 80488,
                    "China", 212258, 213766
)
  
tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

table4b <- tribble (~country, ~"1999", ~"2000",
                    #-------|------|-------
                    "Afghanistan", 19987071, 20595360,
                    "Brazil", 172006362, 174504898,
                    "China", 1272915272, 1280428583
)

tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")
  
left_join(tidy4a, tidy4b)

table2 <- tribble (~country, ~year, ~type, ~count,
                   #-------|------|------|-------
                   "Afghanistan", 1999, "cases", 745,
                   "Afghanistan", 1999, "population", 19987071,
                   "Afghanistan", 2000, "cases", 2666,
                   "Afghanistan", 2000, "population", 20595360,
                   "Brazil", 1999, "cases", 37737,
                   "Brazil", 1999, "population", 172006362,
                   "Brazil", 2000, "cases", 80488,
                   "Brazil", 2000, "population", 174504898,
                   "China", 1999, "cases", 212258,
                   "China", 1999, "population", 1272915272,
                   "China", 2000, "cases", 213766,
                   "China", 2000, "population", 1280428583
)

spread(table2, key = type, value = count)

table3 <- tribble(~country, ~year, ~rate,
                 #-------|------|------
                 "Afghanistan", 1999, "745/19987071",
                 "Afghanistan", 2000, "2666/20595360",
                 "Brazil", 1999, "37737/172006362",
                 "Brazil", 2000, "80488/174504898",
                 "China", 1999, "212258/1272915272",
                 "China", 2000, "213766/1280428583"
)

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/", convert = TRUE)

table3 %>%
  separate(year, into = c("century", "year"), sep = 2)

# Case Study
who1 <- who %>%
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)

who1 %>% 
  count(key)

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- who2 %>% 
  separate(key, c("new", "type", "agesex"), sep = "_")

who4 <- who3 %>% 
  select (-iso2, -iso3, -new)

who5 <- who4 %>% 
  separate(agesex, c("sex", "age"), sep = 1)

who5 %>% 
  count(country, year, sex)

# ------------------------------------------------------------------------
# Relational data with dplyr
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Mutating joins
# ------------------------------------------------------------------------
flights2 <-  flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>% 
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

flights2 %>% 
  left_join(weather)

flights2 %>% 
  left_join(planes, by = "tailnum")

flights2 %>% 
  left_join(airports, c("dest" = "faa"))

airports %>% 
  semi_join(flights, c("faa" = "dest")) %>% 
  ggplot(aes(lon, lat)) +
  borders("state") + 
  geom_point() +
  coord_quickmap()

flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

# ------------------------------------------------------------------------
# Strings with stringr
# ------------------------------------------------------------------------
str_length( c(" a", "R for data science", NA))

str_c("x", "y", "z", sep = "   ")

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

str_view(x, "an")
str_view(x, ".a.")
str_view(x, "\\d")
str_view(x, "^....$")
str_view(x, "[^abc]")
str_view(x, "^(a|e|i|o|u)")
str_view(x, "[aeiou]")

str_view( c(" grey", "gray"), "gr(e|a)y")

y <- "\"\'\\\\"
writeLines(y)

str_view(stringr::words, "^y", match = TRUE)

str_view(x, "(..)\\1", match = TRUE)

str_detect(x, "e")
sum(str_detect(words, "^t"))
mean(str_detect(words, "[aeiou]$"))

no_vowels_1 <- !str_detect(words,"[aeiou]")
str_view(words, "^[^aeiou]+$", match = TRUE)

str_subset(words, "x$")
str_view_all(x, "an")

str_view(words, "^h|h$", match = TRUE)
str_view(sentences, "s |s.|s,", match = TRUE)

noun <- "(a|the|A|The) ([^ ]+)"
has_noun <- sentences %>% 
  str_subset(noun) %>% 
  head(10)
has_noun %>% 
  str_extract(noun)

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)

# ------------------------------------------------------------------------
# Factors with forcats
# ------------------------------------------------------------------------
gss_cat %>% 
  count(race)

gss_cat %>% 
  count(relig)

relig <- gss_cat %>% 
  group_by(relig) %>% 
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig, aes(tvhours, relig)) +
  geom_point()

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

# ------------------------------------------------------------------------
# Dates and times with lubridate
# ------------------------------------------------------------------------
today()
now()

ymd("2017-11-24")
ymd(20171212)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(
    departure = make_datetime(year, month, day, hour, minute)
  )

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
  
flights_dt %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400)

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600)

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) + 
  geom_freqpoly(binwidth=300)

# ------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])
commas(letters)
commas(letter, collapse = "-")

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important Output")
rule("How does this work", "with multiple", 56)

# ------------------------------------------------------------------------
# Vectors
# ------------------------------------------------------------------------
x <- as.Date("1969-12-12")
unclass(x)
typeof(x)
attributes(x)
attr(x, "tzone")

# ------------------------------------------------------------------------
# Chapter 17: Iteration with purrr
# ------------------------------------------------------------------------
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

for (i in seq_along(df)) {
  df[[i]] <-rescale01(df[[i]])
}
df

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

#
# Switching to my preferred method of naming variables
# However code copied directly from the text will be typed verbatim
#
outputMeans <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  outputMeans[[i]] <- mean(mtcars[[i]])
}
outputMeans

outputTypes <- vector("character", ncol(flights))
for (i in seq_along(flights)) {
  outputTypes[[i]] <- typeof(flights[[i]])
}
outputTypes

# ------------------------------------------------------------------------
# Part IV: Model - Chapter 18: Model basics with modelr
# ------------------------------------------------------------------------
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

sim1 <- tibble(
  x = runif(25, 0, 10),
  y = 5 + 2.5 * x + runif(25, -5, 5)
)

ggplot(sim1, aes(x,y)) +
  geom_point()

ggplot(sim1, aes(x, y)) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4
  ) +
  geom_point()

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = map2_dbl(a1, a2, sim1_dist)) 
models

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, colour = "red"
  ) +
  geom_point(aes(colour = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
  ) %>% 
  mutate(dist = map2_dbl(a1, a2, sim1_dist))

ggplot(grid, aes(a1, a2))+
  geom_point(
    data = filter(grid, rank(dist) <= 10),
    size = 4, colour = "red"
  ) +
  geom_point(aes(colour = -dist))
  
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0,0), measure_distance, data = sim1)  
str(best)  
best$par  

ggplot(sim1, aes(x,y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)  
str(sim1_mod)  
coef(sim1_mod)
  
# ------------------------------------------------------------------------
# Visualising models
# ------------------------------------------------------------------------
grid <- sim1 %>% 
  data_grid(x)
grid

grid <- grid %>% 
  add_predictions(sim1_mod)
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) + 
  geom_line(
    aes(y = pred),
    data = grid, 
    colour = "red",
    size = 1
  )

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(resid)) + 
  geom_histogram(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

ggplot(sim2) +
  geom_point(aes(x,y))

model_matrix(sim2, y ~ x)
mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(
    data = grid, 
    aes(y = pred),
    colour = "red",
    size =4
  )

mySim <- tibble(
  y = runif(10, 0, 1),
  x1 = runif(10, 0, 1),
  x2 = runif(10, 0, 1)
)

model_matrix(mySim, y ~ x1 + x2)
model_matrix(mySim, y ~ x1 * x2)
mySimModel <- lm(y ~ x1 * x2, data = mySim)
ggplot(mySim, aes(x1, y)) +
  geom_point(aes(colour = x2))

# ------------------------------------------------------------------------
# Interactions (Continuous & Categorical)
# ------------------------------------------------------------------------
ggplot(sim3, aes(x1, y)) +
  geom_point(aes(colour = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, colour = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

ggplot(sim3, aes(x1, y, colour = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(model ~ x2)

sim3  <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

ggplot(sim3, aes(x1, resid, colour = x2)) +
  geom_point() +
  facet_grid(~ model)

# ------------------------------------------------------------------------
# Interactions (Two Continuous)
# ------------------------------------------------------------------------
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>% 
  gather_predictions(mod1, mod2)
grid

ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model)

ggplot(grid, aes(x2, pred, colour = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)

# ------------------------------------------------------------------------
# Transformations
# ------------------------------------------------------------------------
sim5 <- tibble (
  x = seq(0, 3.5* pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x,y)) +
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)

# ------------------------------------------------------------------------
# Chapter 19: Model building
# ------------------------------------------------------------------------
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()
ggplot(diamonds, aes(carat, price)) + geom_hex(bins = 50)

diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, colour = "red", size = 1)

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity,
                   data = diamonds2)

# ------------------------------------------------------------------------
# What affects the daily number of flights ?
# ------------------------------------------------------------------------
daily <- flights %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>% 
  summarise(n = n())

ggplot(daily, aes(date, n)) +
  geom_line()

daily <- daily %>%
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
  geom_boxplot()

mod <-lm(n ~ wday, data = daily)

grid <- daily %>%
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() + 
  geom_point(data = grid, colour = "red", size = 4)

daily <- daily %>% 
  add_residuals(mod)
ggplot(daily, aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line()

ggplot(daily, aes(date, resid, colour = wday)) +
  geom_ref_line(h = 0) +
  geom_line()

daily %>% 
  filter(resid < -100)

ggplot(daily, aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line(colour = "grey50") +
  geom_smooth(se = FALSE, span = 0.2)

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() +
  geom_line() +
  scale_x_date(NULL,
               date_breaks = "1 month",
               date_labels = "%b")

term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
  )
}

daily <- daily %>% 
  mutate(term = term(date))

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(NULL,
               date_breaks = "1 month",
               date_labels = "%b")

ggplot(daily, aes(wday, n, colour = term)) +
  geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, colour = "red") +
  facet_wrap(~ term)

mod3 <- MASS::rlm(n ~ wday * term, data = daily)  

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, colour = "white") +
  geom_line()

mod <-  MASS::rlm(n ~ wday * ns(date, 5), data = daily)  
  
daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) +
  geom_line() +
  geom_point()
  
# ------------------------------------------------------------------------
# Chapter 20: Many models with purrr and broom
# ------------------------------------------------------------------------
ggplot(gapminder, aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() +
  ggtitle("Full data =")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear Trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, colour = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()
  
country_model <-function(df) {
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)
  
by_country <- by_country %>% 
  mutate(model = map(data, country_model))

by_country %>% 
  filter(continent == "Europe")

by_country %>% 
  arrange(continent, country)

by_country <- by_country %>% 
  mutate(resids = map2(data, model, add_residuals))

resids <- unnest(by_country, resids)

resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE)

resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~continent)

broom::glance(nz_mod)

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

glance %>% 
  arrange(r.squared)

glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()

gapminder %>% 
  group_by(country, continent) %>% 
  nest()

gapminder %>% 
  nest(year : gdpPercap)

# ------------------------------------------------------------------------
# Part V. Communicate
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Chapter 22: Graphics for communication
# ------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with engine size"
    )
  )

best_in_class <- mpg %>% 
  group_by(class) %>% 
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_label(aes(label = model), data = best_in_class,
             nudge_y = 2, alpha = 0.5)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red",
                                 Democratic = "blue")) +
  scale_x_date(NULL,
               breaks = presidential$start,
               date_labels = "`%y")

base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right")
base + theme(legend.position = "none")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(
    nrow = 1,
    override.aes = list(size = 4)
  )
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df,aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5,7), ylim = c(10,30))

mpg %>% 
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>% 
  ggplot(aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth()
  



