#
# Worked through examples from "R for Data Science" book
#
# Location: /Users/raymondtse/Dropbox/Analysis/Books/RForDataScience/RForDataScienceWorkings01.r
# First created: 08:11 - Thursday 6 July 2017
# Last modified: 08:11 - Thursday 6 July 2017
#

### System time ###
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

#install.packages("hexbin")
library(tidyverse)
library(nycflights13)
library(ggstance)
library(lvplot)
library(hexbin)
library(modelr)
library(stringr)

mpg

### ggplot
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=cyl, y=hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=class, y=drv))

### Aesthetics
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

### Facets
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

### Geometric objects
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

### Statistical transformations

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

### Chapter 3: Data transformation with dplyr
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

### Chapter 5: Exploratory Data Analysis

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

# Two categorical variables
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

# Patterns and Models
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

# RStudio projects
ggplot(diamonds, aes(carat, price)) +
  geom_hex()
ggsave("diamonds.pdf")

write_csv(diamonds, "diamonds.csv")

# Tidy data with tidyr

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

# Relational data with dplyr

# Mutating joins
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

# Strings with stringr
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
