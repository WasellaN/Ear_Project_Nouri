load('.RData')
library(dplyr)
library(ggplot2)

climate %>% 
  select(ends_with("Temperature")) %>% 
  head(.,3) %>%
  glimpse()


climate %>% 
  mutate(across(where(is.numeric),~{round(.x, digits = 2)})) %>%
  # mutate(across(where(is.numeric),function(x){round(x, digits = 2)})) %>%
  select(ends_with("Temperature")) %>% 
  head(.,3) %>% 
  glimpse()

# reduce your code chunk by using function
display <- function(x){
  # subset dataframe and summarized for displaying purporse
  # x: input data frame
  x %>% 
    dplyr::select(ends_with("Temperature")) %>% 
    head(.,3) %>% 
    dplyr::glimpse()
}

climate %>% 
  mutate(across(where(is.numeric),function(x){round(x, digits = 2)})) %>%
  display()


climate %>% 
  dplyr::filter(DFG_year=="DFG2019") %>% 
  group_by(y,m) %>% 
  summarise()

climate %>% 
  dplyr::filter(DFG_year=="DFG2019") %>% 
  dplyr::select(y,m) %>% 
  dplyr::distinct()

climate %>%names()

climate_long <- climate %>% 
  tidyr::pivot_longer(names_to = "Daily_Terms",
                      values_to = "Daily_value",
                      cols = contains("Daily")) 
climate_long%>% 
  names()

by_cyl <- mtcars %>% group_by(cyl)

# grouping doesn't change how the data looks (apart from listing
# how it's grouped):
by_cyl

# It changes how it acts with the other dplyr verbs:
by_cyl %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)
by_cyl %>% filter(disp == max(disp))

# Each call to summarise() removes a layer of grouping
by_vs_am <- mtcars %>% group_by(vs, am)
by_vs <- by_vs_am %>% summarise(n = n())
by_vs
by_vs %>% summarise(n = sum(n))

# To removing grouping, use ungroup
by_vs %>%
  ungroup() %>%
  summarise(n = sum(n))

# By default, group_by() overrides existing grouping
by_cyl %>%
  group_by(vs, am) %>%
  group_vars()

# Use add = TRUE to instead append
by_cyl %>%
  group_by(vs, am, .add = TRUE) %>%
  group_vars()

# You can group by expressions: this is a short-hand
# for a mutate() followed by a group_by()
mtcars %>%
  group_by(vsam = vs + am)

# The implicit mutate() step is always performed on the
# ungrouped data. Here we get 3 groups:
mtcars %>%
  group_by(vs) %>%
  group_by(hp_cut = cut(hp, 3))

# If you want it to be performed by groups,
# you have to use an explicit mutate() call.
# Here we get 3 groups per value of vs
mtcars %>%
  group_by(vs) %>%
  mutate(hp_cut = cut(hp, 3)) %>%
  group_by(hp_cut)

# when factors are involved and .drop = FALSE, groups can be empty
tbl <- tibble(
  x = 1:10,
  y = factor(rep(c("a", "c"), each  = 5), levels = c("a", "b", "c"))
)
tbl %>%
  group_by(y, .drop = FALSE) %>%
  group_rows()

by_cyl %>% summarise(
  disp = mean(disp),
  hp = mean(hp))

by_cyl %>% summarise(across(c(disp, hp),mean))

climate_SD <-
  climate %>% group_by(DFG_year, sowing_date) %>%
  slice(1) %>%
  select(DFG_year, sowing_date, DayTime)
  summarise()

climate_TT <- 
  climate %>% 
  select(DayTime, DailyMean_Temperature, DFG_year, sowing_date) %>%
  group_by(DFG_year, sowing_date) %>% 
  mutate(., ThermalTime = cumsum(DailyMean_Temperature)) 


  
  