load('.RData')
install.packages('tidyr')
library(dplyr)
df <- expand.grid(x=letters[1:4],
                  y=1:2)

df%>% mutate(paste(x,y))
df%>% mutate(z=paste(x,y))
df%>% mutate(z=paste(x,y,sep = "-"))
df %>% tidyr::unite(data = .,col = "z",c(x,y))
df <- df %>% mutate(z=interaction(x,y))

# add identifier based on row numbers
df %>% mutate(id=1:n())
df <- df %>% mutate(id=1:nrow(.))
# row names
rownames(df)
df %>% filter()
rownames(df) <- LETTERS[1:nrow(df)]
rownames(df)

subset_df <- df[(df$x == "a" & df$y == 1) | (df$x == "c" & df$y == 2), ]
print(subset_df)

subset_df2 <- df[(df$z == "a.1") | (df$z == "c.2"), ]
print(subset_df2)

subset_df3 <- df %>% filter((x == "a" & y == 1) | (x == "c" & y == 2))
print(subset_df3)

df %>% mutate(k=ifelse(x=="a","A","B"))

df %>% mutate(k=ifelse(y==1,"A","B"))

df %>% mutate(k=case_when(x=="a"~"A",
                          TRUE~"B"))

df %>% mutate(k=case_when(x=="a"~"A",
                          x=="b"~"B",
                          TRUE~"C"))
look_table <- data.frame(x=letters,
                         X=LETTERS)
merged_df <- df %>% merge(look_table)

#Using the toupper() function with normal R
vec <- c("c", "a", "b", "d")
uppercase_vec <- toupper(vec)
print(uppercase_vec)
#using the toupper() function with pipe
vec <- c("c", "a", "b", "d")
uppercase_vec <- vec %>% toupper()
print(uppercase_vec)


climate <- read.csv('./data/climate.csv')
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

summary(climate)

climate %>% group_by(DFG_year)
climate %>% group_by(sowing_date)


library(dplyr)

climate %>% group_by(DFG_year, sowing_date) %>%
  slice(1) %>%
  select(DFG_year, sowing_date, DayTime)
summarise()

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


#calculate thermal time
