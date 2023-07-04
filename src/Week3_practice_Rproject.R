df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
df %>% dplyr::glimpse() 
names(df)
# extract column from dataframe
df$thermal_time
df[,3]
df[,'thermal_time']

df[['thermal_time']]

# not work
df[thermal_time]
# different error message
#!! name space conflict
df[time]
time

# summarize dataframe
lapply(df, range)
# turn as data frame
lapply(df, range) %>% data.frame()

summary(df)


df <- read.csv('./data/ear_summarized.csv')
nrow(df)
ncol(df)
lapply(df, unique)

#individual dates
df$date #11 idividual dates
#using nrow()
unique_dates <- df %>%
  distinct(date) %>%
  nrow()
#using length()
unique_dates <- length(unique(df$date))
print(unique_dates)

#glimpse vs. str
df %>% dplyr::glimpse()
df %>% str() #glimpse seems a little better organized in return

#column weight
#using []
weight_column <- df[, "weight"]
#using [[]]
weight_column <- df[["weight"]]
#using $
weight_column <- df$weight
#output variable
print(weight_column)

#functions head and tail
head(df)
tail(df)
#head outputs the first lines in a column, tail outputs the last
first_three_rows <- df[1:3,]
print(first_three_rows)


#1_dplyr

df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))

#1.1_Subset rows

df$time %>% str()
df %>% dplyr::filter(time=='2023-04-17') %>% .[["temp"]]
df %>% dplyr::filter(time==as.Date('2023-04-17')) %>% .$temp

#1.2_Add columns
# result is not save
df %>% dplyr::mutate(Year="2023") #won't be saved
df <- df %>% dplyr::mutate(Year="2023") #will be saved
df
# result is saved (R_based)
df$Year <- "2023"
df[['Year']] <- "2023"
df

#1.3_Combine_dataframes_by _columns

df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
# with same length dataframe
ear_df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                     ear_weight=c(20,40,50))
merge(df,ear_df,by="time")
dplyr::left_join(df,ear_df,by="time")
# combind with vector of same length 
cbind(df, ear_weight=c(20,40,50))
df$ear_weight <- c(20,40,50)

# with differnt length 
short_ear_df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,2,1),
                           ear_weight=c(20,40))
merge(df,short_ear_df,by="time")
dplyr::left_join(df,short_ear_df,by="time")

# combind with vector of different length 
cbind(df, ear_weight=c(20,40))
df$ear_weight <- c(20,40)
