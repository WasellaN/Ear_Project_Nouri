library(dplyr)

# working directory, abbreviated as "."
getwd()
# parent directory, abbreviated as ".."
dirname(getwd())
# assign current path to variable
current_path <- getwd()
# check the type 
current_path %>% str()


# check files in the directory

# are they different?
"." %>% list.files(path=.)
getwd() %>% list.files(path=.)

# are they different?
".." %>% list.files(path=.)
getwd() %>% dirname() %>% list.files(path=.)

# absolute path, did you get error?
"C:/Users/marse/seadrive_root/Tien-Che/My Libraries/PhD_Tien/Project/Postdoc_teaching/BSC_project_IPFS2023/data" %>% list.files(path=.)
# relative path in R base
parent_path <- getwd() 
paste0(parent_path,"/data") %>% list.files(path=.)

# R project relative path, are they different? 
"./data" %>% list.files(path=.)
"data" %>% list.files(path=.)
list.files('data')

empty_vec <- c()
length(empty_vec)
# what is the type of the empty vec?
empty_vec %>% str()

# NULL: empty 
empty_vec[1]
empty_vec[0]


vec <- c(1,3,5)
vec[1]
#reorder the vector 
vec[c(2,1,3)]
# removing the indexed elements
vec[-1]
vec[-2]

# indexing start from 1, not 0
# therefore you get, numeric(0)
vec[0]
# when access exceeding the range of a vector, what datatype do you get? 
vec[4]
vec %>% .[length(.)+1]
vec[1:4]
vec[4:1]

# find specific element or position
vec[c(F,T,F)]
vec[vec==5]
# when codition not match at all, it will return? 
vec[vec==2]
vec[c(F,F,F)]
vec %>% .[c(F)]
vec[vec=="a"]


# default str vector
letters
LETTERS
# when the query does not match, guess what will be the datatype? 
letters %>% .[.==2]
letters %>% .[c(F)]
# vector over write
vec
vec <- c(2,1,3)
vec

vec <- c(1, 2, 3, 4, 5)
logical_vec <- c(FALSE, TRUE)
subset_vec <- vec[logical_vec]
subset_vec
vec[TRUE]
vec
subset_vec


# create a simple list
list(1)
# create a simple list with name "x" for first element
list(x=1)
list(x=1)["x"]
# extract content
list(x=1)$"x"
list(x=1)[[1]]
list(x=1)[["x"]]

# extract with pipe
list(x=1) %>% .[[1]]
list(x=1) %>% .$"x"

# long list
long_list_example <- list(1,c(1,2),
                          T,c(T,T),
                          "str",c("a","b"),
                          list(1),
                          mean,data.frame())
# check structure of this list 
long_list_example %>% str()
long_list_example %>% glimpse()
long_list_example
# first list 
long_list_example[1]
# content of first list
long_list_example[[1]]
# first element of content of first list
long_list_example[[1]][1]

#Challenge (Guess Datatypes)
long_list_example[[1]][2]
long_list_example[1][1]
long_list_example[1][2]
long_list_example[2][2]
long_list_example[[2]][2]

# input is vector
c(1,4) %>% 
  lapply(.,FUN=function(x){x+3})
# input is list
list(2,4,c(1,4)) %>% 
  lapply(.,FUN=function(x){x+3})
# input has differnt type
list(2,4,c(1,4),"8") %>% 
  lapply(.,FUN=function(x){x+3}) #error, because of character instead of numeric

# create a dataframe 
df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
# another way
df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1)) 
df$temp=c(20,15,13)
df$thermal_time=cumsum(df$temp)

# third method
library(dplyr)
df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,4,1)) %>% 
  mutate(temp=c(20,15,13,16), 
         thermal_time=cumsum(temp))
df

data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),                 temp=c(20,13))
#it is not possible to create a dataframe with vectors of different lengths

df[,1]
df[1,] 
df[,3] #column thermal_time as vector

df[1,2] #temp when time is 2023-04-17

#Challenge: Transpose Dataframe

#transpose Dataframe using t() function
df_transposed <- t(df)

#print transposed dataframe
print(df_transposed)
df_transposed

# with pipe

#transpose Dataframe using t() function
df_transposed <- df %>% t()

#convert to dataframe
df_transposed <- as.data.frame(df_transposed)

#print transposed dataframe
print(df_transposed)
df_transposed

##Conclusion: You get slightly different results using pipe in the rownames

##Real Data

data <- read.csv("C:/Users/nouri/Documents/Week2_practice/Ear_development_BSC_project/data/ear_summarized.csv")
data %>% str()
data %>% glimpse()
names(data)
# extract column from dataframe
data$BBCH
data %>% unique()

# summarize dataframe
lapply(data, range)
# turn as data frame
lapply(data, range) %>% data.frame()

summary(data)

install.packages('ggplot')
install.packages('ggplot2')


library(ggplot2)

data %>% 
  ggplot(aes(x=date,y=weight,color=var))+
  geom_point()+
  geom_line(aes(group=group))+ # link the point by group.
  xlab("date of harvest")+ #x axis title
  ylab("ear weight(g)")+   #y axis title
  theme_gray()
  guides(color=guide_legend(title="Cultivar")) #change legend title 
  
