# load df
df = read.csv("C:/Users/billl/Documents/School/Babson/Fall 22 Classes/Machine Learning/Data/ToyotaCorolla.csv")
# view df
View(df)
# Met_color to logical
df$Met_Color = as.logical(df$Met_Color)
library(dplyr)
df = df %>% mutate(Met_Color = as.logical(Met_Color))
# slicing
df[1000,c("HP")]
86
# row 123, col. 4
df[123,4]
# row 1-10, all cols.
df[1:10,]
