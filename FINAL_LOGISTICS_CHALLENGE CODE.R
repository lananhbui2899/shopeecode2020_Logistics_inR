setwd("D:/0. Study hard/DAS DA in practice/shopee code 2020")
df3<- read.csv("delivery_orders_march.csv")
df3 <- df[1:100,]


#Khai bao thu vien
library(tidyverse)
library(lubridate)

head(df3)
glimpse(df3)
#Convert numeric to date with tz=local time
make_dt <- function(x){
  x <- as_date(as_datetime(x, tz="ETC/GMT-8",origin = lubridate::origin))}

df3[,2:4] <- lapply(df3[,2:4],make_dt)
str(df3[,2:4])

#Time difference between two time
`%notin%` <- negate(`%in%`)
holidays <- as.Date(c("2020-03-08","2020-03-25",
                      "2020-03-30","2020-03-31"))
for (i in 1:length(df3[,2])){
  thegap= seq(df3[i,2],df3[i,3],by = "day")
  includ <- thegap!=df3[i,2]&
    (thegap %notin% holidays)&
    (as.POSIXlt(thegap)$wday!=0)
  df3$time_diff1[i] <- length(thegap[includ])
}

for (i in 1:length(df3[,3])){
  if (is.na(df3[i,4])==FALSE){
  thegap= seq(df3[i,3],df3[i,4],by = "day",na.rm=T)
  includ <- thegap!=df3[i,3]&
    (thegap %notin% holidays)&
    (as.POSIXlt(thegap)$wday!=0)
  df3$time_diff2[i] <- length(thegap[includ])
  }else{
    df3$time_diff2[i] <- NA
  }
}

#Text Cleaning- address
for (i in 1:length(df3[,5])){
  df3[i,5] <- tolower(word(df3[i,5],-1))}
for (j in 1:length(df3[,6])){
  df3[j,6] <- tolower(word(df3[j,6],-1))}

#Results
for (i in 1:length(df3[,5])){
  if(df3[i,5]=="manila"& df3[i,6]=="manila"){
    df3$exp_de[i] <- 3
    }else if((df3[i,5]=="manila"|df3[i,5]=="luzon")&
    (df3[i,6]=="manila"|df3[i,6]=="luzon")){
      df3$exp_de[i] <- 5
    } else {df3$exp_de[i] <- 7}
}

df3$latepick1 <- if_else(df3$time_diff1>df3$exp_de,1,0)
df3$latepick2 <- if_else(!is.na(df3$time_diff2)&df3$time_diff2>3,1,0)

for(i in 1:length(df3[,1])){
  if(sum(df3$latepick1[i],df3$latepick2[i],na.rm = TRUE)==0){
    df3$final_result[i] <- 0
  }else{
    df3$final_result[i] <- 1
  }
}
df <- df3 %>% select(orderid,final_result)
