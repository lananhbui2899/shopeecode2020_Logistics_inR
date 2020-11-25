setwd("D:/0. Study hard/DAS DA in practice/shopee code 2020")
#Trial_khong chay
df<- read.csv("delivery_orders_march.csv")
df3 <- df[1:10,]
#Khai bao thu vien
library(lubridate)
library(tidyverse)
library(stringr)
library(magicfor)

df3[,2:4] <- lapply(df3[,2:4],make_dt)
#cach1 hoc tren stackoverflow
time_diff <- function(start_date, end_date, holidays,na.rm=TRUE){
  thegap= seq(from=start_date,to=end_date,by="day")
  isHoliday= thegap %in% holidays
  isSun= as.POSIXlt(thegap)$wday %in% c(0)
  na.rm=T
  return(length(thegap[exclud]))
}

#cach2 mo
`%notin%` <- negate(`%in%`)
time_diff <- function(start_date, end_date, holidays){
  for (i in 1:length(start_date)){
  thegap= seq(start_date[i],end_date[i],by = "day")
  includ <- thegap!=start_date[i]&(thegap %notin% holidays)&(as.POSIXlt(thegap)$wday!=0)
  magic_result_as_vector(length(thegap[includ]))}
}
df3 <- df3 %>% 
  mutate(timeDiff2 = time_diff(df3[,2],df3[,3],c))

for (i in 1:length(df3)){
  thegap= seq(df3[i,2],df3[i,3],by = "day")
  includ <- thegap!=df3[i,2]&(thegap %notin% c)&(as.POSIXlt(thegap)$wday!=0)
  print(length(thegap[includ]))}






length(df3[,2])
thegap=seq(df3[2,2],df3[2,3],by = "day")
includ <- thegap!=df3[2,2]&(thegap %notin% c)&(as.POSIXlt(thegap)$wday!=0)
length(thegap[includ])
for (i in 1:length(df3[,2])){
  thegap= seq(df3[i,2],df3[i,3],by = "day")
  includ <- thegap!=df3[i,2]&(thegap %notin% c)&(as.POSIXlt(thegap)$wday!=0)
  df3$time_diff1[i] <- length(thegap[includ])}


i <- i %in% 1:length(df3)
seq(df3[i,2],df3[i,3],by = "day")
#it is wat it is :(
create.calendar("shopee", 
                holidays = as.Date(c("2020-03-08",
                                               "2020-03-25",
                                               "2020-03-30",
                                               "2020-03-31")),
                weekdays =c("sunday"))
str(df3)
count <- 0
for (i in 1:length(df3)){
  if (is.bizday(seq(df3[i,2],df3[i,3],by="day"),"shopee")=T){
    count <- count + 1
  }else{
    count
  }
}
seq(df3[i,2],df3[i,3],by="day")
#test1
a <- as.Date("2020-03-02")
b <- as.Date("2020-03-09")
c <- as.Date(c("2020-03-08","2020-03-25","2020-03-30","2020-03-31"))
time_diff(a,b,c)

df3[,2:4] <- lapply(df3[,2:4],make_dt)
df3$timeDiff2 <- lapply(df3[,2:3],time_diff)
for(start_date in df3[,2]){
  print(as_date(start_date))
  break
}


lapply(df3[,2:3],time_diff)
time_diff(df3[,2],df3[,3],c)
a <- as.Date("2020-03-01")
b <- as.Date("2020-03-30")
c <- as.Date(c("2020-03-08","2020-03-25","2020-03-30","2020-03-31"))
time_diff(a,b,c)
thegap= seq(a,b,by="day")
thegap
includ <- thegap!=a&(thegap %notin% c)&(as.POSIXlt(thegap)$wday!=0)
includ





make_dt <- function(x){x <- as_datetime(x, tz="ETC/GMT-8",origin = lubridate::origin)}


as_date(1583137548,tz="ETC/GMT-8")



format()
#Official man :)
df <- read.csv("delivery_orders_march.csv")
str(df)


make_dt <- function(x){x <- as_datetime(x, 
                                        tz="ETC/GMT-8",
                                        origin = lubridate::origin)}
df[,2:4] <- lapply(df[,2:4],make_dt)

head(df)



#Xu ly text
a <- "Block 2, Lots 2,3,10 & 11, Honest St cor. Determined Street, Calamba Premiere International Park (CPIP), Batino, Calamba, Laguna, Philippines Calamba City Batino Laguna Luzon"
b <- "unit 2 seaviews castles, Tambo, Paranaque City, Metro Manila, Metro Manila"
c <- "999maII 201,26 Villaruel Barretto gen.t number: 70-B 7A. MALL kanto- 1040 Metro Manila"


a <- tolower(word(a,-1))
b <- tolower(word(b,-1))
c <- tolower(word(c,-1))
d <- tolower(word(d,-1))
e <- tolower(word(e,-1))

sub('^.*([[:alnum:]]+)$','\\1',a)
a=="manila"&b=="manila"
a="manila"|a="luzon"
mutate(
  result= case_when( a=="manila"&b=="manila"& time_dif<=3
                     (a=="manila"|a=="luzon")&(b=="manila"|b=="luzon")& time_dif<=5
                     (a=="visayas"|a=="mindanao"|b=="visayas"|b=="mindanao")& time_dif <=7
  )


time_dif <-6
a <- "manila"
a <- "luzon"
a <- "visayas"
a <- "mindanao"
b <- "manila"
b <- "luzon"
b <- "visayas"
b <- "mindanao"

if((a=="manila"& 
    b=="manila"&
    time_dif<=3)|((a=="manila"|a=="luzon")&
                  (b=="manila"|b=="luzon")&
                  time_dif<=5)|((a=="visayas"|a=="mindanao"|b=="visayas"|b=="mindanao")&
                                time_dif<=7)){
  print(0)
} else{
  print(1)
}
 
isLate <- function(a,b,time_dif){
  if((a=="manila"& 
      b=="manila"&
      time_dif<=3)|((a=="manila"|a=="luzon")&
                    (b=="manila"|b=="luzon")&
                    time_dif<=5)|((a=="visayas"|a=="mindanao"|b=="visayas"|b=="mindanao")&
                                  time_dif<=7)){
    print(0)
  } else{
    print(1)
  }
}  
isLate("manila","visayas",8)
  
  
  