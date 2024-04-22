
rm(list = ls())
#---------------------------------
####Libraries####
#---------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(readxl)

#----------------------------------
#####Load data###
#----------------------------------

clearing_data <- read_excel(here("data","ClearingEfficiency_v1.xlsx")) %>% 
  mutate(Capture = if_else(Capture == "Y", true = 1,
                           false = if_else(Capture == "N", true = 0,
                                         false = NA_real_)))

pompal_85to98_raw <- read_excel(here("data","MacroInvert 85-98 & throw numbers fixed_JKline.xlsx"),
                            skip = 1, sheet = 1, na = c("",".")) |> 
  mutate(pompal = if_else(Order == 1, true = "pompal",false = "other")) |> 
  filter(pompal == "pompal") |> 
  rename(Cnt = Number...16) |> 
  filter(Site != 20)


###create function to take my count data to individual data

addrows_fromcolumn <- function(df,vctr){
  
  ##funciton  as is will only work for df with 8 columns
  
  df.final <- df[1,] 
  temp <- as.matrix(df)
  
  for (i in 1:nrow(df)) {
    
    
    if (vctr[i] == 0) { 
      
      df.final <- df.final
      bind_rows(df[i,])
      
    } else {
      
      
      
      rowstoadd <- tibble(pompal = rep(temp[i,1], times = vctr[i]),
                          Year = as.numeric(rep(temp[i,2], times = vctr[i])),
                          Month = as.numeric(rep(temp[i,3], times = vctr[i])),
                          Day = as.numeric(rep(temp[i,4], times = vctr[i])),
                          Site = as.numeric(rep(temp[i,5], times = vctr[i])),
                          Plot = rep(temp[i,6], times =vctr[i]),
                          Throw = as.numeric(rep(temp[i,7], times = vctr[i])),
                          Cnt = as.numeric(rep(temp[i,8], times = vctr[i])))
      
      df.final <- df.final |> 
        bind_rows(rowstoadd)
      
    }}
  
  
  df.final |> 
    slice(2:n())
  
}


#apply function

pompal_85to97_ind <- pompal_85to98_raw |> 
  select(pompal,Year, Month, Day, Site, Plot, Throw, Cnt) |> 
  drop_na(Cnt) |> 
  filter(Year != 1998)

pompal_85to97_mul <- addrows_fromcolumn(df = pompal_85to97_ind, vctr = pompal_85to97_ind$Cnt) 

#clean the data to be merged with MDW data

pompal_85to97_ind <- pompal_85to98_raw |> 
  select(pompal,Year, Month, Day, Site, Plot, Throw, Cnt) |> 
  filter(Cnt == 0|is.na(Cnt)) |> 
  filter(Year != 1998) |> 
  bind_rows(pompal_85to97_mul) |> 
  mutate(Species = case_when(is.na(Cnt)| Cnt == 0~"NO_POMPAL",
                             Cnt>0 ~ "POMPAL"),
         Length_mm = NA_integer_,
         Period = case_when(Month == 2|Month == 3~1,
                             Month == 4|Month == 5~2,
                             Month == 7 ~ 3,
                             Month == 9|Month==10|Month ==11 ~ 4,
                             Month == 12~5),
         Region = "SRS") |> 
  select(-pompal,-Cnt,Month,-Day)



###old data summary

#pompal_85to98_summ <- pompal_85to98_raw |> 
#  select(pompal,Year, Month, Day, Site, Plot, Throw, Cnt,WWt.,DWt.) |>  
#  group_by(pompal,Year,Month,Site) |> 
#  summarise(n_throw = n(),
#            Day = round(mean(Day, na.rm = T)),
#            Cnt = sum(Cnt,na.rm = T),
#            Wwt = sum(WWt., na.rm = T),
#            Dwt = sum(DWt., na.rm = T)) |> 
#  unite(Year,Month,Day, sep = "/", col = "Date") |> 
#  mutate(Date = ymd(Date),
#         density = Cnt/n_throw,
#         Site = as.character(Site)) |> 
#  filter(Site %in% c("6", "23","50")) |>
#  mutate(Site = as.numeric(Site))




SRS.raw <- read_excel(here("data","SRS_POMPAL_forbayesproject.xlsx"),
                      sheet = 1,na = c("",".")) |> 
  filter(Site != "CP") |> 
  mutate(Site = as.numeric(Site),
         Throw = if_else(Throw == "1,2,3,4,5,6,7"|Throw == "1,2,7",
                         true = 1, false = as.numeric(Throw)),
         filter.temp = if_else(is.na(Comments),true = "a",
                               false = if_else(Comments == "EMPTY", true = "b",
                                               false = "a"))) |> 
  filter(filter.temp == "a") |> 
  select(-filter.temp)

x <- expand_grid(Year = unique(SRS.raw$Year),
                 Period = unique(SRS.raw$Period),
                 Site = as.numeric(unique(SRS.raw$Site)),
                 Plot = c("A","B","C"),
                 Throw = unique(SRS.raw$Throw))

y <- expand_grid(Year = c(2005,2006,2007),
                 Period = unique(SRS.raw$Period),
                 Site = c(6,23),
                 Plot = c("D","E"),
                 Throw = 1:7)
z <- expand_grid(Year = 2006,
                 Period = 1:5,
                 Plot = "E",
                 Site = 50,
                 Throw = 1:7)

SRS.complete <- x |> 
  bind_rows(y,z) |> 
  filter(!(Year == 1996 & Site == 23)) |> 
  filter(!(Year == 1996 & Site == 6)) |> 
  filter(!(Year == 1996 & Site == 50)) |> 
  filter(!(Year == 1997 & Site == 23)) |> 
  filter(!(Year == 1997 & Site == 6)) |> 
  filter(!(Year == 1997 & Site == 50)) |> 
  mutate(Region = "SRS") |> 
  full_join(SRS.raw, by = c("Region","Year","Period","Site","Plot","Throw")) |> 
  select(-Page,-`Sorted By`,-`Entered By`,-`Checked By`,-Day) |>
  bind_rows(pompal_85to97_ind) |> 
  mutate(Length_mm = if_else(is.na(Length_mm)&is.na(Comments),
                             true = NA_integer_, false = Length_mm),
         Species = if_else(is.na(Species)&is.na(Comments),
                           true = "NO_POMPAL",false = Species),
         Month = case_when(Period == 1 ~2,
                           Period == 2 ~4,
                           Period == 3 ~7,
                           Period == 4 ~10,
                           Period == 5 ~12),
         Day = 1)
  
SRS.summ <- SRS.complete |> 
  group_by(Year,Month,Day,Period,Site,Plot,Species,Throw) |> 
  summarise(count= n()) |> 
  group_by(Year,Month,Day,Period,Site,Plot,Species) |> 
  summarise(n = n(),
            count = sum(count)) |> 
  mutate(n = if_else(is.na(Species),
                     true = NA_integer_,
                     false = n),
         n_throw = sum(n,na.rm = TRUE)) |> 
  pivot_wider(names_from = Species, values_from = count)|>
  ungroup() |> 
  group_by(Year,Month,Day, Period,Site, Plot) |>
  #group_by(Year,Month,Day, Period,Site) |> 
  #group_by(Year,Site) |> 
  summarise(n_throw = sum(n, na.rm = T) ,
            NO_POMPAL = sum(NO_POMPAL,na.rm = T),
             POMPAL = sum(POMPAL,na.rm = T),
            `NA` = sum(`NA`,na.rm = T)) |> 
  mutate(POMPAL = if_else(is.na(POMPAL) & is.na(`NA`),
                                true = 0,
                                false = POMPAL),
         density = POMPAL/n_throw) |> 
  unite(Month,Day,Year,col = "Date", sep = "/") |> 
  mutate(Date = mdy(Date),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date))  


throws <- SRS.summ |> 
  select(-POMPAL,-NO_POMPAL,-`NA`,-density)


rm(pompal_85to97_mul,pompal_85to98_raw,pompal_85to97_ind, SRS.raw,x,y,z, addrows_fromcolumn)


