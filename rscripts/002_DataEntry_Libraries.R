
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
  mutate(Species = case_when(is.na(Cnt)| Cnt == 0~"NO_POMACEA",
                             Cnt>0 ~ "POMPAL"),
         Length_mm = NA_integer_,
         Period = case_when(Month == 2|Month == 3~1,
                             Month == 4|Month == 5~2,
                             Month == 7 ~ 3,
                             Month == 9|Month==10|Month ==11 ~ 4,
                             Month == 12~5),
         Region = "SRS") |> 
  select(-pompal,-Cnt,Month,-Day) |> 
  mutate(Site = as.character(Site))



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




MDW.raw <- read_excel(here("data","AppleSnail_Length_1996-2024.xlsx"),
                      sheet = 1,na = c("",".")) |> 
  filter(Site != "13") |> 
  mutate(rm = if_else(Comments == "EMPTY" | Comments == "EMPTY, THRGUESS",
                      true = "a", false ="b")) |> 
  filter(is.na(rm) | rm == "b")

WCA <- expand_grid(Year = min(MDW.raw$Year[MDW.raw$Region== "WCA"]):2024,  
                 Region = unique(MDW.raw$Region[MDW.raw$Region== "WCA"]),
                 Period = unique(MDW.raw$Period[MDW.raw$Region== "WCA"]),
                 Site = unique(MDW.raw$Site[MDW.raw$Region== "WCA"]),
                 Plot = unique(MDW.raw$Plot[MDW.raw$Region== "WCA"]),
                 Throw = 1:5) |> 
  mutate(Site = as.character(Site))

TSL <- expand_grid(Year = min(MDW.raw$Year[MDW.raw$Region== "TSL"]):2024,
                   Region = unique(MDW.raw$Region[MDW.raw$Region== "TSL"]),
                   Period = unique(MDW.raw$Period[MDW.raw$Region== "TSL"]),
                   Site = unique(MDW.raw$Site[MDW.raw$Region== "TSL"]),
                   Plot = unique(MDW.raw$Plot[MDW.raw$Region== "TSL"]),
                   Throw = 1:7)|> 
  mutate(Site = as.character(Site))

SRS <- expand_grid(Year = min(MDW.raw$Year[MDW.raw$Region== "SRS"]):2024,  
                   Region = unique(MDW.raw$Region[MDW.raw$Region== "SRS"]),
                   Period = unique(MDW.raw$Period[MDW.raw$Region== "SRS"]),
                   Site = unique(MDW.raw$Site[MDW.raw$Region== "SRS"]),
                   Plot = c("A","B","C"),
                   Throw = 1:7) |> 
  mutate(Site = as.character(Site))

SRS.extraplot.1 <- expand_grid(Year = c(2005,2006,2007),
                 Period = unique(MDW.raw$Period),
                 Region = "SRS",
                 Site = c(6,23),
                 Plot = c("D","E"),
                 Throw = 1:7)|> 
  mutate(Site = as.character(Site))

SRS.extraplot.2 <- expand_grid(Year = 2006,
                 Period = 1:5,
                 Region = "SRS",
                 Plot = "E",
                 Site = 50,
                 Throw = 1:7)|> 
  mutate(Site = as.character(Site))

data.complete <- WCA |> 
  bind_rows(SRS,TSL,SRS.extraplot.1,SRS.extraplot.2) |> 
  filter(!(Year == 1996 & Site == "23" & Region == "SRS")) |> 
  filter(!(Year == 1996 & Site == "6" & Region == "SRS")) |> 
  filter(!(Year == 1996 & Site == "50" & Region == "SRS")) |> 
  filter(!(Year == 1997 & Site == "23" & Region == "SRS")) |> 
  filter(!(Year == 1997 & Site == "6" & Region == "SRS")) |> 
  filter(!(Year == 1997 & Site == "50" & Region == "SRS")) |> 
  full_join(MDW.raw, by = c("Region","Year","Period","Site","Plot","Throw")) |> 
  select(-Page,-`Sorted By`,-`Entered By`,-`Checked By`,-Day) |>
  bind_rows(pompal_85to97_ind) |> 
  mutate(Length_mm = if_else(is.na(Length_mm)&is.na(Comments),
                             true = NA_integer_, false = Length_mm),
         Species = if_else(is.na(Species)&is.na(Comments),
                           true = "NO_POMACEA",false = Species),
         Month = case_when(Period == 1 ~2,
                           Period == 2 ~4,
                           Period == 3 ~7,
                           Period == 4 ~10,
                           Period == 5 ~12),
         Day = 1)
  
data.summ <- data.complete |> 
  group_by(Year,Month,Day,Region,Period,Site,Plot,Species,Throw) |> 
  summarise(count= n()) |> 
  group_by(Year,Month,Day,Region,Period,Site,Plot,Species) |> 
  summarise(n = n(),
            count = sum(count)) |> 
  mutate(n = if_else(is.na(Species),
                     true = NA_integer_,
                     false = n),
         n_throw = sum(n,na.rm = TRUE)) |> 
  pivot_wider(names_from = Species, values_from = count)|>
  ungroup() |> 
  #group_by(Year,Month,Day,Region,Period,Site, Plot) |>
  group_by(Year,Month,Day,Region,Period,Site) |> 
  #group_by(Year,Month,Day,Region,Period) |> 
  #group_by(Year,Region) |> 
  summarise(n_throw = sum(n, na.rm = T) ,
            NO_POMACEA = sum(NO_POMACEA,na.rm = T),
             POMPAL = sum(POMPAL,na.rm = T),
            POMMAC = sum(POMMAC,na.rm = T),
            `NA` = sum(`NA`,na.rm = T)) |> 
  mutate(POMPAL = if_else(is.na(POMPAL) & is.na(`NA`),
                                true = 0,
                                false = POMPAL),
         POMMAC = if_else(is.na(POMMAC) & is.na(`NA`),
                           true = 0,
                           false = POMMAC),
         POMPAL.density = POMPAL/n_throw,
         POMMAC.density = POMMAC/n_throw) |> 
  unite(Month,Day,Year,col = "Date", sep = "/") |> 
  mutate(Date = mdy(Date),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date))  



mwd.hydrology <- read_csv(here("data", "EDEN_MDW_1992_2024_P2.csv")) |> 
  select(DATE, YEAR, MONTH, DAY, REGION, SITE, PLOT, DEPTH) |> 
  filter(REGION != "PHD") |> 
  group_by(REGION,SITE,YEAR, MONTH, DAY, DATE) |> 
  summarise(DEPTH = mean(DEPTH))


rm(pompal_85to97_ind, pompal_85to97_mul, pompal_85to98_raw,SRS,
   SRS.extraplot.1,SRS.extraplot.2,TSL,WCA, addrows_fromcolumn)

