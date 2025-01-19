
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

data.apple <- WCA |> 
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
         Day = 1,
         Site = case_when(Site == "1" ~ "01",
                     Site == "2" ~ "02",
                     Site == "3" ~ "03",
                     Site == "4" ~ "04",
                     Site == "5" ~ "05",
                     Site == "6" ~ "06",
                     Site == "7" ~ "07",
                     Site == "8" ~ "08",
                     Site == "9" ~ "09",
                     .default = Site
           )) |> 
  filter(!(Year == 1992 & Site == "07" & Region == "SRS")) |> 
  filter(!(Year == 1992 & Site == "08" & Region == "SRS")) |>
  filter(!(Year == 1992 & Site == "37" & Region == "SRS")) 
  


data.apple.summ <- data.apple |> 
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
         Day = day(Date),
         target.year = case_when(Date %within% interval(mdy("7/1/1985"),mdy("6/30/1986")) ~ 1985,
                                 Date %within% interval(mdy("7/1/1986"),mdy("6/30/1987")) ~ 1986,
                                 Date %within% interval(mdy("7/1/1987"),mdy("6/30/1988")) ~ 1987,
                                 Date %within% interval(mdy("7/1/1988"),mdy("6/30/1989")) ~ 1988,
                                 Date %within% interval(mdy("7/1/1989"),mdy("6/30/1990")) ~ 1989,
                                 Date %within% interval(mdy("7/1/1990"),mdy("6/30/1991")) ~ 1990,
                                 Date %within% interval(mdy("7/1/1991"),mdy("6/30/1992")) ~ 1991,
                                 Date %within% interval(mdy("7/1/1992"),mdy("6/30/1993")) ~ 1992,
                                 Date %within% interval(mdy("7/1/1993"),mdy("6/30/1994")) ~ 1993,
                                 Date %within% interval(mdy("7/1/1994"),mdy("6/30/1995")) ~ 1994,
                                 Date %within% interval(mdy("7/1/1995"),mdy("6/30/1996")) ~ 1995,
                                 Date %within% interval(mdy("7/1/1996"),mdy("6/30/1997")) ~ 1996,
                                 Date %within% interval(mdy("7/1/1997"),mdy("6/30/1998")) ~ 1997,
                                 Date %within% interval(mdy("7/1/1998"),mdy("6/30/1999")) ~ 1998,
                                 Date %within% interval(mdy("7/1/1999"),mdy("6/30/2000")) ~ 1999,
                                 Date %within% interval(mdy("7/1/2000"),mdy("6/30/2001")) ~ 2000,
                                 Date %within% interval(mdy("7/1/2001"),mdy("6/30/2002")) ~ 2001,
                                 Date %within% interval(mdy("7/1/2002"),mdy("6/30/2003")) ~ 2002,
                                 Date %within% interval(mdy("7/1/2003"),mdy("6/30/2004")) ~ 2003,
                                 Date %within% interval(mdy("7/1/2004"),mdy("6/30/2005")) ~ 2004,
                                 Date %within% interval(mdy("7/1/2005"),mdy("6/30/2006")) ~ 2005,
                                 Date %within% interval(mdy("7/1/2006"),mdy("6/30/2007")) ~ 2006,
                                 Date %within% interval(mdy("7/1/2007"),mdy("6/30/2008")) ~ 2007,
                                 Date %within% interval(mdy("7/1/2008"),mdy("6/30/2009")) ~ 2008,
                                 Date %within% interval(mdy("7/1/2009"),mdy("6/30/2010")) ~ 2009,
                                 Date %within% interval(mdy("7/1/2010"),mdy("6/30/2011")) ~ 2010,
                                 Date %within% interval(mdy("7/1/2011"),mdy("6/30/2012")) ~ 2011,
                                 Date %within% interval(mdy("7/1/2012"),mdy("6/30/2013")) ~ 2012,
                                 Date %within% interval(mdy("7/1/2013"),mdy("6/30/2014")) ~ 2013,
                                 Date %within% interval(mdy("7/1/2014"),mdy("6/30/2015")) ~ 2014,
                                 Date %within% interval(mdy("7/1/2015"),mdy("6/30/2016")) ~ 2015,
                                 Date %within% interval(mdy("7/1/2016"),mdy("6/30/2017")) ~ 2016,
                                 Date %within% interval(mdy("7/1/2017"),mdy("6/30/2018")) ~ 2017,
                                 Date %within% interval(mdy("7/1/2018"),mdy("6/30/2019")) ~ 2018,
                                 Date %within% interval(mdy("7/1/2019"),mdy("6/30/2020")) ~ 2019,
                                 Date %within% interval(mdy("7/1/2020"),mdy("6/30/2021")) ~ 2020,
                                 Date %within% interval(mdy("7/1/2021"),mdy("6/30/2022")) ~ 2021,
                                 Date %within% interval(mdy("7/1/2022"),mdy("6/30/2023")) ~ 2022,
                                 Date %within% interval(mdy("7/1/2023"),mdy("6/30/2024")) ~ 2023,
                                                            .default = 2024),
         target.month = month(Date),
         target.day = day(Date)) |> 
  unite(target.month,target.day,target.year, col = "TARGET.DATE", sep = "/") |> 
  mutate(TARGET.DATE= mdy(TARGET.DATE),
         target.month.lag = month(TARGET.DATE),
         target.day.lag = day(TARGET.DATE),
         target.year.lag = year(TARGET.DATE)-1) |> 
  unite(target.month.lag,target.day.lag,target.year.lag, col = "TARGET.DATE.LAG", sep = "/") |> 
  mutate(TARGET.DATE.LAG = mdy(TARGET.DATE.LAG))





mwd.hydrology <- read_csv(here("data", "EDEN_MDW_1992_2024_P2.csv")) |>   #read in data
  #select(DATE, YEAR, MONTH, DAY, REGION, SITE, PLOT)|> 
  filter(REGION != "PHD") |>   #remove PHD data
  unite(MONTH, DAY, TARGETYEAR, col = TARGET.DATE, sep = "/") |> 
  group_by(REGION,SITE,TARGET.DATE,DATE,SEASON) |>                        #group by everything but plot to get site summaries
  summarise_if(is.numeric, mean, na.rm = TRUE) |>                         #get means of every site, year, period combo for every column that is numeric
  ungroup() |> 
  mutate(DATE = mdy(DATE),
         TARGET.DATE = mdy(TARGET.DATE),
         DAY = day(DATE),
         MONTH = month(DATE),
         target.depths = if_else(DEPTH >=20 & DEPTH <= 50, true = 1,false = 0),  #Identify days with target depths
         target.dates = if_else(MONTH >=3 & MONTH <= 6, true = 1, false = 0),    #identify days with target dates
         target.days = if_else(target.depths == 1 & target.dates == 1,           #Identify days with target depths are within target dates
                               true = 1, false = 0),
         dry = if_else(DEPTH > 0, true = 0, false = 1),
         TARGET.YEAR = year(TARGET.DATE)) |>                                     #identify days that are dry
  group_by(REGION,SITE,TARGET.YEAR) |>                                           #group by sites and years to get year summaries 
  mutate(tot.target.days = sum(target.days),                                     #find the total number of days within target depths during the correct dates for each site and year
         dry = sum(dry),                                                         #find the number of days dry each year
         dry_down = if_else(dry > 0, true = "yes", false = "no"),
         SITE = case_when(SITE == "1" ~ "01",
                          SITE == "2" ~ "02",
                          SITE == "3" ~ "03",
                          SITE == "4" ~ "04",
                          SITE == "5" ~ "05",
                          SITE == "6" ~ "06",
                          SITE == "7" ~ "07",
                          SITE == "8" ~ "08",
                          SITE == "9" ~ "09",
                          .default = SITE)) |> 
  select(-target.depths,-target.dates,-target.days) |>      #remove the intermediate variables
  rename(Region = REGION,
         Site = SITE,
         Year = YEAR,
         Month = MONTH,
         Day = DAY,
         Date = DATE)|> 
  select(-Date,-Year,-Month,-Day) |> 
  ungroup()

rm(pompal_85to97_ind, pompal_85to97_mul, pompal_85to98_raw,SRS,
   SRS.extraplot.1,SRS.extraplot.2,TSL,WCA, addrows_fromcolumn)

