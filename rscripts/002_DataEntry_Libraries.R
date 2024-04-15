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
  rename(Cnt = Number...16)


pompal_85to98_summ <- pompal_85to98_raw |> 
  select(pompal,Year, Month, Day, Site, Plot, Throw, Cnt,WWt.,DWt.) |>  
  group_by(pompal,Year,Month,Site) |> 
  summarise(n_throw = n(),
            Day = round(mean(Day, na.rm = T)),
            Cnt = sum(Cnt,na.rm = T),
            Wwt = sum(WWt., na.rm = T),
            Dwt = sum(DWt., na.rm = T)) |> 
  unite(Year,Month,Day, sep = "/", col = "Date") |> 
  mutate(Date = ymd(Date),
         density = Cnt/n_throw,
         Site = as.character(Site)) |> 
  filter(Site %in% c("6", "23","50")) |>
  mutate(Site = as.numeric(Site))




SRS.raw <- read_excel(here("data","SRS_POMPAL_forbayesproject.xlsx"),
                      sheet = 1,na = c("",".")) |> 
  filter(Site != "CP") |> 
  mutate(Site = as.numeric(Site),
         Throw = if_else(Throw == "1,2,3,4,5,6,7"|Throw == "1,2,7",
                         true = 1, false = as.numeric(Throw)))

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
  mutate(Region = "SRS") |> 
  full_join(SRS.raw, by = c("Region","Year","Period","Site","Plot","Throw")) |> 
  select(-Page,-`Sorted By`,-`Entered By`,-`Checked By`,-Day) |>
  mutate(Length_mm = if_else(is.na(Length_mm)&is.na(Comments),
                             true = 0, false = Length_mm),
         Species = if_else(is.na(Species)&is.na(Comments),
                           true = "NO_POMPAL",false = Species),
         Month = case_when(Period == 1 ~2,
                           Period == 2 ~4,
                           Period == 3 ~7,
                           Period == 4 ~10,
                           Period == 5 ~12),
         Day = 1)
  

SRS.complete|> ggplot(aes(x = Length_mm))+
  theme_bw()+
  geom_histogram()+
  facet_grid(Species~Period, scales = "free_y")

SRS.complete|> ggplot(aes(x = Length_mm))+
  theme_bw()+
  geom_histogram()+
  facet_grid(Year~Species, scales = "free_y")

SRS.complete|> ggplot(aes(x = Length_mm))+
  theme_bw()+
  geom_histogram()+
  facet_grid(Species~Site, scales = "free_y")


SRS.summ <- SRS.complete |> 
  group_by(Year,Month,Day,Period,Site,Plot,Species,Throw) |> 
  summarise(count= n()) |> 
  group_by(Year,Month,Day,Period,Site,Species) |> 
  summarise(n = n(),
            count = sum(count)) |> 
  mutate(n = if_else(is.na(Species),
                     true = NA_integer_,
                     false = n),
         n_throw = sum(n,na.rm = TRUE)) |> 
  pivot_wider(names_from = Species, values_from = count) |> 
  mutate(POMPAL = if_else(is.na(POMPAL) & is.na(`NA`),
                                true = 0,
                                false = POMPAL),
         density = POMPAL/n.throw) |> 
  unite(Month,Day,Year,col = "Date", sep = "/") |> 
  mutate(Date = mdy(Date)) |> 
  select(-NO_POMPAL,-`NA`,-n) 


