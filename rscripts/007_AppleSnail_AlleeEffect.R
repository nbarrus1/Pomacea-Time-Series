######for allee effect analysis####

data.apple.summ <- data.apple |> 
  group_by(Year,Month,Day,Region,Period,Site,Plot,Species,Throw) |> 
  summarise(count= n()) |> 
  group_by(Year,Month,Day,Region,Period,Site,Plot,Species) |> 
  summarise(n = n(),
            count = sum(count)) |>
  unite(Month,Day,Year,col = "Date", sep = "/") |>
  mutate(n = if_else(is.na(Species),
                     true = NA_integer_,
                     false = n),
         n_throw = sum(n,na.rm = TRUE),
         Date = mdy(Date),
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
  mutate(TARGET.DATE.LAG = mdy(TARGET.DATE.LAG),
         target.year = year(TARGET.DATE)) |> 
  pivot_wider(names_from = Species, values_from = count)|>
  ungroup() |> 
  #group_by(Year,Month,Day,Region,Period,Site, Plot) |>
  #group_by(Year,Month,Day,Region,Period,Site) |> 
  #group_by(Year,Month,Day,Region,Period) |> 
  group_by(target.year,Region,Site) |> 
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
         POMMAC.density = POMMAC/n_throw) 



data.apple.summ |> 
  #filter(Site != 50) |> 
  filter(Region == "SRS") |> 
  ggplot(aes(x = target.year,y = POMPAL.density, group = as.character(Site)))+
  geom_line()+
  geom_point()+
  facet_wrap(~as.character(Site),scales = "free")+
  theme_bw()+
  geom_hline(yintercept = 0.4, color = "red")+
  labs(title = "Shark River Slough (SRS)")

data.apple.summ |> 
  #filter(Site != 50) |> 
  filter(Region == "WCA") |> 
  ggplot(aes(x = target.year,y = POMPAL.density, group = as.character(Site)))+
  geom_line()+
  geom_point()+
  facet_wrap(~as.character(Site),scales = "free")+
  theme_bw()+
  geom_hline(yintercept = 0.4, color = "red")+
  labs(title = "Shark River Slough (SRS)")

data.apple.summ |> 
  #filter(Site != 50) |> 
  filter(Region == "TSL") |> 
  ggplot(aes(x = target.year,y = POMPAL.density, group = as.character(Site)))+
  geom_line()+
  geom_point()+
  facet_wrap(~as.character(Site),scales = "free")+
  theme_bw()+
  geom_hline(yintercept = 0.4, color = "red")+
  labs(title = "Shark River Slough (SRS)")


allee.timeseries.calcs <- function(df) {
  
  df |> 
    mutate(POMPAL.density.lag = lag(POMPAL.density),
           lambda = POMPAL.density/POMPAL.density.lag)
  
}


data.apple.summ.ls <- data.apple.summ|> 
  ungroup() |> 
  group_by(Region,Site) |> 
  nest() |> 
  mutate(data.calc = map(.x = data, .f = allee.timeseries.calcs)) |> 
  select(-data) |> 
  unnest(data.calc)



####Allee effect plot

p2 <- data.apple.summ.ls |> 
  filter(lambda < 20 & lambda > 0) |> 
  ggplot(aes(x = POMPAL.density, y = log(lambda)))+
  theme_bw()+
  geom_hline(yintercept = 0, color = "#d0938a", linetype = "dashed",linewidth = 1)+
  geom_point(size = 6, shape = 21, color = "black", fill = "#8ba1b6")+
  geom_smooth(color = "#3f3546", linewidth = 2)+
  labs(y = expression(paste("Population Growth (", r[t]," obs)")),
       x = expression(paste("Density (n ","\u00B7"," ",m^-2,")" )))+
  scale_y_continuous(breaks = seq(round(min(log(data.apple.summ.ls$lambda[data.apple.summ.ls$lambda>0]), na.rm = T)),
                                  round(max(log(data.apple.summ.ls$lambda[data.apple.summ.ls$lambda<20]), na.rm = T)),
                                  by = 1))+
  scale_x_continuous(breaks = seq(round(min(data.apple.summ.ls$POMPAL.density, na.rm = T)),
                                  round(max(data.apple.summ.ls$POMPAL.density, na.rm = T)),
                                  by = 0.2),
                     expand = expansion(add = 0.01))+
  theme(axis.title = element_text(face = "bold", size = "40"),
        axis.text = element_text(size = "30"))


ggsave(filename = "C:/Users/nbarr/OneDrive/Pictures/Documents/Career/Presentations/FIU_Biosynposium_2025/Demographic.pdf",
       plot = p2,
       device = "pdf",
       units = "in",
       width = 16,
       height = 16)  
