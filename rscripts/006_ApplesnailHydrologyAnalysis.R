###this script is for analyzing the APPLE SNAILS DENSITY DATA using the
###hydrologic variables


#Join apple snail data to hydrology data

data.apple.hydro <- data.apple.summ |>
  ungroup() |> 
  left_join(mwd.hydrology |> rename(TARGET.DATE.LAG = TARGET.DATE),
            by = c("Region","Site","TARGET.DATE.LAG"))



data.apple.hydro |> 
  ggplot(aes(y = POMPAL.density, x = tot.target.days))+
  theme_bw()+
  geom_smooth(method = "loess")#+
  geom_point()


#---------------------------------
#### Target Time Series
#---------------------------------
  
  data.apple.hydro |> 
    #filter(Site != 50) |> 
    filter(Region == "SRS") |> 
    mutate(Date = as.POSIXct(TARGET.DATE.LAG)) |> 
    ggplot(aes(x = Date,y = tot.target.days, group = as.character(Site)))+
    geom_line()+
    geom_point()+
    geom_smooth()+
    facet_wrap(~as.character(Site))+
    theme_bw()+
    geom_hline(yintercept = 75, color = "red")+
    labs(title = "Shark River Slough (SRS)")
  
  data.apple.hydro |> 
    #filter(Site != 50) |> 
    filter(Region == "WCA") |> 
    mutate(Date = as.POSIXct(TARGET.DATE.LAG)) |> 
    ggplot(aes(x = Date,y = tot.target.days, group = as.character(Site)))+
    geom_line()+
    geom_point()+
    geom_smooth()+
    facet_wrap(~as.character(Site))+
    theme_bw()+
    geom_hline(yintercept = 75, color = "red")+
    labs(title = "Water Conservation Area 3 (WCA)")  
  
data.apple.hydro |> 
    #filter(Site != 50) |> 
    filter(Region == "TSL") |> 
    mutate(Date = as.POSIXct(TARGET.DATE.LAG)) |> 
    ggplot(aes(x = Date,y = tot.target.days, group = as.character(Site)))+
    geom_line()+
    geom_point()+
  geom_smooth()+
  facet_wrap(~as.character(Site))+
    theme_bw()+
  geom_hline(yintercept = 75, color = "red")+
    labs(title = "Taylor Slough (WCA)")

####----------------------------------
#### Snail Time Series
####----------------------------------

data.apple.summ |> 
  #filter(Site != 50) |> 
  filter(Region == "SRS") |> 
  mutate(Date = as.POSIXct(Date)) |> 
  ggplot(aes(x = Date,y = POMPAL.density, group = as.character(Site)))+
  geom_line()+
  geom_point()+
  facet_wrap(~as.character(Site),scales = "free")+
  theme_bw()+
  geom_hline(yintercept = 0.4, color = "red")+
  labs(title = "Shark River Slough (SRS)")

data.apple.summ |> 
  #filter(Site != 50) |> 
  filter(Region == "WCA") |> 
  mutate(Date = as.POSIXct(Date)) |> 
  ggplot(aes(x = Date,y = POMPAL.density, group = as.character(Site)))+
  geom_line()+
  geom_point()+
  facet_wrap(~as.character(Site),scales = "free")+
  theme_bw()+
  geom_hline(yintercept = 0.4, color = "red")+
  labs(title = "Water Conservation Area 3 (WCA)")

data.apple.summ |> 
  #filter(Site != 50) |> 
  filter(Region == "TSL") |> 
  mutate(Date = as.POSIXct(Date)) |> 
  ggplot(aes(x = Date,y = POMPAL.density, group = as.character(Site)))+
  geom_line()+
  geom_point()+
  facet_wrap(~as.character(Site),scales = "free")+
  theme_bw()+
  geom_hline(yintercept = 0.4, color = "red")+
  labs(title = "Taylor Slough (TSL)")
