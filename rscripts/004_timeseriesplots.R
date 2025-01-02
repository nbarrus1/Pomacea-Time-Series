


#data.summ |> 
  #filter(Site != 50) |> 
  #filter(Region == "SRS") |> 
  #mutate(Date = as.POSIXct(Date)) |> 
 # ggplot(aes(x = Year,y = POMPAL.density))+
#  geom_line()+
#  geom_point()+
 # facet_wrap(~Region)+
 # theme_bw()+
#  geom_hline(yintercept = 0.4, color = "red")




####separated out by site and region



data.summ |> 
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

data.summ |> 
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

data.summ |> 
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


###By region

 data.summ |> 
  filter(Region != "PHD") |> 
  #filter(Site != 50) |> 
  #filter(Region == "TSL") |> 
  mutate(Date = as.POSIXct(Date)) |> 
  ggplot(aes(x = Date,y = POMPAL.density, group = as.character(Site)))+
  geom_line()+
  geom_point()+
  facet_grid(~as.character(Region))+
  theme_bw()+
  geom_hline(yintercept = 0.4, color = "red")


 
 ####POMMAC
 
 
 data.summ |> 
   #filter(Site != 50) |> 
   filter(Region == "SRS") |> 
   mutate(Date = as.POSIXct(Date)) |> 
   ggplot(aes(x = Date,y = POMMAC.density, group = as.character(Site)))+
   geom_line()+
   geom_point()+
   facet_wrap(~as.character(Site),scales = "free")+
   theme_bw()+
   geom_hline(yintercept = 0.4, color = "red")+
   labs(title = "Shark River Slough (SRS)")
 
 data.summ |> 
   #filter(Site != 50) |> 
   filter(Region == "WCA") |> 
   mutate(Date = as.POSIXct(Date)) |> 
   ggplot(aes(x = Date,y = POMMAC.density, group = as.character(Site)))+
   geom_line()+
   geom_point()+
   facet_wrap(~as.character(Site),scales = "free")+
   theme_bw()+
   geom_hline(yintercept = 0.4, color = "red")+
   labs(title = "Water Conservation Area 3 (WCA)")
