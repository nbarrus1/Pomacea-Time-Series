


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
 
 
 
 ####biosymposium figure
 
 
 groups <- data.apple.summ |> 
   #filter(Site != 50) |> 
   group_by(Region,Site) |>
   group_keys() |> 
   ungroup() |> 
   mutate(group = as.character(1:n()))
 
 
 ever.trend <- data.apple.summ |>  
   filter(Year > 1995) |> 
   filter(Region != "PHD") |> 
   filter(!(Region == "WCA" & Site == "05" & Year == "2012" & Month == "10")) |> 
   ungroup() |> 
   select(Year,POMPAL.density) |> 
   drop_na(POMPAL.density)|> 
   group_by(Year) |> 
   summarise(POMPAL.density.ave = mean(POMPAL.density),
             pompal.sd = sd(POMPAL.density),
             n.sample = n()) |> 
   mutate(Month = 6,
          Day = 1) |> 
   unite(Year, Month, Day, col = "Date", sep = "/") |> 
   mutate(Date = as.POSIXct(ymd(Date)),
          upp = POMPAL.density.ave + (1.96*pompal.sd/sqrt(n.sample)),
          low = POMPAL.density.ave - (1.96*pompal.sd/sqrt(n.sample)),
          low = if_else(low < 0, true = 0, false = low))
 
 
 p1 <- data.apple.summ |> 
   filter(!(Region == "WCA" & Site == "05" & Year == "2012" & Month == "10")) |> 
   filter(Region != "PHD") |> 
   filter(Year>1995) |> 
   ungroup() |> 
   select(Region,Site,Year,POMPAL.density) |> 
   drop_na(POMPAL.density)|> 
   group_by(Region,Site,Year) |> 
   summarise(POMPAL.density = mean(POMPAL.density)) |> 
   mutate(Month = 6,
          Day = 1) |> 
   unite(Year, Month, Day, col = "Date", sep = "/") |> 
   mutate(Date = as.POSIXct(ymd(Date))) |>
   left_join(groups, by = c("Region", "Site")) |> 
   ggplot(aes(x = as_date(Date),y = POMPAL.density))+
   geom_line(aes(group = group), color = "#8ba1b6", linewidth = 2)+
   geom_point(aes(group = group), color = "black", fill = "#8ba1b6", pch = 21, size = 6)+
   geom_line(data = ever.trend,
             aes(x = as_date(Date), y = POMPAL.density.ave),
             color = "#3f3546", linewidth = 2)+
   geom_ribbon(data = ever.trend, 
               aes(x = as_date(Date), ymin = low, ymax = upp, y = POMPAL.density.ave),
               fill = "#3f3546", alpha = 0.7, show.legend = T)+
   geom_point(data = ever.trend,
              aes(x = as_date(Date), y = POMPAL.density.ave),
              color = "black",
              fill = "#3f3546",
              pch = 21, size = 6)+
   scale_y_continuous(name = expression(paste("Density (n ","\u00B7"," ",m^-2,")")),
                      expand = expansion(add = 0.01))+
   #scale_y_continuous(name = expression(paste("Density (n ","\u00B7"," ",m^-2,")")),
   #                    sec.axis = sec_axis(trans = ~./20,
   #                                        name = expression(paste("Average Density (n ","\u00B7"," ",m^-2,")")))
   # #                   )+
   scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                name = "Year",
                expand = expansion(add = 100))+
   theme_bw()+
   theme(axis.title = element_text(face = "bold", size = 40),
         axis.text = element_text(size = 30),
         axis.text.x = element_text(angle = 90))
 
 ggsave(filename = "C:/Users/nbarr/OneDrive/Pictures/Documents/Career/Presentations/FIU_Biosynposium_2025/TimeSeries.pdf",
        plot = p1,
        device = "pdf",
        units = "in",
        width = 16,
        height = 8)   
 

 
 
 
 
 
 
 
 
 ever.trend <- data.apple.summ |>  
   filter(Year > 1995) |> 
   filter(Region != "PHD") |> 
   filter(!(Region == "WCA" & Site == "05" & Year == "2012" & Month == "10")) |> 
   ungroup() |> 
   select(Year,Region,POMPAL.density) |> 
   drop_na(POMPAL.density)|> 
   group_by(Region,Year) |> 
   summarise(POMPAL.density.ave = mean(POMPAL.density),
             pompal.sd = sd(POMPAL.density),
             n.sample = n()) |> 
   mutate(Month = 6,
          Day = 1) |> 
   unite(Year, Month, Day, col = "Date", sep = "/") |> 
   mutate(Date = as.POSIXct(ymd(Date)),
          upp = POMPAL.density.ave + (1.96*pompal.sd/sqrt(n.sample)),
          low = POMPAL.density.ave - (1.96*pompal.sd/sqrt(n.sample)),
          low = if_else(low < 0, true = 0, false = low))
 
 
 
 
p3 <- data.apple.summ |> 
   filter(!(Region == "WCA" & Site == "05" & Year == "2012" & Month == "10")) |> 
   filter(Region != "PHD") |> 
   filter(Year>1995) |> 
   ungroup() |> 
   select(Region,Site,Year,POMPAL.density) |> 
   drop_na(POMPAL.density)|> 
   group_by(Region,Site,Year) |> 
   summarise(POMPAL.density = mean(POMPAL.density)) |> 
   mutate(Month = 6,
          Day = 1) |> 
   unite(Year, Month, Day, col = "Date", sep = "/") |> 
   mutate(Date = as.POSIXct(ymd(Date))) |>
   left_join(groups, by = c("Region", "Site")) |> 
   ggplot(aes(x = as_date(Date),y = POMPAL.density))+
   geom_line(aes(group = group), color = "#8ba1b6", linewidth = 2)+
   geom_point(aes(group = group), color = "black", fill = "#8ba1b6", pch = 21, size = 6)+
   geom_line(data = ever.trend,
             aes(x = as_date(Date), y = POMPAL.density.ave),
             color = "#3f3546", linewidth = 2)+
   geom_ribbon(data = ever.trend, 
               aes(x = as_date(Date), ymin = low, ymax = upp, y = POMPAL.density.ave),
               fill = "#3f3546", alpha = 0.7, show.legend = T)+
   geom_point(data = ever.trend,
              aes(x = as_date(Date), y = POMPAL.density.ave),
              color = "black",
              fill = "#3f3546",
              pch = 21, size = 6)+
   scale_y_continuous(name = expression(paste("Density (n ","\u00B7"," ",m^-2,")")),
                      expand = expansion(add = 0.01))+
   #scale_y_continuous(name = expression(paste("Density (n ","\u00B7"," ",m^-2,")")),
   #                    sec.axis = sec_axis(trans = ~./20,
   #                                        name = expression(paste("Average Density (n ","\u00B7"," ",m^-2,")")))
   # #                   )+
   scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                name = "Year",
                expand = expansion(add = 100))+
   theme_bw()+
   theme(axis.title = element_text(face = "bold", size = 40),
         axis.text = element_text(size = 30),
         axis.text.x = element_text(angle = 90),
         strip.text = element_text(size = 40))+
   facet_wrap(~Region, dir = "v")
 
 
 ggsave(filename = "C:/Users/nbarr/OneDrive/Pictures/Documents/Career/Presentations/FIU_Biosynposium_2025/TimeSeries_Region.pdf",
        plot = p3,
        device = "pdf",
        units = "in",
        width = 16,
        height = 24)   
 
 