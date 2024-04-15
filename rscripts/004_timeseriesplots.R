
SRS.summ |> 
  bind_rows(pompal_85to98_summ) |> 
  filter(Site != 50) |> 
  mutate(Date = as.POSIXct(Date)) |> 
  ggplot(aes(x = Date,y = density, color = as.character(Site)))+
  geom_line()+
  geom_point()+
  facet_wrap(~as.character(Site),scales = "free")+
  theme_bw()+
  geom_hline(yintercept = 0.2, color = "red")


