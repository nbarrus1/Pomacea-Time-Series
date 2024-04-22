
#libraires

library(R2jags)
library(ggmcmc)



#----------------
#matrix format
#------------------

data.matrix.obs <- SRS.complete |> 
   ungroup() |> 
   group_by(Year, Site) |> 
   mutate(ind = row_number(),
          species.factor = if_else(Species=="POMPAL", true = 1,false = 0)) |> 
   ungroup() |> 
   select(Year,Site,ind,species.factor) |> 
   pivot_wider(names_from = ind, values_from = species.factor, values_fill = 0) |> 
   rowwise() |> 
   mutate(snail.tot = sum(c_across(3:180),na.rm = T)) |> 
   left_join(throws, by = c("Year", "Site")) |> 
   filter(n_throw > 0) #|> 
   #filter(Year == 1999|Year == 2004|Year==2009|Year == 2014|Year == 2021)
   


data.matrix.len <- SRS.complete |> 
   group_by(Year, Site) |> 
   mutate(ind = row_number()) |> 
   ungroup() |> 
   select(Year,Site,ind,Length_mm) |> 
   pivot_wider(names_from = ind, values_from = Length_mm, values_fill = NA)|> 
   left_join(throws, by = c("Year", "Site"))|> 
   filter(n_throw > 0)#|> 
   #filter(Year == 1999|Year == 2004|Year==2009|Year == 2014|Year == 2021)

 
##get plot information

#data.plot <- obs |> 
#  bind_rows(augment) |> 
#  mutate(y = if_else(Species == "POMPAL", true = 1, false = 0)) |> 
#  group_by(Year,Period,Site,Plot) |> 
#  summarise(tot = n(),
#            nind = sum(y)) |> 
#  mutate(nz = tot-nind) |> 
#  ungroup() |> 
#  mutate(nplot = row_number())

###set up the data list for JAGS

data.ls <- list(y = as.matrix(data.matrix.obs |> select(3:180)),
                len = as.matrix(data.matrix.len |> select(3:180)),
                nsite = nrow(as.matrix(data.matrix.obs |> select(3:180))),
                ncol = ncol(as.matrix(data.matrix.obs |> select(3:180))),
                site = data.matrix.obs$Site,
                year = data.matrix.obs$Year,
                nthrow = data.matrix.obs$n_throw)

###write jags model


write("model {
#---------
#main set up
#---------

mu.len <-26.5057
sd.len <-9.92816
tau.len <-1/(sd.len*sd.len)

#priors for detection model coefficients

a1 ~ dnorm(0, 1.0E-6)
b1 ~ dnorm(0, 1.0E-6)

#priors for existance model coefficients

a2 ~ dnorm(0, 1.0E-6)
b2[3] ~ dnorm(0, 1.0E-6)
b2[1] ~ dnorm(0, 1.0E-6)
b2[2] ~ dnorm(0, 1.0E-6)

#---------
#main loop
#---------

for(i in 1:nsite) {
  for(j in 1:ncol) {
    len[i,j]~dnorm(mu.len,tau.len)
    logit(P.detect[i,j])<-a1+b1*len[i,j]
    logit(P.exist[i,j])<- a2+b2[1]*year[i]+b2[2]*site[i]+b2[3]*year[i]*site[i]
    z[i,j]~dbern(P.exist[i,j])
    mu[i,j]<-z[i,j]*P.detect[i,j]
    y[i,j]~dbern(mu[i,j])
    LL[i,j]<-log((y[i,j]*mu[i,j])+((1-y[i,j])*(1-mu[i,j])))
  }
 #calculated quantities
 N.plot[i]<-sum(z[i,])  
}
      }", file = here("JAGS_mods", "jags1.txt"))

#run jags model


init1 <- list(z = as.matrix(data.matrix.obs |> select(3:180)))

init2 <- list(z = as.matrix(data.matrix.obs |> select(3:180)))

jags1 <- jags.parallel(data.ls, inits = list(init1), model.file = here("JAGS_mods", "POMPAL_density_correction_simple.txt"),
                                   parameters.to.save = c("a1","b1","a2","b2","N.plot","P.exist","len","P.detect","LL"),
                                   n.chains = 2, n.iter = 1000, n.burnin = 100, n.thin = 2)


range(jags1$BUGSoutput$summary[,"n.eff"])
range(jags1$BUGSoutput$summary[,"Rhat"])


jags1$BUGSoutput$summary[c("a1","b1","a2"),]
jags1$BUGSoutput$summary[c("b2[1]")]


jags.summary <- jags1$BUGSoutput$summary    
parameter <- paste0("N.plot[",1:nrow(data.matrix.obs),"]")

data.matrix.obs$N.plot <- jags.summary[parameter,"mean"]
data.matrix.obs$N.plot.low <- jags.summary[parameter,"2.5%"]
data.matrix.obs$N.plot.upp <- jags.summary[parameter,"97.5%"]


data.matrix.obs |> 
  group_by(Year,Site,n_throw) |> 
  summarise(N.site = mean(N.plot),
            N.site.low = mean(N.plot.low),
            N.site.upp = mean(N.plot.upp)) |>
  mutate(N.site = N.site/n_throw,
         N.site.low = N.site.low/n_throw,
         N.site.upp = N.site.upp/n_throw) |> 
  ggplot(aes(x = Year,y = N.site))+
  geom_ribbon(aes(ymax = N.site.upp, ymin = N.site.low),alpha = 0.6)+
  geom_point()+
  geom_line()+
  facet_wrap(~as.character(Site))+
  theme_bw()+
  geom_hline(yintercept = 1.0, color = "red")+
  geom_hline(yintercept = 0.4, color = "red")



jags_summary <- as.data.frame(jags.summary) |> 
  rownames_to_column(var = "parameter") |> 
  as_tibble() |> 
  filter(str_detect(parameter,pattern = "P.exist")) |> 
  separate_wider_delim(parameter,delim = "[", names = c("parameter","matrix")) |> 
  mutate(matrix = str_remove_all(matrix,pattern="]")) |> 
  separate_wider_delim(matrix,delim = ",", names = c("year.site.id","ind.id")) |> 
  pivot_wider(names_from = year.site.id,values_from = ind.id,) |> 
  select(-(11:166)) |> 
  mutate(Year = data.matrix.obs$Year,
         Site = data.matrix.obs$Site)


jags_summary |>
  ggplot(aes(x = Year, y = mean, color = as.character(Site),
             fill = as.character(Site)))+
  geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`), alpha = 0.6)+
  facet_wrap(~as.character(Site))+
  geom_point()+
  geom_line()+
  theme_bw()


#grand mean

data.matrix.obs |> 
  group_by(Year) |> 
  summarise(N = sum(N.plot),
            N.low = sum(N.plot.low),
            N.upp = sum(N.plot.upp),
            n_throw = sum(n_throw)) |>
  mutate(D = N/n_throw,
         D.low = N.low/n_throw,
         D.upp = N.upp/n_throw) |> 
  ggplot(aes(x = Year,y = D))+
  geom_ribbon(aes(ymax = D.upp, ymin = D.low),alpha = 0.6)+
  geom_point()+
  geom_line()+
  theme_bw()+
  geom_hline(yintercept = 1.0, color = "red")+
  geom_hline(yintercept = 0.4, color = "red")
  