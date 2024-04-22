
library(R2jags)

#----------------------------
####setup data for matrix format detections####
#----------------------------


data.matrix.obs <- SRS.complete |> 
  filter(Year > 1995) |> 
  group_by(Year, Period,Site,Plot) |> 
  mutate(ind = row_number(),
         species.factor = if_else(Species=="POMPAL", true = 1,false = 0)) |> 
  drop_na(species.factor) |> 
  ungroup() |> 
  select(-Length_mm,-Comments,-Throw,-Species) |> 
  pivot_wider(names_from = ind, values_from = species.factor, values_fill = 0) |> 
  add_column(`39`=0,
             `40`=0,
             `41`=0,
             `42`=0,
             `43`=0,
             `44`=0,
             `45`=0,
             `46`=0,
             `47`=0,
             `48`=0,
             `49`=0,
             `50`=0,
             `51`=0,
             `52`=0,
             `53`=0,
             `54`=0,
             `55`=0,
             `56`=0,
             `57`=0,
             `58`=0,
             `59`=0,
             `60`=0) |> 
  left_join(throws |> filter(Year > 1995) |> select(-Date),
            by = c("Year", "Period","Site","Plot","Month","Day"),
            keep = FALSE)  |> 
  drop_na(n_throw) |> 
  filter(Year == 1996 |Year == 2009 | Year == 2021) |> 
  rowwise() |> 
  mutate(ntot = sum(c_across(8:66))) |> 
  ungroup() |> 
  arrange(desc(ntot))


data.matrix.len <- SRS.complete |> 
  filter(Year > 1995) |> 
  group_by(Year, Period,Site,Plot) |> 
  mutate(ind = row_number()) |> 
  ungroup() |> 
  select(-Comments,-Throw,-Species) |> 
  pivot_wider(names_from = ind, values_from = Length_mm, values_fill = NA)|> 
  add_column(`39`=NA_integer_,
             `40`=NA_integer_,
             `41`=NA_integer_,
             `42`=NA_integer_,
             `43`=NA_integer_,
             `44`=NA_integer_,
             `45`=NA_integer_,
             `46`=NA_integer_,
             `47`=NA_integer_,
             `48`=NA_integer_,
             `49`=NA_integer_,
             `50`=NA_integer_,
             `51`=NA_integer_,
             `52`=NA_integer_,
             `53`=NA_integer_,
             `54`=NA_integer_,
             `55`=NA_integer_,
             `56`=NA_integer_,
             `57`=NA_integer_,
             `58`=NA_integer_,
             `59`=NA_integer_,
             `60`=NA_integer_)|> 
  left_join(throws |> filter(Year > 1995) |> 
            select(-Date), by = c("Year", "Period","Site","Plot","Month","Day")) |> 
  left_join(data.matrix.obs |> select(Year,Period,Site,Plot, Month, Day,ntot),
            by = c("Year", "Period","Site","Plot","Month","Day")) |> 
  filter(n_throw > 0)|> 
  filter(Year == 1996 |Year == 2009| Year == 2021)|> 
  ungroup() |> 
  arrange(desc(ntot))

#----------------------------------------------
###Set up the data for jags in list format
#----------------------------------------------

data.ls <- list(y = as.matrix(data.matrix.obs |> select(8:67)),
                len = as.matrix(data.matrix.len |> select(8:67)),
                N = sum(rowSums(as.matrix(data.matrix.obs |> select(8:67)))>0),
                nunit = nrow(data.matrix.obs),
                nrep = ncol(data.matrix.obs |> select(8:67)),
                year = data.matrix.obs$Year,
                site = data.matrix.obs$Site)


write("model {

#---------
#main set up
#---------

mu.len <-26.5057
sd.len <-9.92816
tau.len <-1/(sd.len*sd.len)

#priors for detection model coefficients

b[1] ~ dnorm(0, 1.0E-6)
b[2] ~ dnorm(0, 1.0E-6)

#priors for existance model coefficients

a[1] ~ dnorm(0, 1.0E-6)
a[2] ~ dnorm(0, 1.0E-6)
a[3] ~ dnorm(0, 1.0E-6)
a[4] ~ dnorm(0, 1.0E-6)


#--------------
#main loop for occupancy
#--------------

for (i in 1:N) {
  ones[i] ~ dbern(psi[i])
  logit(psi[i]) <- a[1] + a[2]*year[i] + a[3]*site[i] + a[4]*year[i]*site[i]
  for(j in 1:nrep) {
    len[i,j]~dnorm(mu.len,tau.len)
    y[i,j] ~ dbern(theta[i,j])
    logit(theta[i,j]) <- b[1] + b[2]*len[i,j]
  }
}
for (i in (N+1):n_unit) {
  ones[i] ~ dbern(1-psi[i] + psi[i] * (1-theta[i,1]) * (1-theta[i,2]) * (1-theta[i,3]) * (1-theta[i,4])) *(1-theta[i,5])) * (1-theta[i,6])) * (1-theta[i,7])) * (1-theta[i,8])) * (1-theta[i,9]))  * (1-theta[i,10])) * (1-theta[i,11]) * (1-theta[i,12]) * (1-theta[i,13]) * (1-theta[i,14])) *(1-theta[i,15])) * (1-theta[i,16])) * (1-theta[i,17])) * (1-theta[i,18])) * (1-theta[i,19]))  * (1-theta[i,20]))  * (1-theta[i,21]) * (1-theta[i,22]) * (1-theta[i,23]) * (1-theta[i,24])) *(1-theta[i,25])) * (1-theta[i,26])) * (1-theta[i,27])) * (1-theta[i,28])) * (1-theta[i,29]))  * (1-theta[i,30])) * (1-theta[i,31]) * (1-theta[i,32]) * (1-theta[i,33]) * (1-theta[i,34])) *(1-theta[i,35])) * (1-theta[i,36])) * (1-theta[i,37])) * (1-theta[i,38])) * (1-theta[i,39]))  * (1-theta[i,40]))* (1-theta[i,41]) * (1-theta[i,42]) * (1-theta[i,43]) * (1-theta[i,44])) *(1-theta[i,45])) * (1-theta[i,46])) * (1-theta[i,47])) * (1-theta[i,48])) * (1-theta[i,49]))  * (1-theta[i,50]))* (1-theta[i,51]) * (1-theta[i,52]) * (1-theta[i,53]) * (1-theta[i,54])) *(1-theta[i,55])) * (1-theta[i,56])) * (1-theta[i,57])) * (1-theta[i,58])) * (1-theta[i,59]))  * (1-theta[i,60]))                      
  logit(psi[i]) <- a[1] + a[2]*year[i] + a[3]*site[i] + a[4]*year[i]*site[i]
  for(j in 1: n_rep) {
    len[i,j]~dnorm(mu.len,tau.len)
    logit(theta[i,j]) <- b[1] + b[2]*len[i,j]
  }
}
}",file = here("JAGS_mods", "jags_simple_marginal.txt"))

