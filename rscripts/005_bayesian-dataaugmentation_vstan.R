
#libraires

library(R2jags)
library(rstan)
options(mc.cores = parallel::detectCores())


#----------------
#matrix format
#------------------


data.matrix.obs <- SRS.complete |> 
   group_by(Year, Period,Site,Plot) |> 
   mutate(ind = row_number(),
          species.factor = if_else(Species=="POMPAL", true = 1,false = 0)) |> 
   ungroup() |> 
   select(-Length_mm,-Comments,-Throw,-Species) |> 
   pivot_wider(names_from = ind, values_from = species.factor, values_fill = 0) |> 
   add_column(`15`=0,
              `16`=0,
              `17`=0,
              `18`=0,
              `19`=0,
              `20`=0,
              `21`=0,
              `22`=0,
              `23`=0,
              `24`=0,
              `25`=0,
              `26`=0,
              `27`=0,
              `28`=0,
              `29`=0,
              `30`=0,
              `31`=0,
              `32`=0,
              `33`=0,
              `34`=0,
              `35`=0) |> 
   left_join(plot_throws, by = c("Year", "Period","Site","Plot","Month","Day")) |> 
   filter(n_throw > 0) |> 
   ungroup() |> 
   mutate(nplot = row_number()) |> 
   pivot_longer(8:42,names_to = "snail", values_to = "presence")
   

data.matrix.len <- SRS.complete |> 
   group_by(Year, Period,Site,Plot) |> 
   mutate(ind = row_number()) |> 
   ungroup() |> 
   select(-Comments,-Throw,-Species) |> 
   pivot_wider(names_from = ind, values_from = Length_mm, values_fill = NA)|> 
  add_column(`15`=as.numeric(NA),
             `16`=as.numeric(NA),
             `17`=as.numeric(NA),
             `18`=as.numeric(NA),
             `19`=as.numeric(NA),
             `20`=as.numeric(NA),
             `21`=as.numeric(NA),
             `22`=as.numeric(NA),
             `23`=as.numeric(NA),
             `24`=as.numeric(NA),
             `25`=as.numeric(NA),
             `26`=as.numeric(NA),
             `27`=as.numeric(NA),
             `28`=as.numeric(NA),
             `29`=as.numeric(NA),
             `30`=as.numeric(NA),
             `31`=as.numeric(NA),
             `32`=as.numeric(NA),
             `33`=as.numeric(NA),
             `34`=as.numeric(NA),
             `35`=as.numeric(NA))|> 
  left_join(plot_throws, by = c("Year", "Period","Site","Plot","Month","Day"))|> 
  filter(n_throw > 0)|> 
  ungroup() |> 
  mutate(nplot = row_number()) |> 
  pivot_longer(8:42,names_to = "snail", values_to = "length")

data.long.full <- data.matrix.obs |> 
  left_join(data.matrix.len, by = c("Year","Period","Site","Plot","Region","Month","Day",
                                    "Date","n_throw","nplot","snail")) |> 
  filter(Year == 1999|Year == 2021)

write_csv(data.long.full, file = here("data","longformatdata.csv"))
write_csv(SRS.complete, file = here("data","srs_complete.csv"))

data.long.obs <- data.long.full |> 
  filter(!is.na(length))

data.long.imp <- data.long.full |> 
  filter(is.na(length))



write("
data{
//mu and sigma for imputed lengths

real mu_len;
real sig_len;

//nplot for calculate plot densities

int <lower = 0> nplot;

//split the data into observed and missing data

int <lower = 0> n_obs;
int <lower = 0> n_mis;
int <lower = 1, upper = n_obs+n_mis> ii_obs[n_obs];
int <lower = 1, upper = n_obs+n_mis> ii_mis[n_obs];
int y_obs[n_obs];
int y_mis[n_mis];
real year_obs [n_obs];
real year_mis [n_mis];

//the imputed data

real len_obs[n_obs];

}


transformed data {
//combine the data that is not imputed into a single vector

int <lower = 0> N = n_obs + n_mis;
int <lower = 0> y[N];
real <lower = 0> year[N];
y[ii_obs] = y_obs;
y[ii_mis] = y_mis;
year[ii_obs] = year_obs;
year[ii_mis] = year_mis;

}


parameters{
real len_mis[n_mis];
real z[N];
real a_det;
real b_det;
real a_exi;
real b_exi;
real mu[N];
}


transformed parameters{
real len[N];
real <lower = 0, upper = 1> p_detect[N];
real <lower = 0, upper = 1> p_exist[N];
len[ii_obs] = len_obs;
len[ii_mis] = len_mis;


for (i in 1:N){
p_detect[i] = inv_logit(a_det + b_det * len[i]);
p_exist[i] = inv_logit(a_exi + b_exi * year[i]);
}

}


model{
//Priors
a_det ~ normal(0,1000);
b_det ~ normal(0,1000);
a_exi ~ normal(0,1000);
b_exi ~ normal(0,1000);


for(i in 1:N){
//impute lengths
len[i] ~ normal(mu_len,sig_len);


//states
z[i]~bernoulli(p_exist[i]);

//Likelihood
mu[i] = z[i]*p_detect[i];

y[i] ~ bernoulli(mu[i]);

}
}


generated quantities {

real LL[N];
for(i in 1:N) {
LL[i] = bernoulli_lpmf(y[i]|mu[i]);
}
}      
      ", file = here("STAN_mods","POMPAL_density_correction_simple.stan"))


data.ls <- list(mu_len = 26.5057,
                sig_len = 9.92816,
                n_obs = nrow(data.long.obs),
                n_mis = nrow(data.long.imp),
                y_obs = data.long.obs$presence,
                y_mis = data.long.imp$presence,
                year_obs = data.long.obs$Year,
                year_mis = data.long.imp$Year,
                len_obs = data.long.obs$length)





POMPAL_correction_stan_simple <- stan(file = here("STAN_mods","POMPAL_density_correction_simple.stan"),
                                      data = data.ls)







####Jags first attempt####


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

#data.ls <- list(y = as.matrix(data.matrix.obs |> select(8:42)),
#                len = as.matrix(data.matrix.len |> select(8:42)),
#                nplot = nrow(as.matrix(data.matrix.obs |> select(8:42))),
#                ncol = ncol(as.matrix(data.matrix.obs |> select(8:42))),
#                year = data.matrix.obs$Year,
#                nthrow = data.matrix.obs$n_throw)

###write jags model


#write("model {
#---------
#main set up
#---------

#mu.len <-26.5057
#sd.len <-9.92816
#tau.len <-1/(sd.len*sd.len)

#priors for detection model coefficients

#a1 ~ dnorm(0, 1.0E-6)
#b1 ~ dnorm(0, 1.0E-6)

#priors for existance model coefficients

#a2 ~ dnorm(0, 1.0E-6)
#b2 ~ dnorm(0, 1.0E-6)

#---------
#main loop
#---------

#for(i in 1:nplot) {
#  for(j in 1:ncol) {
#    len[i,j]~dnorm(mu.len,tau.len)
#    logit(P.detect[i,j])<-a1+b1*len[i,j]
##    logit(P.exist[i,j])<- a2+b2*year[i]
#    z[i,j]~dbern(P.exist[i,j])
#    mu[i,j]<-z[i,j]*P.detect[i,j]
#    y[i,j]~dbern(mu[i,j])
#    LL[i,j]<-log((y[i,j]*mu[i,j])+((1-y[i,j])*(1-mu[i,j])))
#  }
 #calculated quantities
# N.plot[i]<-sum(z[i,])  
# D.plot[i]<-N.plot[i]/nthrow[i]
#}
#      }", file = here("JAGS_mods", "POMPAL_density_correction_simple.txt"))

#run jags model

init1 <- list(z = as.matrix(data.matrix.obs |> select(8:42)))
init2 <- list(z = as.matrix(data.matrix.obs |> select(8:42)))

POMPAL_Density_simple_jags <- jags(data.ls, inits = list(init1,init2),
                                   model.file = here("JAGS_mods", "POMPAL_density_correction_simple.txt"),
                                   parameters.to.save = c("a1","b1","a2","b2","LL","N.plot","D.plot"),
                                   n.chains = 2, n.iter = 100000, n.burnin = 10000, n.thin = 64)

