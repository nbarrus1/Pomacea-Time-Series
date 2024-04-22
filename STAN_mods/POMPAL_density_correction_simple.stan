
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
      
