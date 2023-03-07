###The purpose of this code is to generate trials for the clearing
#efficiency experiment of TT. The ultimate goal of the experiment
#is to develop a logistic regression model to predict clearing 
#efficiency by shell length (SL) of apple snails which could be used in the future
#to correct apple snail densities for the time series based on the size of snails
#caught

#experimentally it is rather simple: place marked snails of a known length into throw
#traps and then see if we clear them from the throw trap. I am purposely adding unknown
#snail densities (0-3 per trial) and am going to try and make the trials blind or semiblind
#to reduce experimental error from the crew (i.e., trying to work harder to clear snails
#when known they are in trap).  An additional design aspect, I'm going to bin snails 
#in 10mm bins (i.e., 3-10mm, 10.1-20mm, 20.1-30mm,30.1-40mm) to get a representative size
#range.

#this being said this script is going to randomly select between trial density (0-3) then
#using the trial density randomly select each snails length bin. With a goal of getting upto
#200 observations.  

#---------------------------------
####Libraries####
#---------------------------------

library(here)

#---------------------------------
####code####
#---------------------------------

#vector of densities
densities <- 0:3

#vector of bins
#bin: R = 3.0-10.0mm, G = 10.1-20.0mm, W = 20.1-30.0mm, P = 30.1-40.0mm

bins <- c("R","G","W","P")

#data frame for trial and densities
#note I wanted it less likely to get 0s while the others equally likely

trials <- data.frame(trial = 1:150,
                     density = sample(densities,size = 150, replace = T, prob = c(.1,.3,.3,.3)))

#set up column of bins to house results of for loop

trials$bins <- 0

#for loop to sample our bins without replacement using our density. I'm using without
#replacement so that I don't get two of the same bin in a trial, Paste all bins into
#a character string separated by a ,


for (i in 1:length(trials$trial)) {
  trials$bins[i] <- ifelse(trials$density[i]==0,
                           yes = "none",
                           no = paste(sample(bins,size = trials$density[i]), collapse = ","))
}


###save results as a .csv which will be copied to a .xlsx file in the field data sheets
###for reference

write.csv(trials, file = here("out","clearingefficiency_trials.csv"))



