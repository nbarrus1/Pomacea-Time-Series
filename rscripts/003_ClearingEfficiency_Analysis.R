#------------------------------------------------------------
####logistic regression for capture efficiency by size####
#------------------------------------------------------------

#plot

clearing_data %>% 
  ggplot(aes(x = SL, y = Capture))+
  geom_point(shape = 21)+
  geom_smooth(method = "glm", method.args=list(family="binomial"), se = T)+
  theme_classic()+
  coord_cartesian(ylim = c(0,1))

library(lme4)


mod1 <- glm(Capture~SL,data = clearing_data, family = binomial(link = 'logit'))
mod2 <- glm(Capture~ 1 ,data = clearing_data, family = binomial(link = 'logit'))

summary(mod1)
summary(mod2)
