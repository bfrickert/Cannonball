cars <- read.table('data/cannonball.tsv', sep='\t', fill= T, stringsAsFactors = F, header = T)
cars.s <-select(cars[c(-7,-10),], -car, -occupants)
cars.s$am <- factor(cars.s$am)
cars.s$won <- factor(cars.s$won)

glm.out<-glm(won ~ female.proportion, family = binomial(link = "logit"), cars.s)

plot(won ~ female.proportion, data=cars.s)
lines(cars.s$female.proportion, glm.out$fitted, type="l", col="red")
title(main="Likelihood of winning increases\nwith proportion of females in the car")

summary(glm.out)
