# Getting started with Matrix Models

# libraries we need
# install these first (only once!!) -> over there in packages tab.
library(tidyverse)
library(popbio)

## matrix multiplication Basics
myMat <- matrix(c(0,2,0.5,0.1), nrow = 2, byrow = TRUE)
myMat

# vector of starting population sizes
n0<-c(10,10)

# multiply once
myMat %*% n0

# LOOP to multiply
# collection zone to collect J and A numbers

times <- 50
pops <- matrix(data = NA, nrow = times, ncol = 2)
pops[1,] <- n0

for (i in 2:times){
  pops[i,]<-myMat %*% pops[(i-1),]
}

# quick trick
matplot(pops, type = 'l')

# ggplot it
plotPops <- as.data.frame(pops)
names(plotPops) <- c("J", "A")
plotPops <- mutate(plotPops, totalN = J+A, time = 1:times)
plotPops

ggplot(plotPops, aes(x = time , y = J))+
  geom_line()+
  geom_line(aes(x = time, y = A), colour = 'red')+
  geom_line(aes(x = time, y = totalN), colour = "green")+
  scale_y_log10()

# roughly calcuate stable age
mutate(plotPops, propJ = J/totalN, propA = A/totalN) %>% tail

## eigen magic
eigen(myMat)
# stable age
eigen(myMat)$vectors[,1]/sum(eigen(myMat)$vectors[,1])

# popbio magic
lambda(myMat)
stable.stage(myMat)
n <- c(10,10)
out <- pop.projection(myMat, n, 50)
names(out)

# some plotting
stage.vector.plot(out$stage.vectors)
matplot(t(out$stage.vectors), type = 'l')
