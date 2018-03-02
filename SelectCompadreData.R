library(tidyverse)
library(popbio)
library(matrixcalc)


load('~/Google Drive/###Teaching Active/2017_18/APS 273/Workshops/COMADRE_v.2.0.1.RData')
length(comadre$mat) # 1927


# Get some A-matrices of dimension 5 ----
set.seed(1)
out <- map(comadre$mat[sample(1:1927,300)], 'matA', dim)
dims<-map_dbl(out, function(x) dim(x)[1])
use <- out[dims==5]

# Get rid of any with NA ----
noNA<-map(.x = use, .f=function(x) all(!is.na(x)))
idx<-which(noNA == TRUE)

use2 <- use[idx]

# Check singularity using matrixcalc library and remove ----
notSing <- map(.x = use2, .f=function(x) !is.singular.matrix(x))
idx2 <- which(notSing == TRUE)

use3 <- use2[idx2]

# look at the ones left ----
map(.x = use3, .f=function(x) elasticity(x)) # some baddies still

idx3 <- c(2,8,10,15) # don't use these

# Final set ----
useStudent <- use3[-idx3]
length(useStudent)

# all elasticities look interesting
map(.x = useStudent, .f=function(x) elasticity(x))

# save to give out ----
save(useStudent, file = 'studentMatrices.RData')

myMatrix <- useStudent[[10]]


## ANSWERS

load("/Users/apb/Google Drive/###Teaching Active/2017_18/APS 273/Workshops/studentMatrices.RData")

stuff <- function(mat){
  ll <- lambda(mat)
  ss <- stable.stage(mat)
  pp <- pop.projection(mat, rep(10,5), 50)
  ee <- elasticity(mat)
  sen <- sensitivity(mat)
  
  out<-list(lambda = ll, stable_stage = ss,
                  elasticity = ee, sensitivity = sen)
  
  pd<-data.frame(time= 1:50, numbers = pp$pop.sizes, t(pp$stage.vectors))
  names(pd)[3:7] <- c("S1","S2","S3","S4","S5")
  
  print("-----")
  cat('\n')
  
  print(mat)
  print("-----")
  cat('\n')
  
  print(out)
  
  ggplot(pd, aes(x = time, y = numbers))+
    geom_line()+
    geom_line(data = pd, aes(x = time, y= S1), col = 'red')+
    geom_line(data = pd, aes(x = time, y= S2), col = 'green')+
    geom_line(data = pd, aes(x = time, y= S3), col = 'blue')+
    geom_line(data = pd, aes(x = time, y= S4), col = 'purple')+
    geom_line(data = pd, aes(x = time, y= S5), col = 'yellow')+
    scale_y_log10()
}


stuff(useStudent[[1]])

map(.f = stuff, .x = useStudent)
