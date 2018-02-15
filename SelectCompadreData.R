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
