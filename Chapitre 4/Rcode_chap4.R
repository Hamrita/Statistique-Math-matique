#########################################
#  Illustrations des différents
#    modes de convergences
#########################################

# Installation du package: ConvergenceConcepts
install.packages("ConvergenceConcepts")

# chargement du package

library(ConvergenceConcepts)

#####################################
# Convergence en probabilité

# Exercice 1 (cours)
# Xi ~ U[0,1] et Yn=min(Xi)

Yn=function(n) cummin(runif(n))

check.convergence(10000,1000,Yn,mode="p")