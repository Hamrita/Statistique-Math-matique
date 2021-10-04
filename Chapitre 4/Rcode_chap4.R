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
# Xi ~ U[0,1], Yn=min(Xi) et Zn=max(Xi)

Yn=function(n) cummin(runif(n))

Zn=function(n) cummax(runif(n))

check.convergence(10000,1000,Yn,mode="p")  # Yn cvge en prob vers 0

check.convergence(10000,1000,Zn,mode="p")  # Zn cvge en prob vers 1