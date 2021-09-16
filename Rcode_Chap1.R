
##############################
#    Exercice 1
##############################

# Fonction de répartition (v.a.d)
x1=c(0,1,2,3)
p=c(0.1,0.25,0.3,0.35)
Fx1=cumsum(p)
Fx1
plot(x1,Fx1,type="s", lwd=3, col=2, xlab="x", ylab=expression(F(x)))

# Fonction de répartition de la loi uniforme [0,1]
x2=seq(-1,2,len=10) # séquence de -1 jusqu'à 2 de longueur 10
Fx2=punif(x, min=0, max=1)
Fx2   # Afficher les valeurs de F(x)
plot(x2,Fx2,type="l", lwd=3, col=2, xlab="x", ylab=expression(F(x)))

# Fonction de répartition de la loi exponentielle (lambda=2)

x3=seq(0,3.5, len=100)
Fx3=pexp(x,2)
plot(x3,Fx3,type="l", lwd=3, col=2, xlab="x", ylab=expression(F(x)))
# ou encore
curve(pexp(x,2),0,4,lwd=3, col=2, xlab="x", ylab=expression(F(x)))

#####################################
#  Exercice 2
####################################

# 1)
E_x1=sum(x1*p) # ou crossprod(x1,p)
E_x1

# 2)

f2=function(x) x
E_x2=integrate(f2,0,1)$value
E_x2

# 3)
f3=function(x) 2*x*exp(-2*x)
E_x3=integrate(f3,0,+Inf)$value
E_x3

#################################
#     Exercice 3
#################################

# 1)
E_x11=sum(x1^2*p) # ou crossprod(x1^2,p)
E_x11

# 2)

f22=function(x) x^2
E_x22=integrate(f22,0,1)$value
E_x22

# 3)
f33=function(x) 2*x^2*exp(-2*x)
E_x33=integrate(f33,0,+Inf)$value
E_x33

##############################
#    Exercice 4
#############################

# v(x)=E(X^2)-E(X)^2=1/lambda^2=1/(2^2)=0.25

vx=E_x33 - E_x3^2
vx

#############################
#     Exercice 5
#############################

install.packages("devtools")
devtools::install_github("alexandernel14/MGF",force = T)
MGF::mgf("Exponential")

# espérance mathématique pour lambda=2

MGF::MGF_evaluator("Exponential", t=0, order_of_moment = 1, lambda=2)