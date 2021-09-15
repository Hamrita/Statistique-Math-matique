# Fonction de répartition (v.a.d)
x=c(0,1,2,3)
p=c(0.1,0.25,0.3,0.35)
Fx=cumsum(p)
Fx
plot(x,Fx,type="s", lwd=3, col=2, xlab="x", ylab=expression(F(x)))

# Fonction de répartition de la loi uniforme [0,1]
x=seq(-1,2,len=10) # séquence de -1 jusqu'à 2 de longueur 10
Fx=punif(x, min=0, max=1)
Fx   # Afficher les valeurs de F(x)
plot(x,Fx,type="l", lwd=3, col=2, xlab="x", ylab=expression(F(x)))

# Fonction de répartition de la loi exponentielle (lambda=2)

x=seq(0,3.5, len=100)
Fx=pexp(x,2)
plot(x,Fx,type="l", lwd=3, col=2, xlab="x", ylab=expression(F(x)))
# ou encore
curve(pexp(x,2),0,4,lwd=3, col=2, xlab="x", ylab=expression(F(x)))