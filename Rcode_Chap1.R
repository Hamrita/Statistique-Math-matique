# Fonction de répartition (v.a.d)
x=c(0,1,2,3)
p=c(0.1,0.25,0.3,0.35)
plot(x,p,type="s", lwd=3, col=4)

# Fonction de répartition de la loi uniforme [0,1]
x=seq(-1,2,len=10) # séquence de -1 jusqu'à 2 de longueur 10
Fx=dunif(x, min=0, max=1)
Fx   # Afficher les valeurs de F(x)
plot(x,Fx,type="s", lwd=3, col=4)