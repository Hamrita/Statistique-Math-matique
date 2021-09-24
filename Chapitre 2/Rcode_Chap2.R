#########################################
#   Lois usuelles discrètes
########################################

# Loi de Bernoulli

# génération de 100 réalisations B(1,0.4)
X=rbinom(100,1,0.4)
X[1:10]

# fonction de répartition

plot(-1:2, pbinom(-1:2, size = 1, prob = 0.4), type="s",lwd=3, xlab="",
     ylab="", col=2, main = "Fonction de répartition de Bin(1,0.4)")

# Loi binômiale

# génération de 100 réalisations B(10,0.4)
X=rbinom(100,10,0.4)
X[1:10]

# fonction de répartition

plot(-1:10, pbinom(-1:10, size = 10, prob = 0.4), type="s",lwd=3, xlab="",
     ylab="", col=2, main = "Fonction de répartition de Bin(10,0.4)")

# loi hypergéométrique

# génération de 100 réalisations H(Np=5,N=12,n=7)
X=rhyper(100,5,12,7)
X[1:10]

# fonction de répartition

plot(0:7, phyper(0:7,5,12,7), type="s",lwd=3, xlab="",
     ylab="", col=2, main = "Fonction de répartition de H(Np=5,N=12,n=7)")

# loi géométrique

# génération de 100 réalisations G(p=0.35)
# Sous R, le support de X est 0,1,2,...
X=rgeom(100,0.35)+1
X[1:10]

# La densité

plot(dgeom(0:15,0.35), type="h", lwd=3, xlab="",
     ylab="", col=2, main = "Fonction de probabilité de G(p=0.35)")

# fonction de répartition

plot(0:13, pgeom(0:13,0.35), type="s",lwd=3, xlab="",
     ylab="", col=2, main = "Fonction de répartition de G(p=0.35)")

#################################################
#     lois usuelles continues
################################################

# install.packages("ggplot2")
library(ggplot2)

x <- seq(from = 0, to = 6, length.out = 200) # Define the density domains
ylim <- c(0, 0.6)

#######
df <- rbind(
  data.frame(x = x, dist_name = "Uniforme"  , y = dunif(x, min   = 2, max = 4)),
  data.frame(x = x, dist_name = "Normale"     , y = dnorm(x, mean  = 3, sd = 1)),
  data.frame(x = x, dist_name = "Exponentielle", y = dexp(x, rate  = 1 / 2)),
  data.frame(x = x, dist_name = "Gamma"      , y = dgamma(x, shape = 2, rate = 1)) )

############
ggplot(data = df, aes(x = x, y = y)) +
  geom_line(size=1.3, color="red") +
  facet_wrap(~dist_name)+theme(plot.title=element_text(size=20,face="bold"))

#################################
#   Loi normale
#################################

# Fonction de répartition (loi normale standard)

# P(X <= 1.65)

pnorm(1.65)

# P(X > 1.65)

pnorm(1.65, lower.tail = FALSE)

# ou encore

1-pnorm(1.65)

# P(X<= -1.25)

pnorm(-1.25)

# P(|X| <= 1.65)= 2*phi(1.65)-1

2*pnorm(1.65) - 1

# loi normale quelconque X~N(m,sigma)

# P(X<=2.3)=?  avec X~N(1.5,sigma=0.5)

pnorm(2.3,mean=1.5,sd=0.5)


#  Quantiles: q? tel que P(X <= q)=p
# qnorm(p)

qnorm(0.05, mean=0, sd=1)  # quantile de alpha=5% pour loi normale standard

qnorm(0.95)    # quantile de alpha=95% pour loi normale standard

qnorm(0.05,lower.tail = FALSE)  # quantile de 1-0.05 (loi normale standard)

qnorm(0.95, 2,0.5)  # quantile de 95% pour la loi N(2,0.5)

########################################
#  Table de la loi normale standard
#######################################
x=seq(0,3.5,by=0.1)
y=seq(0,0.09,by=0.01)
Tab=outer(x,y,"+")
Tab=pnorm(Tab)
colnames(Tab)=as.character(seq(0,0.09,by=0.01))
rownames(Tab)=as.character(seq(0,3.5,by=0.1))

head(Tab)  # afficher les six premières lignes

Tab["1.6", "0.05"]  # phi(1.65)

############################################
#  Fonction Gamma
##########################################

# gamma(4)

gamma(4)

# gamma(5/2)

gamma(5/2)

3*sqrt(pi)/4

############################################
#   Loi Gamma
############################################

alpha=c(1,1.5,3.5); lambda=c(2,2,7); cc=c(4,5)
xx=seq(0.01,6,len=100)
plot(xx,dgamma(xx,alpha[1], rate=lambda[1]), col=2, lwd=3,xlab="",
     ylab="", type="l", xlim=c(0,3),
     main=expression("Densité de "~ Gamma(alpha,lambda)))
for(i in 2:3){
  lines(xx, dgamma(xx,alpha[i],rate=lambda[i]), col=cc[(i-1)], lwd=3)
}
legend("topright", c(expression(alpha==1 ~"," ~ lambda==2),
                   expression(alpha==1.5 ~"," ~ lambda==2),
                   expression(alpha==3.5 ~"," ~ lambda==7)),bty="n",
                   lty=1, lwd=3, col=c(2,4,5))

##############################################
#  Loi khi-deux
#############################################
nn=c(3,5,7)
xx=seq(0.01,20,len=100)
plot(xx,dchisq(xx,nn[1]), col=2, lwd=3,xlab="",
     ylab="", type="l", xlim=c(0,20),
     main=expression("Densité de "~ chi^2 ~""~ (n)))
for(i in 2:3){
  lines(xx, dchisq(xx,nn[i]), col=cc[(i-1)], lwd=3)
}
legend("topright", c(expression(n==3),
                     expression(n==5),
                     expression(n==7)),bty="n",
       lty=1, lwd=3, col=c(2,4,5))

##############################################
#   Loi Beta de première espèce
#############################################

beta1=function(x,p,q){
  ifelse(x >0, gamma(p+q)/(gamma(p)*gamma(q))*x^{p-1}*(1+x)^{-p-q},0)
}

xx=seq(0.01,3,len=200)
pp=c(0.5,1,3/2,4); qq=c(0.5,3,2,5)

plot(xx,beta1(xx,pp[1],qq[1]), xlim=c(0,2.5),ylim=c(0,1.3), lwd=3, col=1, 
     type="l", xlab="", ylab="",main=expression("Densité de "~ B[1](p,q)))
for(i in 2:4){
  lines(xx,beta1(xx,pp[i], qq[i]), lwd=3, col=i)
}
legend("topright", c(expression(p==0.5 ~"," ~q==0.5),
                     expression(p==1~ "," ~q==3),
                     expression(p==frac(3,2) ~"," ~q==2),
                     expression(p==4~ "," ~q==5)),bty="n",
       lty=1, lwd=3, col=1:4)

######################################################
#    Loi beta deuxième espèce
######################################################

xx=seq(0.001,1,len=200)
pp=c(0.5,5,1,2,2); qq=c(0.5,1,3,2,5)

plot(xx,dbeta(xx,pp[1],qq[1]), xlim=c(0,1), ylim=c(0,3),lwd=3, col=1, 
     type="l", xlab="", ylab="",main=expression("Densité de "~ B[1](p,q)))
for(i in 2:5){
  lines(xx,dbeta(xx,pp[i], qq[i]), lwd=3, col=i)
}
legend("top", c(expression(p==0.5 ~"," ~q==0.5),
                     expression(p==5~ "," ~q==1),
                     expression(p==1 ~"," ~q==3),
                     expression(p==2~ "," ~q==2),
                     expression(p==2~ "," ~q==5)),bty="n",
       lty=1, lwd=3, col=1:4)

####################################################
#  Loi de Student
###################################################

nn=c(1,5,10,Inf)
xx=seq(-5,5,len=200)
plot(xx,dt(xx,nn[1]), col=1, lwd=3,xlab="",
     ylab="", type="l", ylim=c(0,0.4),
     main=expression("Densité de "~ T(n)))
for(i in 2:4){
  lines(xx, dt(xx,nn[i]), col=i, lwd=3)
}
legend("topright", c(expression(n==1),
                     expression(n==5),
                     expression(n==10),
                     expression(n==infinity)),bty="n",
       lty=1, lwd=3, col=1:4)

####################################################
# Loi de Fisher
###################################################
n1=c(1,5,10,100);  n2=c(1,8,4,50)
xx=seq(0,3,len=200)
plot(xx,df(xx,n1[1],n2[1]), col=1, lwd=3,xlab="",
     ylab="", type="l", ylim=c(0,2),
     main=expression("Densité de "~ F(n[1],n[2])))
for(i in 2:4){
  lines(xx, df(xx,n1[i], n2[i]), col=i, lwd=3)
}
legend("topright", c(expression(n[1]==1 ~ "," ~n[2]==1),
                     expression(n[1]==5 ~ "," ~n[2]==8),
                     expression(n[1]==10 ~ "," ~n[2]==4),
                     expression(n[1]==100 ~ "," ~n[2]==50)),bty="n",
       lty=1, lwd=3, col=1:4)
