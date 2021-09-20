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

############################################
#  Fonction Gamma
##########################################

# gamma(4)

gamma(4)

# gamma(5/2)

gamma(5/2)

3*sqrt(pi)/4
