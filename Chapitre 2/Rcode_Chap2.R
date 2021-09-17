#########################################
#   Lois usuelles discrètes
########################################

# Loi de Bernoulli

# génération de 100 réalisations B(1,0.4)
X=rbinom(100,1,0.4)
x[1:10]

# fonction de répartition

plot(-1:2, pbinom(-1:2, size = 1, prob = 0.4), type="s",lwd=3, xlab="",
     ylab="", col=2, main = "Fonction de répartition de Bin(1,0.4)")

# Loi binômiale

# génération de 100 réalisations B(10,0.4)
X=rbinom(100,10,0.4)
x[1:10]

# fonction de répartition

plot(-1:10, pbinom(-1:10, size = 10, prob = 0.4), type="s",lwd=3, xlab="",
     ylab="", col=2, main = "Fonction de répartition de Bin(10,0.4)")

# loi hypergéométrique

# génération de 100 réalisations B(10,0.4)
X=rhyper(100,5,12,7)
x[1:10]

# fonction de répartition

plot(0:7, phyper(0:7,5,12,7), type="s",lwd=3, xlab="",
     ylab="", col=2, main = "Fonction de répartition de H(Np=5,N=12,n=7)")

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




