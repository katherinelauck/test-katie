library(rjags)

c <- read.csv("./data/ohio_cancer_hw.csv")

cat("model{

# Fixed effects

for (c in 1:n.county){
  for (g in 1:n.gender){
    for (t in 2:n.year){
    y[c,g,t] ~ dnorm(y.hat[c,g,t],tau.y)
    y.hat[c,g,t] <- y[c,g,t-1] + trend*year[t] + g.trend*year[t]*gender[g] + b*gender[g] + a[c]
    }
  }
}

# Random effects
for (j in counties) {
    a[j] ~ dnorm(mu.a, tau.a)
}
  
# priors
trend ~ dnorm(0, 0.1)
trend ~ dnorm(0, 0.1)
tau.y <- pow(sigma.y, -0.5)
tau.a <- pow(sigma.a, -0.5)
sigma.y ~ dnorm(0, 0.5)T(0,)
sigma.a ~ dnorm(0, 0.5)T(0,)
b ~ dnorm(0, 0.1) #  and slope
mu.a ~ dnorm(0, 0.1) # overall mean
}", fill = TRUE, file = './results/cancer_jags.txt')

newc <- c[ c$lograte == ave(c$lograte, c$county, c$gender, c$year,
                            FUN = function(x) {return(x[1])}), ]
newc <- newc[which(ave(newc$lograte,newc$county,FUN = length) == 42),]
head(newc)

cnewdata_jags <- list(y = newc$lograte,n.gender = 2,n.year = 21,county.index = newc$county,gender = newc$gender,counties = unique(newc$county),n.county = 41,year = newc$year,N = length(newc$lograte))

cancer <- jags.model("./results/cancer_jags.txt",data = cnewdata_jags,n.chains = 3, n.adapt = 3000)

cancer.samp <- coda.samples(cancer,c('mu.a','sigma.a','sigma.y','trend','g.trend','b'),n.iter = 30000,thin = 30)
