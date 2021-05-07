#Exercice indications.

SAMPLESEED= 34642
SIMULATIONSEED= 74658
muX=1
muY=0.4
sX=0.9
sY=0.7
rho123=0.41

# Generate Bivariate Gaussian Observations
n1= 140 
      #Ne jamais run les lignes du code d'en dessous individuellement, sinon ça ne marche pas.
gen_normal <- function(n1, muX=0, muY=0, sX=1, sY=1, rho=0){
  
  x <- rnorm(n1, muX, sX)
  z <- rnorm(n1, muY, sY * sqrt(1-rho^2))
  y <- sY/sX * rho * (x-muX) + z
  df <- data.frame(x=x, y=y)
  return(df)
}

# Generate data, normal setting
set.seed(SAMPLESEED)
df <- gen_normal(n=140, muX=1, muY=0.4, sX=0.9, sY=0.7, rho=0.41)
# true correlation coefficient in this setting
rho <- 0.41
plot(df, main=paste("Population correlation = ", round(rho, 3)))

# Compute simulated confidence interval, for normal setting, nonparametric bootstrap
# , spearman correlation coefficient
B <- 100  # number of bootstrap samples
M <- 1000  # number of monte carlo simulations

set.seed(SIMULATIONSEED)
CI.sim <- CI.sims(n = n,
                  setting = "nonlinear", # one of "normal", "outliers", "discrete", or "nonlinear"
                  method = "spearman",  # one of "pearson", "spearman"
                  type = "npboot",  # one of "boot", "npboot"
                  B = B,
                  M = M,
                  muX = muX,
                  muY = muY,
                  sX = sX,
                  sY = sY,
                  rho = rho,
                  out = out, # this parameter is ignored in the normal setting
                  dev = dev, # this parameter is ignored in the normal setting
                  angle = angle) # this parameter is ignored in the normal setting
install.packages(CI.sims)

#diaporama 10 on spearson

sample_size=c(10,20,50,100,150,200,500,1000,5000,10000)
M=10000
vec.means=rep(0,length(sample_size))
mat.stats=matrix(nrow=M,ncol=length(sample_size))
data$x = df$x
data$y = df$y

for(j in 1:length(sample_size)){
  stat.spear=rep(0,M)
  set.seed(SIMULATIONSEED)
  for(i in 1:M){
    data=rnorm(sample_size[j], 1, 0.4)
    stat.spear[i]=cor(df$x,df$y,method="spearman")
  }
  vec.means[j]=mean(stat.spear)
  mat.stats[,j]=stat.spear
}

dimnames(mat.stats)[[2]]=as.character(sample_size)
par(mar=c(5,6,1,1))



boxplot(mat.stats[,-c(1,2)],xlab="Sample size", ylab=" ", 
        cex.lab = 1.8, cex.axis = 1.8)
mtext(side = 2, text = expression(rho^S), cex= 1.8, line = 4,las=1)
abline(h=rho)

#(facultatif jsp à quoi ça sert)
plot(sample_size,vec.means,xlab="n",type="l",ylab=" ", las =1,ylim=c(0.5,0.7), cex.lab = 1.8, cex.axis = 1.8)
abline(h=rho)



