library(car)
library(ggplot2)
require(ggplot2)
dat <- read.table("data.txt", header=T, sep=";")
dat

#Graphs taste~Acetic

ggplot(dat, aes(x = Acetic, y = taste)) +
geom_point(colour="blue", shape=21, size=2) +
geom_smooth(method='lm', colour="red",se=FALSE)

# Linear model with Acetic
mod1 <- lm(taste~Acetic, data=dat, x=TRUE)
mod1

tab <- summary(mod1)
tab

#Graphs taste~H2S

ggplot(dat, aes(x = H2S, y = taste)) +
geom_point(colour="blue", shape=21, size=2) +
geom_smooth(method='lm', colour="red",se=FALSE)

# Linear model with H2S
mod2 <- lm(taste~H2S, data=dat, x=TRUE)
mod2

tab <- summary(mod2)
tab

#Graphs taste~Lactic

ggplot(dat, aes(x = Lactic, y = taste)) +
geom_point(colour="blue", shape=21, size=2) +
geom_smooth(method='lm', colour="red",se=FALSE)

# Linear model with Lactic
mod3 <- lm(taste~Lactic, data=dat, x=TRUE)
mod3
tab <- summary(mod3)
tab

#Multiple linear regression between response variable 
#taste and explanatory variable H2S and acetic.

mod4 <- lm(taste~H2S + Acetic, data=dat, x=TRUE)
mod4

tab <- summary(mod4)
tab

#Multiple linear regression between response variable 
#taste and explanatory variable H2S, and lactic.
mod5 <- lm(taste~H2S +  Lactic, data=dat, x=TRUE)
mod5

tab <- summary(mod5)
tab
#Multiple linear regression between response variable 
#taste and explanatory variables acetic and lactic.

mod6 <- lm(taste~Acetic +  Lactic, data=dat, x=TRUE)
mod6

tab <- summary(mod6)
tab

#Multiple linear regression between response variable 
#taste and explanatory variable H2S, acetic and lactic.

mod7 <- lm(taste~H2S + Acetic + Lactic, data=dat, x=TRUE)
mod7

tab <- summary(mod7)
tab

# Test of Overall Regression
X <- mod7$x
X
n <- nrow(X)
n
k <- ncol(X)-1
k
C <- cbind(rep(0,k), diag(1,k))
C

linearHypothesis(mod7,C)
# Test the hypothesis B1=B2 (Acetic has the same
# composition that the H2S to the tasting)
C <- rbind(c(0,1,-1,0))
C

linearHypothesis(mod7,C)

# Test the hypothesis B1=B3 (Acetic has the same
# composition that the lactic to the tasting)
C <- rbind(c(0,1,0,-1))
C
linearHypothesis(mod7,C)

#Test the individual hypotheses of nullity of each j parameter.
# Individual B0
C <- c(1,0,0,0)
C
linearHypothesis(mod7,C)

# Individual B1
C <- c(0,1,0,0)
C
linearHypothesis(mod7,C)

# Individual B2
C <- c(0,0,1,0)
C
linearHypothesis(mod7,C)

# Individual B3
C <- c(0,0,0,1)
C
linearHypothesis(mod7,C)

#Test the following hypothesis: 2B1=B2=B3
C <- rbind(c(0,2,-1,0),c(0,0,1,-1))
C
linearHypothesis(mod7,C)
# Test B1-B2 = 10; CB = t
C <- rbind(c(0,1,-1,0))
C
linearHypothesis(mod7,C,rhs=10)

# Curve normal for the standardized residues.

res <- residuals(mod5)
res

py <- fitted(mod5)
py

pDat <- as.data.frame(cbind(i=seq(1:nrow(dat)), e=res,
yf=py, x3=dat$x3, x4=dat$x4))
pDat

# Scatter plot e vs y
ggplot(pDat, aes(x = yf, y = e)) +
geom_point(colour="darkblue", shape=21, size=2) +
ylab("Residuals") +
xlab("Predicted values")

# Q-Q Plot
# Standarized residuals are:
res
# Now we calculate observed quantiles:
res1_y <- quantile(res, c(0.25, 0.75))
res1_y
# And then the theoretical quantiles
#assuming that the residuals comes 
#from a standard normal distribution):
res1_x <- qnorm(c(0.25, 0.75))
res1_x
# Calculating the slope of the line
slope <- diff(res1_y)/diff(res1_x)
# Calculating the intercept of the line
inter <- res1_y[1] - slope * res1_x[1]
# Finally:
ggplot(as.data.frame(res), aes(sample=res))+
stat_qq() +
geom_abline(slope = slope, intercept = inter, color="blue")+
ylab("Residuals") +
xlab("nScores")

# Summary best model
summary(mod5)
plot(mod5)
res <- residuals(mod5)
res
sum(res)
plot(res)

py <- fitted(mod5)
py

pDat <- as.data.frame(cbind(i=seq(1:nrow(dat)), e=res,
yf=py, H2S=dat$H2S, Lactic=dat$Lactic))
pDat

res1 <- rstandard(mod5)
res1
res2 <- rstudent(mod5)
res2
pDat <- cbind(pDat, rStan=res1, rStud=res2)
pDat

# Fitted values vs residuals
ggplot(pDat) +
geom_point(aes(x = yf, y = e), colour="darkblue",
 shape=16, size=2) +
geom_point(aes(x = yf, y = rStan), colour="darkred", 
shape=17, size=2) +
geom_point(aes(x = yf, y = rStud), colour="yellow", 
shape=18, size=2) +
ylab("Residuals") +
xlab("Fitted values")












