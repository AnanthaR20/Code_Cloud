#12.1
d <- read.table("http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P339.txt",header = TRUE)

model <- glm(Y~ X1 + X2 + X3, data = d)

residual <- resid(model)

mu <- mean(residual)
sig <- sd(residual)

#points of interest index: 9,52,53

z1 <- (residual[9] - mu)/sig
z2 <- (residual[52] - mu)/sig
z3 <- (residual[53] - mu)/sig

exclude <- -1*c(9,52,53)

d.new <- d[exclude, ]

model2 <- glm(Y~X1+X2+X3,data = d.new)
##################################################
#12.2
model3 <- glm(Y~X1+X2, data = d)
plot(model3)
#################################################
#12.3
d <- read.table("http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P012.txt", header = TRUE)

logmodel <- glm(Damaged~Temp,data = d)

d.new <- d[-18,]
d.new

logmodel2 <- glm(Damaged~Temp,data = d.new)
a <- predict(logmodel2)
#################################################
#Q2
d <- read.table("http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P160.txt", header = TRUE)

model <- lm(V~ ., data = d)
summary(model)

a1 <- lm(V~I+D1+D2+W+G:I+P+N, data=d)
a1
################################################
#Q3
d <- read.table("http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P011.txt", header = TRUE)
plot(d$Diam,d$Time)

model <- lm(Price ~ .,data = d)
summary(model)

