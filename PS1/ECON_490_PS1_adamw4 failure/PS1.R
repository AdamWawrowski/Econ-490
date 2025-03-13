# Question 4

euclidean <- function(a, b) sqrt(sum((a - b)^2))

zero <- c(0, 0, 0)
  
euclidean(c(0, 3, 0), zero)
euclidean(c(2, 0, 0), zero)
euclidean(c(0, 1, 3), zero)
euclidean(c(0, 1, 2), zero)
euclidean(c(-1, 0, 1), zero)
euclidean(c(1, 1, 1), zero)



curve(2.5/(x-5), from = 5, to = 40, ylim = c(0, 2), type="l",  main="Bias-Variance", 
      xlab="Flexibility", ylab="Variance", col = "red") 
#squared bias

curve(.01*(0.5*x^2-20*x+305), from = 2, to = 40, type="l",  main="main title", 
      xlab="Flexibility", ylab="Variance", col = "blue", add = TRUE) 
#test mse


curve(1.6^(x-40)+.05, from = 2, to = 40, type="l",  main="main title", 
      xlab="Flexibility", ylab="Variance", col = "orange", add = TRUE)
#Variance

curve(-((x-20)^3/5000)+1, from = 2, to = 40, type="l",  main="main title", 
      xlab="Flexibility", ylab="Variance", col = "purple", add = TRUE) 
#training mse

abline(h = 1)
#error

legend(29, 1.5, legend=c("Training MSE", "Test MSE", "Squared Bias", 
                         "Variance", "Error"), c("purple","blue","red","orange",
                                                 "black"))

setwd("C:/Users/adamw/Desktop/ECON426")
getwd()

college <- read.csv("College.csv")

rownames(college) <- college[, 1]
View(college)

college <- college[, -1]
View(college)

head(college)

summary(college)


pairs(college[, 2:10])
# Could not pair column 1 under a non-numeric argument error

boxplot(college$Outstate ~ college$Private)

Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

summary(Elite)

par(mfrow = c(2, 2))

hist(college$Apps)
hist(college$PhD)
hist(college$Accept)
hist(college$Enroll)

boxplot(college$Grad.Rate ~ college$S.F.Ratio)

par(mfrow = c(1, 2))
boxplot(college$Expend ~ college$Private)
boxplot(college$Expend ~ college$Elite)


