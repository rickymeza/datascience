library(dslabs)
data(heights)
options(digits = 3)

height <- heights$height
avgheight <- mean(height)
ind <- height > avgheight
sum(ind)
fem <- heights$sex == "Female"

both <- fem & ind 
sum(both)

mean(fem)

heights$sex[which.min(height)]

max(height)

x <- min(height):max(height)

partof <- (x %in% height) != 1
sum(partof)

heights2 = height*2.54
heights2[18]

mean(heights2)

sum(fem)

femcen <- heights2[fem]
mean(femcen)

library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region, data=olive)
