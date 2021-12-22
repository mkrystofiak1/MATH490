#Author: Mitchell Krystofiak
#Class: Prediction Methods
#Date: September 1, 2021
#Description: This is a beginner program demonstrating
#             the basics of R language.

print("Variable assignment")
x <- 5
cat("x = ", x, '\n')

y <- 14
cat("y = ", y, '\n')

z <- 143
cat("z = ", z, '\n\n')

print("Basic operations")
cat("x + y = ", x+y, '\n')
cat("x - z = ", x-z, '\n')
cat("x*y - z = ", x*y -z, '\n')
cat("x/y - z = ", x/y -z, '\n')
cat("x^2 + z = ", x^2 + z, '\n\n')

print("Intrinsic functions")
cat("sin(0) = ", sin(0), '\n')
cat("cos(0) = ", cos(0), '\n')
cat("tan(pi/4) = ", tan(pi/4), '\n')
cat("ln(e) = ", log(exp(1)), '\n')
cat("sqrt(144) = ", sqrt(144), '\n\n')

print("Plotting Functions")
a <- seq(-pi, pi, 0.1)
plot(a, sin(a), 
     main="The Sine and Cosine Functions",
     type="l",
     col="blue")
lines(a, cos(a), col="red")
legend("topleft",
       c("sin(a)","cos(a)"),
       fill=c("blue","red"))

vector <- c(1,2,3,pi)



