#Author: Mitchell Krystofiak
#Class: MATH 490 - Hw2
#Date: September 10, 2021
#Description: Using basic R commands to define variables, vectors and strings,
#             and perform operations.

a <- 15L
b <- 2.5
c <- 108

print(a)
print(b)
print(c)

print(class(a))
print(class(b))
print(class(c))

d <- a+b
e <- a-c
f <- a*b
g <- c^a
h <- a/b

print(d)
print(e)
print(f)
print(g)
print(h)

print(class(d))
print(class(e))
print(class(f))
print(class(g))
print(class(h))

s1 = "Hello, my name is Mitchell."
s2 ="Here, I am not using quotations, but 'here, I am using quotations.'"

print(s1)
print(s2)

print(class(s1))
print(class(s2))


v1 = c(1, 2, 3)
v2 = c(4, 5, 6)
v3 = c(TRUE, FALSE, TRUE)
v4 = c("Mitchell", "Marina", "Toby")

print(v1)
print(v2)
print(v3)
print(v4)

print(class(v1))
print(class(v2))
print(class(v3))
print(class(v4))

print(v1+v2)
print(v1*v2)
print(v1-v2)
print(v1/v2)
print(v1^2)






