mm <- read.csv("Markov1.csv")
mm <- as.matrix(mm)

rold <- matrix(c(1,0,0,0,0),nrows=5)

# can do while loop/for loop with an if statement to check if
# Sr = r, where we have reached the steady state, use a tolerance
# because the rounding might be off
tol = 1
while(tol>.000001)
{
  rold <- rnew
  rnew = mm %*%rold
  tol = max(abs(rnew-rold))
}
print(rnew)