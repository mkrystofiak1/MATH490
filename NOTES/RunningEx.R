re <- read.csv('./RunningExample.csv')
print(re)

nteams <- ncol(re)/2

# want number of games won/ games lost

d.win <- re[,1] > re[,2]
m.win <- re[,3] > re[,4]
un.win <- re[,5] > re[,6]
uv.win <- re[,7] > re[,8]
print(d.win)
print(m.win)
print(un.win)
print(uv.win)


