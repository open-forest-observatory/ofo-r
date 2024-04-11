library(lidR)

X = runif(1000, 684766, 684993)
Y = runif(1000, 5017773, 5018007)
Z = runif(1000, 0, 30)
data = data.frame(X,Y,Z)

l = LAS(data, l@header)
