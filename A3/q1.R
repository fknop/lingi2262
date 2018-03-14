x1_plus = c(0, -2)
x2_plus = c(0, 0)

x1_minus = c(2, 4, 0)
x2_minus = c(2, 2, 4)



plot(c(), c(), 
     ylim=c(-2,4), 
     xlim=c(-4,4), 
     pch = 15,
     xlab = "x1",
     ylab = "x2",
     main = "Q1"
     )
points(x1_plus, x2_plus, col = "red", pch = 15)
points(x1_minus, x2_minus, col = "blue", pch = 15)



abline(a = 2, b = -1, col = "green")