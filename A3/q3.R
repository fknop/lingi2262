data = data.frame(
  x = c(1, 2, 3.405, 4, 5, 6),
  y = integer(6),
  label = c(1, 1, 0, -1, -1, 1),
  labels = c('+', '+', 'Linear Discriminant', '-', '-', '+')
)

library(plotly)

p = plot_ly(data = data, x = ~x, y = ~y, type = 'scatter', color = ~labels, colors = 'Set1')
p

map_point = function(x) {
  c(x ^ 2, sqrt(2) * x, 1)
}

result = sapply(c(1, 2, 4, 5, 6), map_point)

x = result[1,]
y = result[2,]
z = result[3,]

plot_3d = data.frame(
  x = x, y = y, z = z, labels = c(1, 1, -1, -1, 1)
)

p2 = plot_ly(plot_3d, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers', color = ~labels) 
p2


map_point = function(x) {
  c(x ^ 2, sqrt(2) * x, 1)
}

x = c(1, 2, 4, 5, 6)
y = c(1, 1, -1, -1, 1)
a = c(0, 2.5, 0, 7.333, 4.833)

w = sum(x * y * a)

k = function(xi, xj) {
  sum(map_point(xi) * map_point(xj))
}




u = sapply(1:5, function (index) {
  ui = sum(sapply(1:5, function (j) {
    a[[j]] * y[[j]] * k(x[[index]], x[[j]]) 
  }))
  
  ui
})

y - u # u 2, 4, 5 ==



