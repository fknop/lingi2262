data = data.frame(
  x = c(1, 2, 4, 5, 6),
  y = integer(5),
  label = c(1, 1, -1, -1, 1)
)

library(plotly)

p = plot_ly(data = data, x = ~x, y = ~y, type = 'scatter', name = 'Points') %>%
    add_trace(x = 3,  name = 'Linear Discriminant')
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