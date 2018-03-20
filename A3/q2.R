library(plotly)

data = matrix(
  c(
    0, 2, 3,
    0, 2, 3,
    1, 0, 1
  ),
  nrow = 3,
  dimnames = list(c(), c("x1", "x2", "label"))
)

frame = data.frame(data = data)

p = plot_ly(data = frame, x = ~data.x1, y = ~data.x2, color= ~data.label, colors = "Dark2")
p

mapped = data.frame(
  x = c(0, 8, 27),
  y = c(0, 8, 27),
  z = c(0, 4.48, 10.10),
  label = c(1, 0, 1)
)

colnames(mapped) = c("x", "y", "z", "label")

p2 = plot_ly(data = mapped, x = ~x, y = ~y, z = ~z, color = ~label)
p2