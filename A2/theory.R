entropy = function(p1, p2) {
  
  if (p1 == 0 || p2 == 0) {
    0
  }
  else {
    n = p1 + p2 
    - (p1 / n) * log2(p1 / n) - (p2 / n) * log2(p2 / n)
  }
}


gain = function (p1, p2, values) {
  n = p1 + p2
  entropy_s = entropy(p1, p2)
  
  sum = 0
  for (value in values) {
    v1 = value[1]
    v2 = value[2]
    vn = v1 + v2
    pv = vn / n
    sum = sum + (vn / n) * entropy(v1, v2)
  }
  
  entropy_s - sum
}


x1_gain = gain(2, 2, list(c(1,1), c(1,1)))
x2_gain = gain(2, 2, list(c(1,1), c(1,1)))
x3_gain = gain(2, 2, list(c(0,1), c(2,1))) # BEST
x4_gain = gain(2, 2, list(c(0,0), c(2,2)))

x1_gain2 = gain(2, 1, list(c(1, 0), c(1, 1))) # BEST (choose this one)
x2_gain2 = gain(2, 1, list(c(1, 0), c(1, 1))) # BEST
x4_gain2 = gain(2, 1, list(c(0, 0), c(2, 1))) 

x2_gain3 = gain(1, 1, list(c(1, 0), c(1, 0)))
x4_gain3 = gain(1, 1, list(c(0, 0), c(1, 1)))
                

# Q2

a_gain = gain(10, 10, list(c(8, 2), c(2, 8))) # best
b_gain = gain(10, 10, list(c(10, 6), c(0, 4)))

# 2.2

c_gain = gain(100, 10, list(c(80, 2), c(20, 8)))
d_gain = gain(100, 10, list(c(100, 6), c(0, 4))) # best

