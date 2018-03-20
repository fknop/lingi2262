load('RestrictedLetters.RData')

label = ncol(train)

train[-label] = scale(train[-label])
test[-label] = scale(test[-label])

perceptron_with_margin = function (train, lrate, b, max_t = 10000) {
  
  labels = as.numeric(factor(t(train[ncol(train)])))
  labels = sapply(labels, function(x) {
    if (x > 1) {
      -1
    }
    else {
      x
    }
  })
  
  k = 0
  t = 0
  n = nrow(train)
  x = train[-ncol(train)]
  
  w = integer(ncol(x))
  
  repeat {
    
    
    if (t > max_t) {
      print("max iterations")
      break
    }
    
    print(t)

    
    t = t + 1
    k = (k %% n) + 1
    xk = labels[[k]] * x[k, ] 
    
    wtxk = sum(w * xk)
    if (wtxk <= b) {
      w = w + (lrate(t) * xk)
    }
    
    if (sum(w * xk) > b) {
      break
    }
    
  }
  
  w
}

rate_fn = function(k) {
  1/k
}

w = perceptron_with_margin(train, rate_fn, 10)



classify_test_set = function(test_set, w) {
  
  labels = as.numeric(factor(t(test_set[ncol(test_set)])))
  labels = sapply(labels, function(x) {
    if (x > 1) {
      -1
    }
    else {
      x
    }
  })
  
  print(labels)
  
  
  results = sapply(1:nrow(test_set), function(i) {
    x = test_set[i, -ncol(test_set)] 
    ifelse(sum(x * w) < 0, -1, 1)
  })
  
  results
} 

classify_test_set(test, w)
classify_test_set(valid, w)


classifier = svm(formula = labels ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear',
                 cost = 1)

y_pred = predict(classifier, newdata = test)
cm = table(test[, ncol(test)], y_pred)


print(accuracy(cm))

