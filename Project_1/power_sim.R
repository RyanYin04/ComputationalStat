power_iteration = function(A, v0, eps = 1e-6, maxiter=100) {
    
    # Please implement the function power_iteration that takes in the matrix X
    # and initial vector v0 and returns the eigenvector.
    
   for (i in seq(1,maxiter)) {
       if (i == 1) {
           last_c = v0
           c = A %*% v0
       }
       
       else {
           last_c = c
           c = A %*% last_c
       }
       
       c = c / norm(c,'2')
       err = sum((c - last_c)^2)
       
       if(err < eps | i == maxiter){
           return(c)
           break
       }
       
   } 
    
}


set.seed(5)
E = matrix(rnorm(100), 10, 10)
v = c(1, rep(0, 9))
v
E
lams = 1:10
prods = c()
for (lambda in lams) {
  X = lambda*outer(v, v) + E
  print(X)
  v0 = rep(1, nrow(E))
  v0 = v0/sqrt(sum(v0^2))
  vv = power_iteration(X, v0)
  prods = c(prods, abs(v %*% vv))
}
plot(lams, prods, "b")




