library('MASS')

install.packages('quadprog')
library('quadprog')
install.packages('phangorn')
library('phangorn')

# Part a: Realize the algorithm.

skt_ols = function(x, y, eps = 0.3){
    # dim(x) = n*p
    # dim(y) = n*1
    
    n = dim(x)[1]
    p = dim(x)[2]
    
    
    r = round(p*log(n)/eps)
    print(r)

    # get stretch matrix Phi:
    
    # - Random matrix D: n*n
    D = sample(c(-1,1), size = n, replace = T, prob = c(0.5,0.5))
    
    Dx = matrix(0,n,p)
    Dy = matrix(0,n,1)
    
    for(i in seq(1, n)){
        Dx[i,] = D[i] * x[i,]
        Dy[i,] = D[i] * y[i,]
    }
    
    # - Hadamard matrix: n*n
    HDx = fhm(Dx)
    HDy = fhm(Dy)
    
    
    # - Matrix S: n*r
    S = matrix(0, n, r)
    sample2 = sample(seq(1, n), size = r, replace = T, prob = ones(1, n) / n)
    
    #for(i in seq(1,r)){
     #   S[sample2[i],i] = 1* sqrt(n / r)
      #  SHDX = 
    #}
    
    
    
    #HDx = H %*% Dx
    #HDy = H %*% Dy
    
    
    
    #phi_x = H %*% phi_x
    #phi_x = t(S) %*% phi_x
    
    #A_p = solve(t(phi_x) %*% phi_x)     #p*p
    
    #phi_y =D %*% y      #r*1
    #phi_y = H %*% phi_y
    #phi_y = t(S) %*% phi_y
    
    #beta = A_p %*% t(phi_x) %*% phi_y  
    
    return(HDx)
}

tt = skt_ols(x = matrix(rnorm(320), 64, 5), y = matrix(rnorm(64), 64, 1))

tt

# Part b:

# Initialize the data:
quant = 1048576/2
temp_x = matrix(runif(quant * 20, min = 0, max = 1), quant, 20)
temp_y = matrix(runif(quant * 1, min = 0, max = 1), quant, 1)

tt2 = skt_ols(temp_x, temp_y)
tt2

# Part c:

# Calculate running time:
t1 = Sys.time()
t2 = Sys.time()

t2-t1
temp_x = as.big.matrix(temp_x)
temp_y = as.big.matrix(temp_y)
