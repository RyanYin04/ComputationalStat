# Read the data
get_image = function(filename) {
  img = list()
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  n = readBin(f,'integer',n=1,size=4,endian='big')
  nrow = readBin(f,'integer',n=1,size=4,endian='big')
  ncol = readBin(f,'integer',n=1,size=4,endian='big')
  x = readBin(f,'integer',n=n*nrow*ncol,size=1,signed=F)
  img$x = matrix(x, ncol=nrow*ncol, byrow=T)
  close(f)
  img
}
get_label = function(filename) {
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  n = readBin(f,'integer',n=1,size=4,endian='big')
  y = readBin(f,'integer',n=n,size=1,signed=F)
  close(f)
  y
}
train = get_image('/Users/sylviaz/desktop/STA 243/hw3/train-images-idx3-ubyte')
test = get_image('/Users/sylviaz/desktop/STA 243/hw3/t10k-images-idx3-ubyte')
  
train$y = get_label('/Users/sylviaz/desktop/STA 243/hw3/train-labels-idx1-ubyte')
test$y = get_label('/Users/sylviaz/desktop/STA 243/hw3/t10k-labels-idx1-ubyte')  

image(matrix(train$x[4,], nrow=28)[,28:1])


# ---------- Q1 -----------
# Extract the image and labels out from the lists
tr_x = train$x
tr_y = train$y
tt_x = test$x
tt_y = test$y

# Get the compressed images for training set
tr_com_x = matrix(0, 60000, 196)
for (i in 1:14){
  for (j in 1:14){
    tr_com_x[,(14*(i-1)+j)] = 
      (tr_x[,(56*i+2*j-57)] + tr_x[,(56*i+2*j-56)] + tr_x[,(56*i+2*j-29)] + tr_x[,(56*i+2*j-28)])/4
  }
}

# Get the compressed images for testing set
tt_com_x = matrix(0, 10000, 196)
for (i in 1:14){
  for (j in 1:14){
    tt_com_x[,(14*(i-1)+j)] = 
      (tt_x[,(56*i+2*j-57)] + tt_x[,(56*i+2*j-56)] + tt_x[,(56*i+2*j-29)] + tt_x[,(56*i+2*j-28)])/4
  }
}

# Extract out the image for digits {0, 1, 2, 3, 4} for training set and testing set
# 1. Make the training set and testing set to be dataframe 
#    with 1st column to be label, and the rest to be image pixels
# 2. Subset the two dataframe by label == 0, 1, 2, 3, 4
tr = as.data.frame(cbind(tr_y, tr_com_x))
table(tr_y)
tr = tr[tr$tr_y == 0 | tr$tr_y == 1 | tr$tr_y == 2 | tr$tr_y == 3 | tr$tr_y == 4,]
te = as.data.frame(cbind(tt_y, tt_com_x))
table(tt_y)
te = te[te$tt_y == 0 | te$tt_y == 1 | te$tt_y == 2 | te$tt_y == 3 | te$tt_y == 4,]

x_train = as.matrix(tr[, 2:197])
y_train = as.matrix(tr[, 1])
x_test = as.matrix(te[, 2:197])
y_test = as.matrix(te[, 1])

# ---------- Q3 -----------
# ----- (i) -----
em_spherical = function(pi_0, mu_0, sigma_0, X, y, maxiter, esp = 0.0001){
  n = dim(X)[1]
  d = dim(X)[2]
  k = length(pi_0)
  # X is n*d
  # y is n*1
  # pi is k*1
  # mu is d*k
  # sigma is k*1
  # F is n*k
  pi = pi_0
  mu = mu_0
  sigma = sigma_0
  
  # Initialize log-likelihood, and update the log-likelihood per iteration to log_lik_new
  # After comparing log_lik_new and log_lik, update log_lik to be log_lik_new and run the next iteration
  log_lik = 1
  log_like = rep(0,maxiter)
  
  Fmatrix = matrix(0, n, k)
  for (t in 1:maxiter){
    # log-sum-exp trick applied to calculate F[i,j]
    log_weight_prob = matrix(0,n,k)
    
    for (j in 1:k){
      log_weight_prob[,j] = sapply(1:n, function(i) 
        log(pi[j]) - d/2 * log(2 * 3.141593) - d * log(sigma[j]) - 1 / (2 * sigma[j]^2) * t(X[i,] - mu[,j]) %*% (X[i,] - mu[,j]))
    }
    a = sapply(1:n, function(i) max(log_weight_prob[i,]))
    log_deno = sapply(1:n, function(i) a[i] + log(sum(exp(log_weight_prob[i,] - a[i]))))

    # Log-likelihood computing:
    log_lik_new = sum(log_deno)
    log_like[t] = log_lik_new
    
    # Compute F matrix
    log_F_t = sapply(1:n,function(i) sapply(1:k, function(j) log_weight_prob[i,j] - log_deno[i]))
    Fmatrix = t(exp(log_F_t))
    
    pi = sapply(1:k, function(j) sum(Fmatrix[,j])/n)
    mu = sapply(1:k, function(j) t(X) %*% Fmatrix[,j]/sum(Fmatrix[,j]))
    sigmasq = sapply(1:k, function(j) 
      sum(Fmatrix[,j] * apply(X,1,function(k) 
        sum((k - mu[,j])^2)))/(d * sum(Fmatrix[,j])))
    sigma = sqrt(sigmasq)
    
    # Stop if log
    if ( abs((log_lik_new - log_lik) / log_lik) < esp){
      break
    }
    log_lik = log_lik_new
  }
  
  return(list("pi" = pi, 
              "mu" = mu, 
              "sigma" = sigma, 
              "iterations" = t, 
              "log_likelyhood" = log_like))
}

pi_0 = rep(0.2,5)
mu_0 = matrix(rnorm(5*196, 100, 10), 196, 5)
sigma_0 = rep(100,5)


re = em_spherical(pi_0, mu_0, sigma_0, x_train, y_train, 20, esp = 0.0001)
re$log_likelyhood

pix = re$mu
par(mar = c(1,1,1,1))
par(mfrow = c(2,3))
for (i in 1:5){
  image(matrix(pix[,i],14)[,14:1])
  title(main = i)
}

pi_00 = c(0.1, 0.4, 0.4, 0.02, 0.08)
mu_00 = matrix(rnorm(5*196, 50, 5), 196, 5)
sigma_00 = rnorm(5, 50,10)

re2 = em_spherical(pi_00, mu_00, sigma_00, x_train, y_train, 20, esp = 0.0001)
re2$log_likelyhood

pix2 = re2$mu
par(mar = c(1,1,1,1))
par(mfrow = c(2,3))
for (i in 1:5){
  image(matrix(pix2[,i],14)[,14:1])
  title(main = i)
}

pi_000 = c(0.8, 0.05, 0.05, 0.05, 0.05)
mu_000 = matrix(rnorm(5*196), 196, 5)
sigma_000 = rnorm(5, 200, 10)

re3 = em_spherical(pi_000, mu_000, sigma_000, x_train, y_train, 20, esp = 0.0001)
re3$log_likelyhood

pix3 = re3$mu
par(mar = c(1,1,1,1))
par(mfrow = c(2,3))
for (i in 1:5){
  image(matrix(pix3[,i],14)[,14:1])
  title(main = i)
}

# Cluster the test data
test_spherical = function(pi, mu, sigma, x_test){
  n = dim(x_test)[1]
  d = dim(x_test)[2]
  k = length(pi)
  
  Fmatrix = matrix(0, n, k)
  log_weight_prob = matrix(0,n,k)
  for (j in 1:k){
    log_weight_prob[,j] = sapply(1:n, function(i) 
      log(pi[j]) - d/2 * log(2 * 3.141593) - d * log(sigma[j]) - 1 / (2 * sigma[j]^2) * t(x_test[i,] - mu[,j]) %*% (x_test[i,] - mu[,j]))
  }
  
  a = sapply(1:n, function(i) max(log_weight_prob[i,]))
  log_deno = sapply(1:n, function(i) a[i] + log(sum(exp(log_weight_prob[i,] - a[i]))))
  log_F_t = sapply(1:n,function(i) sapply(1:k, function(j) log_weight_prob[i,j] - log_deno[i]))
  Fmatrix = t(exp(log_F_t))
  
  cluster = sapply(1:n, function(i) which.max(Fmatrix[i,]))
  return(cluster)
}

cluster = test_spherical(re$pi, re$mu, re$sigma, x_test)
t1 = table(cluster != 1 & y_test == 4)
t2 = table(cluster != 2 & y_test == 2)
t3 = table(cluster != 3 & y_test == 3)
t4 = table(cluster != 4 & y_test == 0)
t5 = table(cluster != 5 & y_test == 1)
(t1[2] + t2[2] + t3[2] + t4[2] + t5[2])/length(y_test)

cluster3 = test_spherical(re3$pi, re3$mu, re3$sigma, x_test)
t1 = table(cluster3 != 1 & y_test == 4)
t2 = table(cluster3 != 2 & y_test == 2)
t3 = table(cluster3 != 3 & y_test == 0)
t4 = table(cluster3 != 4 & y_test == 1)
t5 = table(cluster3 != 5 & y_test == 3)
(t1[2] + t2[2] + t3[2] + t4[2] + t5[2])/length(y_test)
