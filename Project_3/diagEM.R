#==================Documentation===========================
# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

#=======================Load data=============================================================================================================
load_mnist <- function() {
    load_image_file <- function(filename) {
        ret = list()
        f = file(filename,'rb')
        readBin(f,'integer',n=1,size=4,endian='big')
        ret$n = readBin(f,'integer',n=1,size=4,endian='big')
        nrow = readBin(f,'integer',n=1,size=4,endian='big')
        ncol = readBin(f,'integer',n=1,size=4,endian='big')
        x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
        ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
        close(f)
        ret
    }
    load_label_file <- function(filename) {
        f = file(filename,'rb')
        readBin(f,'integer',n=1,size=4,endian='big')
        n = readBin(f,'integer',n=1,size=4,endian='big')
        y = readBin(f,'integer',n=n,size=1,signed=F)
        close(f)
        y
    }
    train = load_image_file('train-images-idx3-ubyte')
    test = load_image_file('t10k-images-idx3-ubyte')
    
    train$y = load_label_file('train-labels-idx1-ubyte')
    test$y = load_label_file('t10k-labels-idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
    image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}
#=======================Data process===============================================================
# Data processing:
# 2. Only keep 0,1,2,3,4

tr_idx = which(train$y<5)
tr_pics = train$x[tr_idx,]
tr_labels = train$y[tr_idx]

te_idx = which(test$y<5)
te_pics = test$x[te_idx,]
te_label = test$y[te_idx]

# 3. Compress the matrix:
my_compress = function(vec, pixs = 28, rate = 2){
    vec_compressed = c()
    count = 1
    n_block = pixs / rate
    for (j in seq(1,n_block)){
        for (i in seq(1,pixs,rate)){
            lc = (j-1)*56 + i
            vec_compressed[count] = mean(vec[c(lc,lc+1,lc+28,lc+29)])
            count = count + 1
        }
    }
    return(vec_compressed)
}

te_pics_ = apply(te_pics, MARGIN = 1, my_compress)
tr_pics_ = apply(tr_pics, MARGIN = 1, my_compress)
#=======================Q3, part b==================================

diagEM = function(x,mu, sigma, pp){
# x: d*n
# mu: d*k
# sigma: d*k
# F: n*k
    N = dim(x)[2] # Sample number
    d = dim(x)[1] # Pixel number
    k = length(pp)# Cluster number
    
    t1 = Sys.time()
    
    #Step 1: Calcultae F-matrix:
    f = matrix(0, nrow = N, ncol = k)
    log_numer = matrix(0, nrow = N, ncol = k)
    log_deno = matrix(0, nrow = N, ncol = 1)
    
    for (j in seq(1:k)){
        # Calculate inverse of sigma
        inv =  1 / sigma[,j] # This is an array! NOT a matrix!
        
    # Implement log-exp trick:
        log_numer[,j] = log(pp[j]) - d/2 * log(2 * pi)  - 0.5*sum(log(sigma[,j])) - 1/2* apply((inv*(x - mu[,j])^2), MARGIN = 2, sum)
    }
    
    #print(log_numer)
    A = apply(log_numer, MARGIN = 1, max)
    log_deno = sapply(seq(1,N),  function(i) A[i] + log(sum(exp(log_numer[i, ] - A[i]))))
    
    log_like = sum(log_deno)
    log_f = log_numer - log_deno
    
    f = exp(log_f)
    
    #Step 2: M-step:
    # Upate all the parameter:
    sum_fj = apply(f, MARGIN = 2, sum) # k* 1; 
    
    pp = sum_fj / N
    
    mu = sapply(seq(1,k), function(j) matrix(apply(t(t(x) * f[,j]), MARGIN = 1, sum ), ncol = 1)) / sum_fj
    
    sigma = 0.05*matrix(1, nrow = d, ncol = k) +
        sapply(seq(1,k),function(j) matrix(apply(2*t(f[,j]*t(x - mu[,j])^2), MARGIN = 1, sum), ncol = 1) ) / sum_fj
    
    t2 = Sys.time()
    
    ls = list('Fmat' = f,
              'PI' = pp, 
              'MU' = mu,
              'Sigma' = sigma, 
              'RunningTime' = t2-t1,
              'Likelihood' = log_like
              )
    return(ls)
}
#======================Implement EM================================
myEM = function(x,mu, sigma, pp,maxiter, err){
    lh = -10*exp(-20)
    cr = 100
    N = dim(x)[2]
    for (i in 1:maxiter){
        print('========================================')
        print(c(i, 'iteration'))
        
        ls = diagEM(x, mu, sigma, pp)
        
        mu = ls$MU
        sigma = ls$Sigma
        pp = ls$PI
        
        lh_temp = ls$Likelihood
        cr = abs((lh_temp - lh)/lh)
        lh = lh_temp
        
        fmat = ls$Fmat
        print(c(lh_temp, cr))
        
        if (cr<err){
            print('propotional error reach the precision requirement')
            cluster = sapply(1:N, function(i) which.max(fmat[i,]))
            return(list(
                'MU' = mu,
                "PI" = pp,
                'Sigma' = sigma,
                'cluster' = cluster,
                'log-Likelihood' = lh,
                'number of iter' = i,
                'precision' = err
            ))
        } 
        
    }
    print('reach the max iteration number')
}
# Match the clusters with the digits
assign_digit = function(y_cluster, y){
    digit = rep(0,5)
    for (i in 1:5){
        r = sapply(0:4, function(j) table(y_cluster == i & y == j)[2])
        digit[i] = which.max(r)-1
        print(r)
    }
    return(digit)
}
#=================Tr data====================================

sample = dim(tr_pics_)[2]
pix = dim(tr_pics_)[1]
sigma =matrix(runif(pix*k, 0.8, 1.5), nrow = pix,ncol = k)
k = 5
mu = matrix(rnorm(pix*k, 60, 5), ncol = k)

pp = c(0.30, 0.20, 0.1, 0.20, 0.20)

ls = myEM(tr_pics_, mu,sigma , pp, maxiter = 100, err = 0.006)

assign_digit(ls$cluster, tr_labels)

#===============Te data========================
errRate = function(cluster, digit,label){
    count = 0
    N = length(label)
    for (i in 1:5){
        idx = which(cluster == i)
        count = count + table(label[idx] == digit[i])[1]
    }
    return(count / N )
}


sample = dim(te_pics_)[2]
pix = dim(te_pics_)[1]
sigma =matrix(runif(pix*k, 0.8, 1.5), nrow = pix,ncol = k)
k = 5
mu = matrix(rnorm(pix*k, 60, 5), ncol = k)

pp = c(0.30, 0.20, 0.1, 0.20, 0.20)
ls1 = myEM(te_pics_, mu,sigma , pp, maxiter = 100, err = 0.001)
dg1=assign_digit(ls1$cluster, te_label)

err1 = errRate(ls1$cluster, dg1, te_label)
err1


#=====================TE 2=================
sample = dim(te_pics_)[2]
pix = dim(te_pics_)[1]
sigma =matrix(runif(pix*k, 0.8, 1.5), nrow = pix,ncol = k)
k = 5
mu = matrix(rnorm(pix*k, 80, 2), ncol = k)

pp = c(0.2, 0.25, 0.25, 0.15, 0.15)

ls2 = myEM(te_pics_, mu,sigma , pp, maxiter = 100, err = 0.005)
dg2 = assign_digit(ls2$cluster, te_label)

errRate(ls2$cluster, dg2, te_label)

