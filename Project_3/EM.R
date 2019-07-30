# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

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
#==============
install.packages('readmnist')
library(readmnist)
library('MASS')
#=================================================================
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

dim(tr_pics)




#=================================================================================
# Realize algorithms:
# Initialize the data:

pp[1] * (2*pi^(pixs/2) * sigma[1]^pixs)^(-1)*matrix(diag(exp((-2*sigma[1]^2)^(-1) *t(tt - mu[,1])%*%(tt-mu[,1]))), nrow = sample)

fmat = function(data, Mean, Var, Pi, pixs = 196, k = 5){
    t1= Sys.time()
    N = dim(data)[2]
    print(N)
    Fmat = matrix(1,N, k)
    for(j in seq(1, k)){
        inner = exp((-2*sigma[1]^2)^(-1) *t(tt - mu[,j])%*%(tt-mu[,j]))
        e = matrix(diag(inner), nrow = sample)
        temp = pp[j] * (2*pi^(pixs/2) * sigma[j]^pixs)^(-1)* e
        Fmat[,j] = temp
        
    }
    t2 = Sys.time()
    print(t2-t1)
    return(Fmat)
}
sample = 30000
tt = matrix(tr_pics_[,1:sample], nrow = 196)
pixs = 196
k = 5
mu = matrix(rnorm(pixs*k), nrow = pixs, ncol = k)
pp = matrix(runif(k), nrow = k, ncol = 1)
sigma = matrix(rnorm(k), nrow = k)
test = fmat(data = tt, Mean = mu, Var = sigma, Pi = pp )

    # E-step:


estep = function(p, mu,sigma){
    # Conditional probability: P(Z=j|x)
    
    f = 1
    denom = 

    p = 
    mu = 
    sigma =     
    
}

# M-step:

#======================================

image(matrix(tr_pics[3,], 28,28))

show_digit(tr_pics[1,])





