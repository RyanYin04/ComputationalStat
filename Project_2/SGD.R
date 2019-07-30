library(MASS)

# Load all the data:
fh = read.csv('fancyhouse.csv')
Price = read.csv('housingprice.csv')
te = read.csv('test.data.csv')
tr = read.csv('train.data.csv')

# Define a function to standarize the data.

standarize = function(x){
    mu = mean(x)
    sigma = sd(x)
    x_std = (x-mu)/sigma
    return(x_std)
}

# Standarize the data, and extract the columns we are gonna use:

tr = as.data.frame(apply(tr, MARGIN = 2, as.numeric))
tr_std = tr[,c('price','bedrooms','bathrooms','sqft_living', 'sqft_lot')]
tr_std = as.data.frame(apply(tr_std, MARGIN = 2, standarize))

te = as.data.frame(apply(te, MARGIN = 2, as.numeric))
te_std = te[,c('price','bedrooms','bathrooms','sqft_living', 'sqft_lot')]
te_std = as.data.frame(apply(te_std, MARGIN = 2, standarize))

#==============================================================================
## (a)

# Fit the model on tranning data:

lm_tr =lm(price ~ bedrooms + bathrooms +sqft_living + sqft_lot, data = tr_std)
summary(lm_tr)
# R-square is 0.5101, and R_a = 0.51

# Fit the model on test data:
lm_te =lm(price ~ bedrooms + bathrooms +sqft_living + sqft_lot, data = te_std)
summary(lm_te)
# R-square is 0.5054, and R_a = 0.5051

#===============================================================================
##(b):
# Using the lm_model above to predict the price:
SD = sapply(te[,c('bedrooms','bathrooms','sqft_living', 'sqft_lot')],sd)
MU = sapply(te[,c('bedrooms','bathrooms','sqft_living', 'sqft_lot')],mean)
fh_std = (fh[,c('bedrooms','bathrooms','sqft_living', 'sqft_lot')]-MU)/SD
fh$sqft_l

pred_std = predict(lm_tr,fh_std)
pred_std
# 40.60501 before tranffered back

# Transffered back the data.
y_sd = sd(tr$price)
y_mu = mean(tr$price)
pred_std*y_sd + y_mu

# The predicted price is 15458417, which is not so reasonable.

#=================================================================================
##(c):
# Add variable:

lm_extended =lm(price ~ bedrooms* bathrooms +sqft_living + sqft_lot, data = tr_std)
summary(lm_extended)

#=================================================================================
##(d)
# My gradient decent function:
my_GD = function(beta0,x,y, max_iters,eta = 0.005){
    
    N = length(y)
    
    for(i in seq(1, max_iters)){
        
        grd = (-2* t(x) %*% y + 2* t(x) %*% x %*% beta0 )/ N
        temp = beta0 - eta * grd 
        
        # set criteria:
        beta_change = temp - beta0
        l2_norm = norm(beta_change, type = '2')
        beta0 = temp
        
        if (l2_norm < 5*10^(-8) & i>1){
            print('Reach the precision requirement.')
            print(i)
            print(l2_norm)
            print('=========================\n')
            break
        }
    }
    list = list('gradient' = grd, 'beta' = beta0, 'iters' = i)
    return (list)
    
}

### 1. Get the coefficients by GD w/o the extra term:

x = as.matrix(tr_std[,c('bedrooms','bathrooms','sqft_living', 'sqft_lot')])
x = cbind(matrix(0,15129,1),x)
y = as.matrix(tr_std$price)
beta0 = matrix(0,5,1)
#===============================================================================
    # Tuning on eta:
    eta = seq(0.005, 0.5, 0.0005)
    iters = c()
    
    for(k in eta){
        print(k)
        ls = my_GD(beta0, x, y, max_iters = 5000, eta = k)
        iters = cbind(iters, ls[3])
    }
    
    plot(eta, iters)
iters[771]
#============================================================================
# Choose eta = And get the final model.
ls = my_GD(beta0, x, y, max_iters = 5000, eta = 0.39)
ls
#           bedrooms   bathrooms sqft_living    sqft_lot
#[1,]  0    -0.1514695 0.007739715   0.7918288 -0.04514307
lm_tr$coefficients
#(Intercept)      bedrooms     bathrooms   sqft_living      sqft_lot 
#-2.203569e-16 -1.514702e-01  7.736288e-03  7.918328e-01 -4.514359e-02 

# The outcomes are alomost the same.


### 2. Get the coefficients by GD w/ the extra term:

x = as.matrix(tr_std[,c('bedrooms','bathrooms','sqft_living', 'sqft_lot')])
x = cbind(matrix(1,15129,1),x, x[, 'bedrooms'] * x[, 'bathrooms'])
y = as.matrix(tr_std$price)
beta0 = matrix(0,6,1)

ls_extended = my_GD(beta0,x,y, max_iters = 7000, eta = 0.039)

print(ls_extended)
#                   bedrooms   bathrooms sqft_living    sqft_lot           
# [1,] -0.03381309 -0.1470674 0.005501988   0.7718833 -0.04490953 0.06657567

lm_extended$coefficients
#(Intercept)           bedrooms          bathrooms        sqft_living 
#-0.033811650       -0.147075445        0.005465132        0.771927593 
#sqft_lot bedrooms:bathrooms 
#-0.044915165        0.066573483 

# The output converges to the output in 678 iterations by using build-in lm function.

#================================================================================
## (e):
my_SGD = function(beta0,x, y, batchsize, c = 0.1){
    p = dim(x)[2]
    N = length(y)
    iter_round = round(N/batchsize)
    
    for (rd in seq(0,5)){
        print('==================================================================')
        set = seq(1,N)
        remain = N
        for (k in seq(1 + rd*N, rd*N + iter_round)) {
            idx = sample(seq(1,remain), size = batchsize)
            pick = set[idx]
            set = set[-idx]
            remain = remain - batchsize
            
            x_ = matrix(x[pick,], batchsize, p )
            y_ = y[pick]
            
            ls = my_GD(beta0, x_, y_, max_iters = 1,eta = c/(1+k) )
            
            change = norm(ls$beta - beta0, type = '2')
            
            beta0 = ls$beta
            grd = ls$gradient
            
            if (change< 10^(-6) & norm(grd,type = '2')<3*10^(-5)){
                break
            }
        }
    if (change< 10^(-6) | norm(grd,type = '2')<3*10^(-5)){
        print('=======================================')
        print('Reach to the precision requirements')
        break
    }
    } 
    ls = list('beta'= beta0, 'gradient'=grd, 'iters' = k)
    
    return(ls)
}

#==============================================================================

# Tune on c:
iters = c()
C = seq(0.8,1.2,0.01)
for (i in C){
    x = as.matrix(tr_std[,c('bedrooms','bathrooms','sqft_living', 'sqft_lot')])
    x = cbind(matrix(0,15129,1),x)
    y = as.matrix(tr_std$price)
    beta0 = matrix(0,5,1)
    ls = my_SGD(beta0, x, y, batchsize = 1, c = i)
    iters = c(iters, ls$iters)
}

plot(C, iters, type = 'b')

# model w/o inteaction:

x = as.matrix(tr_std[,c('bedrooms','bathrooms','sqft_living', 'sqft_lot')])
x = cbind(matrix(0,15129,1),x)
y = as.matrix(tr_std$price)
beta0 = matrix(0,5,1)
ls_1 = my_SGD(beta0, x, y, batchsize = 1, c =1.1 )
ls_1

# model w/interaction:


x = as.matrix(tr_std[,c('bedrooms','bathrooms','sqft_living', 'sqft_lot')])
x = cbind(matrix(1,15129,1),x, x[, 'bedrooms'] * x[, 'bathrooms'])
y = as.matrix(tr_std$price)
beta0 = matrix(0,6,1)
ls_2 = my_SGD(beta0, x, y, batchsize = 1, c = 1.1)

ls_2
