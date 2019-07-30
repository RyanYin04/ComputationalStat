mat_randomMulti = function(A, B, r){
    # r is the number of samples.
    # r should less or equal to the column number of A
    n = dim(A)[2]
    
    norms_A = c()
    norms_B = c()
    
    # Calculate the probabilty:
    for (i in seq(1, n)){
        
        norm_a = norm(A[,i], '2')
        norm_b = norm(B[i,], '2')
        norms_A = c(norms_A, norm_a)
        norms_B = c(norms_B, norm_b)
        norm_prod = norms_A * norms_B
    }
    
    p = norm_prod / sum(norm_prod)
    
    # Select index:
    sample = sample(seq(1, n), size = r, replace = T, prob = p)
    
    # Calculate product:
    M = matrix(0, dim(A)[1], dim(B)[2])
    
    for (j in sample){
        prod = A[,j] %*% t(B[j,]) / (r*p[j])
        M = M + prod
        # print(j)
    }
    
    return(M)
    }


A = matrix(c(1,2,3,4,5,5,8,10,12,1, 10, 25, 29, 31, 35), 3,5)
B = matrix(c(4,5,6,7,8,8,10,12, 14, 17, 27,29, 31,37,41), 5,3)
dim(A)[2]

# Test the fuction:
mat_randomMulti(A,B,3)
A%*%B

# Part b:

matA = as.matrix(read.csv('STA243_homework_1_matrix_A.csv', header = F))
matB = as.matrix(read.csv('STA243_homework_1_matrix_B.csv', header = F)) 

mat_real = matA %*% matB

mat_est_20 = mat_randomMulti(matA, matB, 20)
mat_est_50 = mat_randomMulti(matA, matB, 50)
mat_est_100 = mat_randomMulti(matA, matB, 100)
mat_est_200 = mat_randomMulti(matA, matB, 200)

# Part c:
err_20 = norm(-mat_real + mat_est_20, 'F')/ (norm(matA, 'F') * norm(matB, 'F'))
err_50 = norm(-mat_real + mat_est_50, 'F')/ (norm(matA, 'F') * norm(matB, 'F'))
err_100 = norm(-mat_real + mat_est_100, 'F')/ (norm(matA, 'F') * norm(matB, 'F'))
err_200 = norm(-mat_real + mat_est_200, 'F')/ (norm(matA, 'F') * norm(matB, 'F'))

err = c(err_20, err_50, err_100, err_200)
err


# Part d:
par(mfrow = c(2,2))
image(matA %*% matB)
image(mat_est_20)
image(mat_est_50)
image(mat_est_100)
image(mat_est_200)
