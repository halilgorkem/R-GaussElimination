A <- matrix(c(1,-1,2,-1,
              2,-2,3,-3,
              1,1,1,0,
              1,-1,4,3), nrow = 4, ncol = 4, byrow = T)
A
b <- c(-8,-20,-2,4)
b

#Satırları yer değiştirme
swaprows <- function(M, row1,row2)
{
  row_tmp <- M[row1,]
  M[row1,] <- M[row2,]
  M[row2,] <- row_tmp
  return(M)
}
swaprows(A, 1,2)
A

#Satırı bir skalerle çarpıp diğerine ekleme
replacerow <- function(M,row1, row2, k)
{
  M[row2,] <- M[row1,]*k - M[row2,]
  return(M)
}
replacerow(A,1,2,2)
A

gaussElimination <- function(A,b)
{
  n <- nrow(A)
  A <- cbind(A,b)
  x <- array(c(NA), dim = c(n,1))
  
  for (i in 1:(n-1))
  {
    # Step 2
    i_index <- which(A[,i] != 0)
    p <- min(i_index[i_index >= i])
    
    if(is.infinite(p) || is.null(p))
    {
      cat("Tek bir çözüm yoktur!!!\n")
      break
    }
    # Step 3
    if(p != i)
    {
      A <- swaprows(A, p, i)
    }
    # Step 4
    for(j in (i+1):n)
    {
      # Step 5
      m <- A[j,i] / A[i, i]
      # Step 6
      A <- replacerow(A, row1 = i, row2 = j, k = -m)
    }
  }
  
  # Step 7
  if(A[n,n] == 0)
  {
    cat("Tek bir çözüm yoktur!!!\n")
    break
  }
  
  # Step 8
  x[n] <- A[n, (n+1)] / A[n,n]
  
  # Step 9
  for (i in (n-1):1)
  {
    x[i] <- (A[i, (n+1)] - sum(A[i,(i+1):n] * x[(i+1):n])) / A[i,i]
  }
  cat("Algoritma başarılı...\n")
  
  list(A=A[,-(n+1)], x=x)
}
gaussElimination(A,b)

