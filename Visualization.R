library(actuar)
library(plotly)

# Problem settings ----------------------------------------------------------------

# Parameters of the PH distribution

alpha <- c(0.2, 0.2, 0.2, 0.2)
matrix_T <- matrix(c(-0.2, 0.05, 0.05, 0.05,
              0.05, -0.2, 0.05, 0.05,
              0.05, 0.05, -0.2, 0.05,
              0.05, 0.05, 0.05, -0.2),
            nrow = 4,
            ncol = 4,
            byrow = T)
T_zero <- (- matrix_T %*% c(1, 1, 1, 1))

# Generate the dataset of PH distribution
size <- 100
data <- rphtype(size, alpha, matrix_T)
# data <- rexp(size)

# Randomly pick two orthogonal directions 
#   which don't violate the equality constraints.

alpha_dx <- c(1, -1, 0.5, 0.5)
alpha_dy <- c(-3/2, -1/4, 1, 1)
  
matrix_T_dx <- matrix(c(-1, 1, 0.5, 0.5,
              1, -1, 0.5, 0.5,
              0.5, 1, -1, 0.5,
              0.5, 1, 0.5, -1),
            nrow = 4,
            ncol = 4,
            byrow = T)
matrix_T_dy <- matrix(c(-1, -3/2, 1, 1,
                  -3/2, -1, 1, 1,
                  1, -3/2, -1, 1,
                  1, -3/2, 1, -1),
                nrow = 4,
                ncol = 4,
                byrow = T)

T_zero_dx <- c(-1, -1, -1, -1)
T_zero_dy <- c(1/2, 1/2, 1/2, 1/2)

# Normalize direction vectors 
alpha_dx <- alpha_dx / (sqrt(sum(alpha_dx^2)))
alpha_dy <- alpha_dy / (sqrt(sum(alpha_dy^2)))

tmp_M <- cbind(matrix_T_dx, T_zero_dx)
for (i in 1:nrow(tmp_M)){
  tmp_M[i,] <- tmp_M[i,] / (sqrt(sum(tmp_M[i,]^2)))
}
matrix_T_dx <- tmp_M[,1:4]
T_zero_dx <- tmp_M[,5]


tmp_M <- cbind(matrix_T_dy, T_zero_dy)
for (i in 1:nrow(tmp_M)){
  tmp_M[i,] <- tmp_M[i,] / (sqrt(sum(tmp_M[i,]^2)))
}
matrix_T_dy <- tmp_M[,1:4]
T_zero_dy <- tmp_M[,5]



# Function definitions ----------------------------------------------------

PH_likelihood <- function(alpha, matrix_T, data) {
  density <- dphtype(data, alpha, matrix_T)
  log_density <- log(density)
  log_likelihood <- sum(log_density)
  
  return(log_likelihood)
  }

compute_grid <- function(x_range, y_range, stepX, stepY, outside) {
  x_list <- seq(x_range[1], x_range[2], stepX)
  y_list <- seq(y_range[1], y_range[2], stepY)
  
  num_grid_x <- (x_range[2]- x_range[1]) / stepX + 1
  num_grid_y <- (y_range[2]- y_range[1]) / stepY + 1
  
  LL_value <- matrix(rep(0, len = num_grid_x * num_grid_y), 
                     nrow = num_grid_x, ncol = num_grid_y)
  
  for (i in 1:num_grid_x){
    for (j in 1:num_grid_y){
      x_tmp <- x_list[i]
      y_tmp <- y_list[j]
      
      alpha_tmp <- alpha + x_tmp * alpha_dx + y_tmp * alpha_dy
      matrix_T_tmp <- matrix_T + x_tmp * matrix_T_dx + y_tmp * matrix_T_dy
      T_zero_tmp <- T_zero + x_tmp * T_zero_dx + y_tmp * T_zero_dy
      
      # if (i == 17 && j == 1){
      #   print(matrix_T_tmp)
      #   print(alpha_tmp)
      #   print(T_zero_tmp)
      #   print(sum(matrix_T_tmp[3,]) + T_zero_tmp[3])
      #   print("=============")
      # }
      
      ## Check the non-negative constraints
      check_alpha <- all(alpha_tmp > 0)
      
      tmp_M <- matrix_T_tmp
      tmp_M <- tmp_M - 2*diag(diag(tmp_M))

      check_matrix_T <- all(tmp_M > 0)
      check_T_zero <- all(T_zero_tmp > 0)
      
      # if(i == 17 & j == 1){
      #   print(check_T_zero)
      # }
      
      ## Compute the log-likelihood values if applicable
      ## Otherwise fill in with the pre-defined value
      if (check_alpha & check_matrix_T & check_T_zero){
        LL_value[i, j] <- -PH_likelihood(alpha_tmp, matrix_T_tmp, data)
      }
      else{
        LL_value[i, j] <- outside
      }
    }
  }
  
  return(list(x_list, y_list, LL_value))
}


# Drawing -----------------------------------------------------------------

# Drawing settings
stepX <- 0.001
stepY <- 0.001
x_range <- c(-0.2, 0.3)
y_range <- c(-0.2, 0.3)
outside <- 0

# Draw
result <- compute_grid(x_range, y_range, stepX, stepY, outside)
x_list <- result[[1]]
y_list <- result[[2]]
df <- result[[3]]
df[df == 0] <- max(df)
 
axx <- list(title = "Direction 1", range = c(-0.2, 0.3))
axy <- list(title = "Direction 2", range = c(-0.2, 0.3))
axz <- list(title = "Log-likelihood")

fig1 <- plot_ly(x = ~x_list, y = ~y_list, z = ~df) %>% 
  add_surface() %>%
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

fig2 <- plot_ly(type = "contour", x = ~x_list, y = ~y_list, z = ~df,
                contours = list(end = max(df), size = 10, start = 400)) %>% 
  layout(xaxis=axx,yaxis=axy) %>%
  colorbar(title = "Log-likelihood")

fig1
fig2







