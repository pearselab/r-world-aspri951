#R world session one: terrain

#First, want to make a matrix of odd dimensions.

create.matrix <- function(dim){
  if(dimension %% 2 == 0){
    stop("Dimensions of matrix must be odd!")
  }
  new.matrix <- matrix(nrow = dim, ncol = dim)
}

#Done. Makes a matrix of n dimensions where n is odd, and fills matrix with NAs.
#Next, pick starting height for four corners.
#Remember, can call parts ofmatrix: matrix[row, col]

top.left.corner <- new.matrix[1, 1]
top.right.corner <- new.matrix[1, dim]
bottom.left.corner <- new.matrix[dim, 1]
bottom.right.corner <- new.matrix[dim, dim]
#But setting top.left.corner equal to anything else now will just re-write the vector called top.left.corner
#NOT replace a value in new.matrix

c(top.left.corner, top.right.corner, bottom.left.corner, bottom.right.corner) <- replicate(rnorm(1, 10, 20))
#doesn't work

#perhaps a way to use expand.grid for this?
expand.grid(c(1, "dim"), c(1, "dim"))
#looks promising
#Test:

vars <- expand.grid(c(1, "dim", c(1, "dim")))
#Gives a data frame like so:
#  Var1 Var2
#1    1    1
#2  dim    1
#3    1  dim
#4  dim  dim

#Can call parts of this dataframe using vars$(NAME OF COLUMN)[[NAME OF ROW]]. For example:
vars$Var1[[4]]
#returns "dim"
#So....

corners <- expand.grid(c(1, "dim", c(1, "dim")))

for(i in 1:4){
  new.matrix[corners$Var1[[i]], corners$Var2[[i]]] <- rnorm(1, 10, 15)
}

#Test combined:

create.matrix <- function(dim){
  if(dim %% 2 == 0){
    stop("Dimensions of matrix must be odd!")
  }
  new.matrix <- matrix(nrow = dim, ncol = dim)
  corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  print(new.matrix)
}
#Score. Example:

create.matrix(5)
#[1,] 23.263056   NA   NA   NA   1.404614
#[2,]        NA   NA   NA   NA         NA
#[3,]        NA   NA   NA   NA         NA
#[4,]        NA   NA   NA   NA         NA
#[5,] -3.525507   NA   NA   NA -17.399197

#Next: do diamond step. 
#Will need to find center of matrix. 
#Instead of using division, why not use median(1, n)? This would prevent rounding problems, i.e. 5/2 = 2.5, ceiling(2.5) = 3

diamond.step <- function(new.matrix, corners.index, dim){
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  new.matrix[median(1:dim), median(1:dim)] <- mean(corners.vector)
  return(new.matrix[median(1:dim), median(1:dim)])
}

#Test: combine:

create.matrix <- function(dim){
  if(dim %% 2 == 0){
    stop("Dimensions of matrix must be odd!")
  }
  new.matrix <- matrix(nrow = dim, ncol = dim)
  corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  diamond.step(new.matrix, corners.index, dim)
  print(new.matrix)
}

#Everything works, except the mean value for center dies inside the function. Maybe set something equal to diamond.step?


diamond.step <- function(new.matrix, corners.index, dim){
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  return(mean(corners.vector))
}

#Test: combine:

create.matrix <- function(dim){
  if(dim %% 2 == 0){
    stop("Dimensions of matrix must be odd!")
  }
  new.matrix <- matrix(nrow = dim, ncol = dim)
  corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  new.matrix[median(1:dim), median(1:dim)] <- diamond.step(new.matrix, corners.index, dim)
  return(new.matrix)
}

#Done. NOTE: must RETURN a value from a function or the function is USELESSESSSSSS 
#Next, square step. First, locate the center side positions in the matrix:

sides.index <- expand.grid(c(1, median(dim), c(1, median(dim))))

#Put function around it:

square.step <- function(new.matrix, corners.index, dim)
sides.index <- expand.grid(c(1, median(dim), c(1, median(dim))))
for(i in 1:4){
  new.matrix[sides.index$Var1[[i]], sides.index$Var2[[i]]] <- rnorm(1, 10, 15)
}


#Test for the bigger algorithm:
new.matrix <- matrix(nrow = 7, ncol = 7)
sub.matrix <- matrix(c(1:25), nrow = 5, ncol = 5)
new.matrix[2:6, 2:6] <- sub.matrix
new.matrix
#[,1] [,2] [,3] [,4] [,5] [,6] [,7]
#[1,]   NA   NA   NA   NA   NA   NA   NA
#[2,]   NA    1    6   11   16   21   NA
#[3,]   NA    2    7   12   17   22   NA
#[4,]   NA    3    8   13   18   23   NA
#[5,]   NA    4    9   14   19   24   NA
#[6,]   NA    5   10   15   20   25   NA
#[7,]   NA   NA   NA   NA   NA   NA   NA

#Sweet: can replace whole chunks of a matrix

#Test number two for bigger algorithm:

index.generator <- function(dim){
  index.list <- list(0)
  index.list[1] <- c(1, dim)
  index.list[1 + 1] <- c(1, median(1:dim), dim)
  index.list[(1 + 1) + 1] <- c(1, median(1:median(dim)), median(1:dim), etc. )
  for(i in 2:(dim - 1)){
    index.list[i] <- c(index.list)
  }
}

#Another test:

test <- c(1, 3, 5, 7)
vector.test <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
vector.test[test]
#[1] "a" "c" "e" "g"


index.generator <- function(dim){
  index.one <- sequence(1:dim)
  odd.values <- seq(1, dim, by = 2)
  print(odd.values)
}
#Works, now apply until length of output = 2

index.generator <- function(dim){
  index.one <- seq(1, dim)
  odd.values <- seq(1, dim, by = 2)
  index.list <- list(index.one)
  for(i in 1:dim){
    if(length(unlist(index.list[i])) > 2){
      new.index <- unlist(index.list[i])
      index.list[i + 1] <- new.index[odd.values[1:((1/2)*(length(new.index)))]]
      print(index.list[i + 1])
    }
  }
}
#No

index.generator <- function(dim){
  index.one <- seq(1, dim)
  odd.values <- seq(1, dim, by = 2)
  index.two <- index.one[odd.values]
  index.three <- index.two[odd.values]
  print(index.three)
}

index.generator <- function(dim){
  index.one <- seq(1, dim)
  odd.values <- seq(1, dim, by = 2)
  index.list <- list(index.one)
  for(i in 1:dim){
    if(length(unlist(index.list[i])) > 2){
      new.index <- unlist(index.list[i])
      index.list[i + 1] <- new.index[odd.values]
      print(index.list[i + 1])
    }
  }
}



#NOTE: diamond-square algorithm appears to only work for matrices of very specific dimensions. 
#For example, n = 7 does NOT work because ceiling(7/2) = 4, which is even
#It is necessary for EVERY sub-matrix of the total matrix to be odd in order to apply diamond-square steps
# n = 15 also does not work because ceiling(15/2) = 8
#Does n = 17 work? ceil(17/2) = 9, ceil(9/2) = 5, ceil(5/2) = 3, done. Yes. 
#So, it is clear that n = 3, 5, 9, 17 all work, but none of the other odds from 1:17 do. 
#This sequence is equivalent to the expansion of 2^m + 1 (wikipedia)
#2^2 + 1 = 5, 2^3 + 1 = 9, 2^4 + 1 = 17, 2^10 + 1 = 1025... etc.

#Also note: a 9x9 matrix takes 3 steps to fill and here m = 3
#A 17x17 matrix takes 4 steps to fill, and here m = 4
#etc...

#So could build a function based on number of STEPS you want to apply (i.e. based on m, not n)
#This would cause less error results for user who may suck at doing everything, including basic math
#alter function to reflect this:

diamond.step <- function(new.matrix, corners.index, dim){
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  return(mean(corners.vector))
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  new.matrix[median(1:dim), median(1:dim)] <- diamond.step(new.matrix, corners.index, dim)
  return(new.matrix)
}

#Try again.
#I first want to first create a vector with all the indices necessary for a given "step" of filling the matrix
#For example, for a 9x9 matrix, the first step will use the vector c(1, 9)
#I will apply diamond step and square step using this vector for index generation
#The second step will use the second vector, c(1, 5, 9)
#Again, diamond step and square step will be applied using this vector for index generation
#Finally, the vector c(1, 3, 5, 7, 9) will be used to apply diamond and square step.

#How will this vector for index generation be used?
#Let the vector be named index.generator.
#For i in index.generator (except the last value in the vector) and for j in index.generator (except the last value in the vector), I want four values:
#I want index.generator[i], index.generator[i +1], index.generator[j], and index.generator[j+1]
#So for index.generator <- c(1, 5, 9), this for-loop-within-a-for-loop will give:
# i = 1, i + 1 = 5, i = 1, i + 1 = 5
#Then i = 1, i + 1 = 5, i = 5, i + 1 = 9
#Then i = 5, i + 1 = 9, i = 1, i + 1 = 5
#Then i = 5, i + 1 = 9, i = 5, i + 1 = 9

#These four values in each "list" of i, i + 1, j, j + 1 represent the two rows and two columns that define the CORNERs of each SUB-MATRIX
#Using expand.grid on these four values will give the INDEX for each CORNER of each SUBMATRIX 
#With these indices, can now apply diamond step. But need to change diamond step to the four index values as input

diamond.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  corners.index <- expand.grid(c(upper.row, lower.row), c(left.col, right.col))
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  return(mean(corners.vector))
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  index.gen <- c(1, dim)
  for(i in 1:(length(index.gen) - 1)){
    for(j in 1:(length(index.gen) - 1)){
      new.matrix[median(index.gen[i]:index.gen[i + 1]), median(index.gen[i]:index.gen[i + 1])] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      return(new.matrix)
    }
  }
}
#Oh hell yes look at this generalizable functionality.
#Now seek to add square step so can move on to apply this thinger to all index.gen vectors.
#Also need to create the index.gen vectors, but... can do this later.

#So... problem with square step. How to handle values that need FOUR values averaged (inners) vs. THREE (edges)
#Easiest answer, I think, is to use "if" statement.
#If the value in the slot = NA, then just put the ave. of three in there.
#If the value in the slot = !NA, then value for that location is 1/4(value of the last diamond step) + 3/4(value already in that location)
#This way any locations hit twice still give a proper average of four squares: three from the first pass, one from the second

#Also want square.step to take same values as diamond.step did
#So corners.index gives corners of the sub matrix in this order: upper left (1), upper right (2), lower left (3), lower right (4)
#Just like reading. 
#So the upper edge will have a value of mean(upper left, upper right, center)
#But here upper left = 1, upper right = 2
#left edge will have a value of mean(upper left, lower left, center)
#But this is mean(1, 3, center)
#right edge will have a value of mean(2, 4, center)
#And finally, lower edge will have a value of mean(3, 4, center)

#What are the indices on these four edges? This:
#sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
#upper.edge <- sub.matrix[upper.row, median(left.col, right.col)]
#lower.edge <- sub.matrix[lower.row, median(left.col, right.col)]
#left.edge <- sub.matrix[median(upper.row, lower.row), left.col]
#right.edge <- sub.matrix[median(upper.row, lower.row), right.col]

square.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  corners.index <- expand.grid(c(upper.row, lower.row), c(left.col, right.col))
  vertical.edges <- expand.grid(median(upper.row:lower.row), c(left.col, right.col))
  horiz.edges <- expand.grid(c(upper.row, lower.row), median(left.col:right.col))
  center <- sub.matrix[median(upper.row:lower.row), median(left.col, right.col)]
  sub.matrix[first.corners.index$Var1[[i]]]
  sub.matrix[]
}
#No. Unnecessarily complicated.

square.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  
  center <- sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))]
  
  upper.l.corner <- sub.matrix[1, 1]
  upper.r.corner <- sub.matrix[1, nrow(sub.matrix)]
  lower.l.corner <- sub.matrix[nrow(sub.matrix), 1]
  lower.r.corner <- sub.matrix[nrow(sub.matrix), nrow(sub.matrix)]
  
  #upper edge:
  sub.matrix[1, median(1:ncol(sub.matrix))] <- mean(upper.l.corner, upper.r.corner, center)
  #lower.edge:
  sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- mean(lower.l.corner, lower.r.corner, center)
  #left.edge:
  sub.matrix[median(1:nrow(sub.matrix)), 1] <- mean(upper.l.corner, lower.l.corner, center)
  #right.edge:
  sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- mean(upper.r.corner, lower.r.corner, center)
  
  return(sub.matrix)
}

#Combine with total matrix function:

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  index.gen <- c(1, dim)
  for(i in 1:(length(index.gen) - 1)){
    # i = rows
    for(j in 1:(length(index.gen) - 1)){
      #j = columns
      new.matrix[median(index.gen[i]:index.gen[i + 1]), median(index.gen[i]:index.gen[i + 1])] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
    }
  }
  return(new.matrix)
}

#works, but problem: mean is a function that takes an input x, so need to CATENATE the values I want to average

#Total fix for first step of diamond-square algorithm:

diamond.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  corners.index <- expand.grid(c(upper.row, lower.row), c(left.col, right.col))
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  return(mean(corners.vector))
}

square.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  
  center <- sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))]
  print(center)
  
  upper.l.corner <- sub.matrix[1, 1]
  upper.r.corner <- sub.matrix[1, nrow(sub.matrix)]
  lower.l.corner <- sub.matrix[nrow(sub.matrix), 1]
  lower.r.corner <- sub.matrix[nrow(sub.matrix), nrow(sub.matrix)]
  
  #upper edge:
  sub.matrix[1, median(1:ncol(sub.matrix))] <- mean(c(upper.l.corner, upper.r.corner, center))
  #lower.edge:
  sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- mean(c(lower.l.corner, lower.r.corner, center))
  #left.edge:
  sub.matrix[median(1:nrow(sub.matrix)), 1] <- mean(c(upper.l.corner, lower.l.corner, center))
  #right.edge:
  sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- mean(c(upper.r.corner, lower.r.corner, center))
  
  print(sub.matrix)
  return(sub.matrix)
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  index.gen <- c(1, dim)
  for(i in 1:(length(index.gen) - 1)){
    # i = rows
    for(j in 1:(length(index.gen) - 1)){
      #j = columns
      new.matrix[median(index.gen[i]:index.gen[i + 1]), median(index.gen[i]:index.gen[i + 1])] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
    }
  }
  return(new.matrix)
}
#Problem solved. Now what remains: to create a list of index vectors, apply the algorithm to each index vector
#Then add in "jitter" to each new value entered into matrix.
#and after alll this, change negative values to NA
#Test second step of algorithm:

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  index.gen <- c(1, dim)
  for(i in 1:(length(index.gen) - 1)){
    # i = rows
    for(j in 1:(length(index.gen) - 1)){
      #j = columns
      new.matrix[median(index.gen[i]:index.gen[i + 1]), median(index.gen[i]:index.gen[i + 1])] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
    }
  }
  print(new.matrix)
  index.gen.two <- c(1, median(1:dim), dim)
  print(index.gen.two)
  for(i in 1:(length(index.gen.two) - 1)){
    # i = rows
    for(j in 1:(length(index.gen.two) - 1)){
      #j = columns
      new.matrix[median(index.gen[i]:index.gen[i + 1]), median(index.gen[i]:index.gen[i + 1])] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
    }
  }
  return(new.matrix)
}
#choking on left.col:right.col when making the sub matrix for square step. Getting an NA instead of a number. why?
#Because didn't change index.gen to index.gen.two... yeah


create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  index.gen <- c(1, dim)
  for(i in 1:(length(index.gen) - 1)){
    # i = rows
    for(j in 1:(length(index.gen) - 1)){
      #j = columns
      new.matrix[median(index.gen[i]:index.gen[i + 1]), median(index.gen[i]:index.gen[i + 1])] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
    }
  }
  index.gen.two <- c(1, median(1:dim), dim)
  for(i in 1:(length(index.gen.two) - 1)){
    # i = rows
    for(j in 1:(length(index.gen.two) - 1)){
      #j = columns
      new.matrix[median(index.gen.two[i]:index.gen.two[i + 1]), median(index.gen.two[i]:index.gen.two[i + 1])] <- diamond.step(new.matrix, index.gen.two[i], index.gen.two[i + 1], index.gen.two[j], index.gen.two[j + 1])
      print(new.matrix)
      new.matrix[index.gen.two[i]:index.gen.two[i + 1], index.gen.two[j]:index.gen.two[j + 1]] <- square.step(new.matrix, index.gen.two[i], index.gen.two[i + 1], index.gen.two[j], index.gen.two[j + 1])
      print(new.matrix)
    }
  }
  return(new.matrix)
}

#Working, but not totally filling the matrix. Something wrong with the for-loops
#works for any index.gen vector of length two, but not above that

#Printing each section of the matrix as it fills shows that the diagonal of the matrix fills properly
#the square step actually appears to be working for the other parts as well, but the CENTER of the submatrix it is given is NA
#So it's something wrong with the diamond step
#since the square step actually REPLACES the properly-filled top left corner with a NEW NA after the next diamond step

create.matrix(3)
#[,1] [,2] [,3] [,4]      [,5] [,6] [,7] [,8]      [,9]
#[1,] -10.909944   NA   NA   NA -8.165050   NA   NA   NA -6.535673
#[2,]         NA   NA   NA   NA        NA   NA   NA   NA        NA
#[3,]         NA   NA   NA   NA        NA   NA   NA   NA        NA
#[4,]         NA   NA   NA   NA        NA   NA   NA   NA        NA
#[5,]  -7.563797   NA   NA   NA -7.049534   NA   NA   NA -6.535272
#[6,]         NA   NA   NA   NA        NA   NA   NA   NA        NA
#[7,]         NA   NA   NA   NA        NA   NA   NA   NA        NA
#[8,]         NA   NA   NA   NA        NA   NA   NA   NA        NA
#[9,]  -4.731912   NA   NA   NA -5.934018   NA   NA   NA -6.020609

#[,1] [,2]      [,3] [,4]      [,5]
#[1,] -10.909944   NA -9.165692   NA -8.165050
#[2,]         NA   NA        NA   NA        NA
#[3,]  -8.965274   NA -8.422081   NA -7.878889
#[4,]         NA   NA        NA   NA        NA
#[5,]  -7.563797   NA -7.678471   NA -7.049534

#[,1] [,2]      [,3] [,4]      [,5] [,6] [,7] [,8]      [,9]
#[1,] -10.909944   NA -9.165692   NA -8.165050   NA   NA   NA -6.535673
#[2,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[3,]  -8.965274   NA -8.422081   NA -7.878889   NA   NA   NA        NA
#[4,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[5,]  -7.563797   NA -7.678471   NA -7.049534   NA   NA   NA -6.535272
#[6,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[7,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[8,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[9,]  -4.731912   NA        NA   NA -5.934018   NA   NA   NA -6.020609

#The above shows that the FIRST part of the diamond-square works fine, and fills properly

#[,1] [,2] [,3] [,4]      [,5]
#[1,] -8.165050   NA   NA   NA -6.535673
#[2,]        NA   NA   NA   NA        NA
#[3,]        NA   NA   NA   NA        NA
#[4,]        NA   NA   NA   NA        NA
#[5,] -7.049534   NA   NA   NA -6.535272

#Here, square step has nothing to fill with EXCEPT NA because there is no center value to draw from, so diamond step problem


#[,1] [,2]      [,3] [,4]      [,5] [,6] [,7] [,8]      [,9]
#[1,] -10.909944   NA -9.165692   NA -8.165050   NA   NA   NA -6.535673
#[2,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[3,]  -8.965274   NA -7.071382   NA        NA   NA   NA   NA        NA
#[4,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[5,]  -7.563797   NA -7.678471   NA -7.049534   NA   NA   NA -6.535272
#[6,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[7,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[8,]         NA   NA        NA   NA        NA   NA   NA   NA        NA
#[9,]  -4.731912   NA        NA   NA -5.934018   NA   NA   NA -6.020609

#So the NEXT iteration OVERWRITES the properly-filled square with a new NA

#etc.... final product below:

#[,1] [,2]      [,3] [,4]      [,5] [,6]      [,7] [,8]      [,9]
#[1,] -10.909944   NA -9.165692   NA -8.165050   NA        NA   NA -6.535673
#[2,]         NA   NA        NA   NA        NA   NA        NA   NA        NA
#[3,]  -8.965274   NA -7.071382   NA        NA   NA        NA   NA        NA
#[4,]         NA   NA        NA   NA        NA   NA        NA   NA        NA
#[5,]  -7.563797   NA        NA   NA -7.049534   NA -6.656555   NA -6.535272
#[6,]         NA   NA        NA   NA        NA   NA        NA   NA        NA
#[7,]         NA   NA        NA   NA -6.456137   NA -6.384858   NA -6.313580
#[8,]         NA   NA        NA   NA        NA   NA        NA   NA        NA
#[9,]  -4.731912   NA        NA   NA -5.934018   NA -6.113162   NA -6.020609

#Actually though, printing mean values for each diamond step show that diamond step IS working
#So problem getting that value INTO the square step?
#Yes, "center" vector in square step is NA for two submatrices, despite diamond step producing a value
#BUT, it appears the problem is that the line moving the value from the diamond step to the original matrix is not running?
#Test without square step

diamond.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  corners.index <- expand.grid(c(upper.row, lower.row), c(left.col, right.col))
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  print(mean(corners.vector))
  return(mean(corners.vector))
}

square.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  
  center <- sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))]
  print(center)
  
  upper.l.corner <- sub.matrix[1, 1]
  upper.r.corner <- sub.matrix[1, nrow(sub.matrix)]
  lower.l.corner <- sub.matrix[nrow(sub.matrix), 1]
  lower.r.corner <- sub.matrix[nrow(sub.matrix), nrow(sub.matrix)]
  
  #upper edge:
  sub.matrix[1, median(1:ncol(sub.matrix))] <- mean(c(upper.l.corner, upper.r.corner, center))
  #lower.edge:
  sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- mean(c(lower.l.corner, lower.r.corner, center))
  #left.edge:
  sub.matrix[median(1:nrow(sub.matrix)), 1] <- mean(c(upper.l.corner, lower.l.corner, center))
  #right.edge:
  sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- mean(c(upper.r.corner, lower.r.corner, center))
  
  return(sub.matrix)
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  index.gen <- c(1, dim)
  for(i in 1:(length(index.gen) - 1)){
    # i = rows
    for(j in 1:(length(index.gen) - 1)){
      #j = columns
      new.matrix[median(index.gen[i]:index.gen[i + 1]), median(index.gen[i]:index.gen[i + 1])] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
    }
  }
  index.gen.two <- c(1, median(1:dim), dim)
  for(i in 1:(length(index.gen.two) - 1)){
    # i = rows
    for(j in 1:(length(index.gen.two) - 1)){
      #j = columns
      new.matrix[median(index.gen.two[i]:index.gen.two[i + 1]), median(index.gen.two[i]:index.gen.two[i + 1])] <- diamond.step(new.matrix, index.gen.two[i], index.gen.two[i + 1], index.gen.two[j], index.gen.two[j + 1])
    }
  }
  return(new.matrix)
}

#Yes, diamond step is NOT returning values for upper right and lower left corners, even though those values were created.
#So, an indexing problem?
#Try re-writing diamond step to take a sub-matrix like square step does:

diamond.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  corners.index <- expand.grid(c(1, nrow(sub.matrix)), c(1, ncol(sub.matrix)))
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  print(mean(corners.vector))
  sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))] <- mean(corners.vector)
  print(sub.matrix)
  return(sub.matrix)
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  index.gen <- c(1, dim)
  for(i in 1:(length(index.gen) - 1)){
    # i = rows
    for(j in 1:(length(index.gen) - 1)){
      #j = columns
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
    }
  }
  index.gen.two <- c(1, median(1:dim), dim)
  for(i in 1:(length(index.gen.two) - 1)){
    # i = rows
    for(j in 1:(length(index.gen.two) - 1)){
      #j = columns
      new.matrix[index.gen.two[i]:index.gen.two[i + 1], index.gen.two[j]:index.gen.two[j + 1]] <- diamond.step(new.matrix, index.gen.two[i], index.gen.two[i + 1], index.gen.two[j], index.gen.two[j + 1])
    }
  }
  return(new.matrix)
}

#Now diamond step appears to be working. Add square step back in for total fix:

diamond.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  corners.index <- expand.grid(c(1, nrow(sub.matrix)), c(1, ncol(sub.matrix)))
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))] <- mean(corners.vector)
  return(sub.matrix)
}

square.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  
  center <- sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))]
  
  upper.l.corner <- sub.matrix[1, 1]
  upper.r.corner <- sub.matrix[1, nrow(sub.matrix)]
  lower.l.corner <- sub.matrix[nrow(sub.matrix), 1]
  lower.r.corner <- sub.matrix[nrow(sub.matrix), nrow(sub.matrix)]
  
  #upper edge:
  sub.matrix[1, median(1:ncol(sub.matrix))] <- mean(c(upper.l.corner, upper.r.corner, center))
  #lower.edge:
  sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- mean(c(lower.l.corner, lower.r.corner, center))
  #left.edge:
  sub.matrix[median(1:nrow(sub.matrix)), 1] <- mean(c(upper.l.corner, lower.l.corner, center))
  #right.edge:
  sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- mean(c(upper.r.corner, lower.r.corner, center))
  
  return(sub.matrix)
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  index.gen <- c(1, dim)
  for(i in 1:(length(index.gen) - 1)){
    # i = rows
    for(j in 1:(length(index.gen) - 1)){
      #j = columns
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
    }
  }
  index.gen.two <- c(1, median(1:dim), dim)
  for(i in 1:(length(index.gen.two) - 1)){
    # i = rows
    for(j in 1:(length(index.gen.two) - 1)){
      #j = columns
      new.matrix[index.gen.two[i]:index.gen.two[i + 1], index.gen.two[j]:index.gen.two[j + 1]] <- diamond.step(new.matrix, index.gen.two[i], index.gen.two[i + 1], index.gen.two[j], index.gen.two[j + 1])
      new.matrix[index.gen.two[i]:index.gen.two[i + 1], index.gen.two[j]:index.gen.two[j + 1]] <- square.step(new.matrix, index.gen.two[i], index.gen.two[i + 1], index.gen.two[j], index.gen.two[j + 1])
    }
  }
  return(new.matrix)
}

#YESSSSSSSSS.
#Next step: alter square step to produce alternate means depending on if the edge space contains an NA or a numeric
#Then create a for-loop from i:numb.steps that creates a NEW index.gen vector for each iteration 
#This should work because the INPUT of my matrix function is equal to the NUMBER OF STEPS required to fill the matrix
#So if the algorithm loops through numb.steps times, using the replacement index.gen vector each time, it should fill the matrix

#Check what the mean should be if there's already a number in the slot:
#(x + y + z)/3 is the number already in the slot. WANT (x + y + z + a)/4 in the end. 
#really, only new value is the center value. So can just take 3/4 number in slot plus 1/4 center. 
#I think I had already figured this out... um...

square.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  
  center <- sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))]
  
  upper.l.corner <- sub.matrix[1, 1]
  upper.r.corner <- sub.matrix[1, nrow(sub.matrix)]
  lower.l.corner <- sub.matrix[nrow(sub.matrix), 1]
  lower.r.corner <- sub.matrix[nrow(sub.matrix), nrow(sub.matrix)]
  
  #upper edge:
  if(is.na(sub.matrix[1, median(1:ncol(sub.matrix))]) == TRUE){
    sub.matrix[1, median(1:ncol(sub.matrix))] <- mean(c(upper.l.corner, upper.r.corner, center))
  } else {
    sub.matrix[1, median(1:ncol(sub.matrix))] <- (3/4)*sub.matrix[1, median(1:ncol(sub.matrix))] + (1/4)*center
  }
  
  #lower.edge:
  if(is.na(sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))]) == TRUE){
    sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- mean(c(lower.l.corner, lower.r.corner, center))
  } else {
    sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- (3/4)*sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] + (1/4)*center
  }
  
  #left.edge:
  if(is.na(sub.matrix[median(1:nrow(sub.matrix)), 1]) == TRUE){
    sub.matrix[median(1:nrow(sub.matrix)), 1] <- mean(c(upper.l.corner, lower.l.corner, center))
  } else {
    sub.matrix[median(1:nrow(sub.matrix)), 1] <- (3/4)*sub.matrix[median(1:nrow(sub.matrix)), 1] + (1/4)*center
  }
  
  #right.edge:
  if(is.na(sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)]) == TRUE){
    sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- mean(c(upper.r.corner, lower.r.corner, center))
  } else {
    sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- (3/4)*sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] + (1/4)*center
  }
  
  return(sub.matrix)
}

#Score. 
#Next step: create a for-loop from i:numb.steps that creates a NEW index.gen vector for each iteration 
#Then add jitter. Then done. 

#Test for index generation: 
#Use sequence?
#First index.gen vector needs to be a sequence from 1:dim, by dim. Check:

dim <- 9
index.gen <- seq(1, dim, by = dim)
#Nope, doesn't give dim at end. Try sequence from 1 through dim + 1?
index.gen <- seq(1, (dim + 1), by = dim)
#Nope, gives sequence 1, 10. Try sequence from 1 through dim by dim - 1?
index.gen <- seq(1, (dim), by = (dim - 1))
#Yes. Works. 

#Next, try to get sequence 1, 5, 9. Try sequence from 1 through dim by dim - 1 / 2?
index.gen <- seq(1, (dim), by = ((dim - 1)/2))
#Yes, works. Now generalize. I am dividing by 2 here, was dividing by 1 above
#BUT am dividing by FOUR in the next step. 
#Sequence of i = 1, 2, 4, 8... 
#This sequence is generated by 2^(i-1) if i starts at i = 1
#So I am dividing by 2^(i-1) for each step in the algorithm
#In other words, for step one, my index.gen vector is counting by (dim-1)/2^0
#For the SECOND step (i = 2), my index.gen is counting by (dim - 1)/2^1
#etc.
#cool.
#Generalized form:
index.gen <- seq(1, (dim), by = (dim - 1)/(2^(i-1)))



diamond.step <- function(new.matrix, upper.row, lower.row, left.col, right.col){
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  corners.index <- expand.grid(c(1, nrow(sub.matrix)), c(1, ncol(sub.matrix)))
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))] <- mean(corners.vector)
  return(sub.matrix)
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  
  for(k in 1:number.steps){
    index.gen <- seq(1, (dim), by = (dim - 1)/(2^(k-1)))
    for(i in 1:(length(index.gen) - 1)){
      # i = rows
      for(j in 1:(length(index.gen) - 1)){
        #j = columns
        new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
        new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1])
      }
    }
  }
  return(new.matrix)
}

#Take that. 

#Next step: add jitter to each step, but progressively less each time. 
#For jitter, it makes sense to decrease the noise exponentially with each step, not linearly
#So reuse my variable 2^(k-1) in each step? Divide SD by 2^(k-1)? 
#Will need to add a new variable to square step and diamond step functions to allow input of k-variable.

diamond.step <- function(new.matrix, upper.row, lower.row, left.col, right.col, SD.factor){
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  corners.index <- expand.grid(c(1, nrow(sub.matrix)), c(1, ncol(sub.matrix)))
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))] <- mean(corners.vector) + rnorm(1, 0, (15/SD.factor))
  return(sub.matrix)
}

square.step <- function(new.matrix, upper.row, lower.row, left.col, right.col, SD.factor){
  
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  
  center <- sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))]
  
  upper.l.corner <- sub.matrix[1, 1]
  upper.r.corner <- sub.matrix[1, nrow(sub.matrix)]
  lower.l.corner <- sub.matrix[nrow(sub.matrix), 1]
  lower.r.corner <- sub.matrix[nrow(sub.matrix), nrow(sub.matrix)]
  
  #upper edge:
  if(is.na(sub.matrix[1, median(1:ncol(sub.matrix))]) == TRUE){
    sub.matrix[1, median(1:ncol(sub.matrix))] <- mean(c(upper.l.corner, upper.r.corner, center)) + rnorm(1, 0, (15/SD.factor))
  } else {
    sub.matrix[1, median(1:ncol(sub.matrix))] <- (3/4)*sub.matrix[1, median(1:ncol(sub.matrix))] + (1/4)*center + rnorm(1, 0, (15/SD.factor))
  }
  
  #lower.edge:
  if(is.na(sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))]) == TRUE){
    sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- mean(c(lower.l.corner, lower.r.corner, center)) + rnorm(1, 0, (15/SD.factor))
  } else {
    sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- (3/4)*sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] + (1/4)*center + rnorm(1, 0, (15/SD.factor))
  }
  
  #left.edge:
  if(is.na(sub.matrix[median(1:nrow(sub.matrix)), 1]) == TRUE){
    sub.matrix[median(1:nrow(sub.matrix)), 1] <- mean(c(upper.l.corner, lower.l.corner, center)) + rnorm(1, 0, (15/SD.factor))
  } else {
    sub.matrix[median(1:nrow(sub.matrix)), 1] <- (3/4)*sub.matrix[median(1:nrow(sub.matrix)), 1] + (1/4)*center + rnorm(1, 0, (15/SD.factor))
  }
  
  #right.edge:
  if(is.na(sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)]) == TRUE){
    sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- mean(c(upper.r.corner, lower.r.corner, center)) + rnorm(1, 0, (15/SD.factor))
  } else {
    sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- (3/4)*sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] + (1/4)*center + rnorm(1, 0, (15/SD.factor))
  }
  
  return(sub.matrix)
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, 15)
  }
  
  for(k in 1:number.steps){
    index.gen <- seq(1, (dim), by = (dim - 1)/(2^(k-1)))
    SD.factor <- (2^(k-1))
    for(i in 1:(length(index.gen) - 1)){
      # i = rows
      for(j in 1:(length(index.gen) - 1)){
        #j = columns
        new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1], SD.factor)
        new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1], SD.factor)
      }
    }
  }
  return(new.matrix)
}

#Works, but image gets more homogeneous with increasing # steps because initial SD does not increase according to matrix size. 
#Need another argument(dim) to determine INITIAL SD (that will then be divided by k whatever)
#Matrix size is going up exponentially, so SD needs to go up exponentially as well

diamond.step <- function(new.matrix, upper.row, lower.row, left.col, right.col, initial.SD, SD.factor){
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  corners.index <- expand.grid(c(1, nrow(sub.matrix)), c(1, ncol(sub.matrix)))
  corners.vector <- numeric(4)
  for(i in 1:4){
    corners.vector[i] <- new.matrix[corners.index$Var1[[i]], corners.index$Var2[[i]]]
  }
  sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))] <- mean(corners.vector) + rnorm(1, 0, (initial.SD/SD.factor))
  return(sub.matrix)
}

square.step <- function(new.matrix, upper.row, lower.row, left.col, right.col, initial.SD, SD.factor){
  
  sub.matrix <- new.matrix[upper.row:lower.row, left.col:right.col]
  
  center <- sub.matrix[median(1:nrow(sub.matrix)), median(1:ncol(sub.matrix))]
  
  upper.l.corner <- sub.matrix[1, 1]
  upper.r.corner <- sub.matrix[1, nrow(sub.matrix)]
  lower.l.corner <- sub.matrix[nrow(sub.matrix), 1]
  lower.r.corner <- sub.matrix[nrow(sub.matrix), nrow(sub.matrix)]
  
  #upper edge:
  if(is.na(sub.matrix[1, median(1:ncol(sub.matrix))]) == TRUE){
    sub.matrix[1, median(1:ncol(sub.matrix))] <- mean(c(upper.l.corner, upper.r.corner, center)) + rnorm(1, 0, (initial.SD/SD.factor))
  } else {
    sub.matrix[1, median(1:ncol(sub.matrix))] <- (3/4)*sub.matrix[1, median(1:ncol(sub.matrix))] + (1/4)*center + rnorm(1, 0, (initial.SD/SD.factor))
  }
  
  #lower.edge:
  if(is.na(sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))]) == TRUE){
    sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- mean(c(lower.l.corner, lower.r.corner, center)) + rnorm(1, 0, (initial.SD/SD.factor))
  } else {
    sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] <- (3/4)*sub.matrix[nrow(sub.matrix), median(1:ncol(sub.matrix))] + (1/4)*center + rnorm(1, 0, (initial.SD/SD.factor))
  }
  
  #left.edge:
  if(is.na(sub.matrix[median(1:nrow(sub.matrix)), 1]) == TRUE){
    sub.matrix[median(1:nrow(sub.matrix)), 1] <- mean(c(upper.l.corner, lower.l.corner, center)) + rnorm(1, 0, (initial.SD/SD.factor))
  } else {
    sub.matrix[median(1:nrow(sub.matrix)), 1] <- (3/4)*sub.matrix[median(1:nrow(sub.matrix)), 1] + (1/4)*center + rnorm(1, 0, (initial.SD/SD.factor))
  }
  
  #right.edge:
  if(is.na(sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)]) == TRUE){
    sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- mean(c(upper.r.corner, lower.r.corner, center)) + rnorm(1, 0, (initial.SD/SD.factor))
  } else {
    sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] <- (3/4)*sub.matrix[median(1:nrow(sub.matrix)), ncol(sub.matrix)] + (1/4)*center + rnorm(1, 0, (initial.SD/SD.factor))
  }
  
  return(sub.matrix)
}

create.matrix <- function(number.steps){
  if(number.steps <= 0){
    stop("Number of steps must be greater than zero or no matrix can be produced!")
  }
  dim <- (2^(number.steps) + 1)
  initial.SD <- 2.2^(dim/2.2)
  
  new.matrix <- matrix(nrow = dim, ncol = dim)
  first.corners.index <- expand.grid(c(1, dim), c(1, dim))
  for(i in 1:4){
    new.matrix[first.corners.index$Var1[[i]], first.corners.index$Var2[[i]]] <- rnorm(1, 10, initial.SD)
  }
  
  for(k in 1:number.steps){
    
    index.gen <- seq(1, (dim), by = (dim - 1)/(2^(k-1)))
    SD.factor <- 0.8*(2^(k-1))
    
    for(i in 1:(length(index.gen) - 1)){
      # i = rows
      for(j in 1:(length(index.gen) - 1)){
        #j = columns
        new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- diamond.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1], initial.SD, SD.factor)
        new.matrix[index.gen[i]:index.gen[i + 1], index.gen[j]:index.gen[j + 1]] <- square.step(new.matrix, index.gen[i], index.gen[i + 1], index.gen[j], index.gen[j + 1], initial.SD, SD.factor)
      }
    }
  }
  return(new.matrix)
}

#enh, good enough.Still starts getting flat at approx. number.steps = 6



#water

#https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/


water <- function(x){
  if(x >= 0){x <- x} else if(x < 0){
    x <- NA
  }
}

terrain.matrix <- matrix(c(-1, 4), 4, 4)

terrain.matrix.water <- apply(terrain.matrix, 1:2, water)
terrain.matrix.water
#PROBLEM: changed all the "4"s to "NULL". PROBLEM: had only "if" statements. Needed an "if-else" statement to run properly.