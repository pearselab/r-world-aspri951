#plants

#setup.plants = a function that checks that all the inputs for reproduction, survival, and competition parameters are correct
#checks to see if repro and surv vectors are of the same length as # species in simulation
#checks to see if competition arguemnt is a matrix with dim equal to length of repro/surv
#stop function if conditions aren't met

#how to set names of matrix? Test:

bekkeh <- matrix(1:16, nrow = 4, ncol = 4)
rownames(bekkeh) <- c("You", "want", "sum", "blue?")
colnames(bekkeh) <- c("learnin'", "to jump", "for", "bekkeh")

bekkeh["sum", "for"]
#Cool. Can call an INDEX by NAME. USEFUL.


setup.plants <- function(reproduce, survive, compete.matrix, names = NULL){
  if(is.null(names) == TRUE){
    names <- letters[1:length(reproduce)]
  }
  if(length(names) != length(reproduce)){
    stop("Each plant must have a name.")
  }
  if(length(reproduce) != length(survive)){
    stop("Reproduction and survival parameters needed for each species.")
  }
  if(nrow(compete.matrix) != length(reproduce) | ncol(compete.matrix) != length(reproduce)){
    stop("Competition matrix must have competition parameters for each pairwise combination of species.")
  }
  if(any(reproduce) > 1 | any(reproduce) < 0 | any(survive) > 1 | any(survive < 0)){
    #what about matrix probabilities? This too?
    stop("Reproduction and survival probabilities must be values between zero and one!")
  }
  reproduction <- setNames(reproduction, names)
  survive <- setNames(survive, names)
  rownames(compete.matrix) <- names
  colnames(compete.matrix) <- names
  
  return(list(reproduce = reproduce, survive = survive, compete.matrix = compete.matrix, names = names))
}

#For the competition matrix, what probability is what? Is it probability ROW will survive, or COL will survive?
#Will need to define



#survive: function that determines whether an individual survives to the next time step
#this function will do something to ONE CELL
#THis function will also need to match probabilities for EACH SPECIES to the species in the cell
#So will need survive vector in function


#for loop... for i in 1: length(names), if cell = name i, then run if-statement about survival
#No, not needed!
#Can check PROPER SPECIES by calling the NAME as the INDEX (see bekkeh["blue?", "for"])
#So if cell <- plant A, then run statement: if(runif(weifj) <= probability plant A survives), put "plant A" in cell
#else, put "" in the cell (blank, NOT NA)

survival <- function(cell, setup.plants){
  if(is.na(cell) == TRUE){
    cell <- NA
  }
  if(runif(1) <= setup.plants$survive[cell]){
    cell <- cell
  } else {
    cell <- ""
  }
  return(cell)
}
  
#test:

survive <- c(0.5, 0.9, 0.01)
survive <- setNames(survive, c("coin", "super", "sucky"))
blahblah <- list(survive = survive, etc = "blah", blargh = "fatcat")
cell_1 <- "coin"
cell_2 <- "super"
cell_3 <- "sucky"
cell_4 <- NA
survival(cell_1, blahblah)
survival(cell_2, blahblah)
survival(cell_3, blahblah)
survival(cell_4, blahblah)

#problem, not calling survival because list stuff NOT NAMED. 
#Need NAMES when MAKING LIST or LIST HAS NO NAMES and CANNOT CALL PARTS OF LIST

#New problem: doesn't work for NA. Says missing value where T/F needed. Probably trying to run BOTH if-statements? 
#Try making else-statement to fix this?

survival <- function(cell, setup.plants){
  if(is.na(cell) == TRUE){
    cell <- NA
  } else if(runif(1) <= setup.plants$survive[cell]){
    cell <- cell
  } else {
    cell <- ""
  }
  return(cell)
}

#Problem solved. Final test:

cell_5 <- ""
survival(cell_5, blahblah)

#Nope, same problem. Tries to run second if-statement and finds the name is NOT in the list of plant names

survival <- function(cell, setup.plants){
  if(is.na(cell) == TRUE){
    cell <- NA
  } else if(cell == ""){
    cell <- ""
  } else if(is.na(setup.plants$survive[cell]) == TRUE){
    stop("You just discovered a new species of plant! Whatever is in this cell shouldn't exist... try again.")
  } else if(runif(1) <= setup.plants$survive[cell]){
    cell <- cell
  } else if(runif(1) > setup.plants$survive[cell]){
    cell <- ""
  } else {
    stop("How have you even done this...")
  }
  return(cell)
}

#Test: 

cell_5 <- ""
cell_6 <- "George"
survival(cell_5, blahblah)
survival(cell_6, blahblah)

#Done!





#plant.timestep: a function that takes a matrix of plants, loops over it and applies "survival" function to each cell
#Will need two for loops, nested: for (i in 1:nrow(matrix)) and then for(j in 1:ncol(matrix)) to hit each cell
#matrix[i, j] <- survival(matrix[i, j], setup.plants)
#want to return new matrix at end

plant.timestep <- function(plant.matrix, setup.plants){
  new.plant.matrix <- plant.matrix
  for(i in 1:nrow(plant.matrix)){
    for(j in 1:ncol(plant.matrix)){
      new.plant.matrix[i, j] <- survival(plant.matrix[i,j], setup.plants)
    }
  }
  return(new.plant.matrix)
}

test.plants <- matrix(nrow = 4, ncol = 4, c("coin", "super", "sucky", "coin"))
#a matrix with coin in the first row, super in the second, sucky in the third, coin in the last
survive <- c(0.5, 0.9, 0.01)
survive <- setNames(survive, c("coin", "super", "sucky"))
blahblah <- list(survive = survive, etc = "blah", blargh = "fatcat")

plant.timestep(test.plants, blahblah)

#A problem in survival function: it is possible to get BELOW the p-value on the first run, but ABOVE it in the second
#I have accidentally drawn TWO random values, not just one...
#Fix:

survival <- function(cell, setup.plants){
  if(is.na(cell) == TRUE){
    cell <- NA
  } else if(cell == ""){
    cell <- ""
  } else if(is.na(setup.plants$survive[cell]) == TRUE){
    stop("You just discovered a new species of plant! Whatever is in this cell shouldn't exist... try again.")
  } else {
    random.draw <- runif(1)
    if(random.draw <= setup.plants$survive[cell]){
      cell <- cell
    } else if(random.draw > setup.plants$survive[cell]){
      cell <- ""
    }
  }
  return(cell)
}

#Problem solved. Plant.timestep now works. 



#run.plant.ecosystem: function that seeds plants into an initial matrix (of same size as terrain),
#then builds an array using survive function each timestep
#need function that takes terrain, #plants of each species you want, plant.timestep, setup.plants, and timesteps+1
#Need timesteps + 1 because the first "sheet" in the array is time t = 0. Need timesteps after that, so timesteps plus one(initial conditions)
#How to draw plants? If you put in first set, then second, then third, and just replace ones that fall on top of another, bias towards last plant species run
#Fix this by creating places for the TOTAL number of plants, then assigning a place for each?
#No, better option: sample function: samples WITHOUT REPLACEMENT BY DEFAULT
#Problem of having plants land on top of one another solved?

#If too many plants, cannot fit in matrix. 
#if sum(plants blah) > #cells in matrix, problem: too many plants


run.plant.ecosystem <- function(terrain, setup.plants, plant.timestep, numb.plants.per.sp, timesteps){
  if(numb.plants.per.sp != )
  plant.array <- array(dim = c(nrow(terrain), ncol(terrain), timesteps + 1))
  #Creates plant array to put stuff into.
  
}


#I want to randomly shuffle a vector of plant names so there is no bias towards the first or last plants for filling
#I also REFUSE to build a matrix that culls a random number of plants that land on water.
#I want to know EXACTLY how many plants are initially put in the matrix, and I want to DECIDE that number

#I want to call cells of a matrix WITHOUT using row/col:

test.plants <- matrix(nrow = 4, ncol = 4, c("coin", "super", "sucky", "coin"))
test.plants
#[,1]    [,2]    [,3]    [,4]   
#[1,] "coin"  "coin"  "coin"  "coin" 
#[2,] "super" "super" "super" "super"
#[3,] "sucky" "sucky" "sucky" "sucky"
#[4,] "coin"  "coin"  "coin"  "coin" 
which(test.plants == "coin")
#[1]  1  4  5  8  9 12 13 16

#So each cell MUST have a call number... just need to figure out how
#Oh well that's simple... if you don't use a comma, it calls the CELL, not the ROW/COL
test.plants[3]
#[1] "sucky"
test.plants[,3]
#[1] "coin"  "super" "sucky" "coin" 

#top to bottom, left to right... fills columns

bob <- matrix(nrow = 4, ncol = 4, 1:16)
#[,1] [,2] [,3] [,4]
#[1,]    1    5    9   13
#[2,]    2    6   10   14
#[3,]    3    7   11   15
#[4,]    4    8   12   16

bob[6] <- "bob"
#[,1] [,2]  [,3] [,4]
#[1,] "1"  "5"   "9"  "13"
#[2,] "2"  "bob" "10" "14"
#[3,] "3"  "7"   "11" "15"
#[4,] "4"  "8"   "12" "16"

#So now I just need to make a sequence from one to length nrow(terrain) ^2 in vector form...
#Then make a vector where NAs are using which(terrain, NA)
#Then remove everything in the NA vector from the first vector (sequence of numbers)
#and finally, sample a certain number of values from this "corrected" vector
#and for every i in 1:length(corrected vector), put shuffled.plant.vector[i] in matrix[i]

#Will need a stop function that indicates that there are length(NA vector) water cells in your terrain. 
#You cannot seed more than nrow(terrain) ^2 - length(NA vector) plants into your matrix

#How to call names of a vector? names function?

survive <- c(0.5, 0.9, 0.01)
survive <- setNames(survive, c("coin", "super", "sucky"))
names(survive[3])
#[1] "sucky"

#Can use this to create vector of plant names to seed into the matrix


seed.plants <- function(terrain, setup.plants, number.plants){
  if(length(number.plants) != length(setup.plants$names)){
    stop("There must be an initial population size given for every plant species in the 'number of plants per species' vector!")
  }
  all.terrain.locations <- seq(1, nrow(terrain)^2, 1)
  #Vector of all possible cells in the terrain matrix, from which locations for plants can be drawn
  water.locations <- which(is.na(terrain) == TRUE)
  #Vector of all the cells in the initial matrix that contain water, and therefore are NOT potential locations for plants.
  terrain.locations.minus.water <- all.terrain.locations[-water.locations]
  #Vector of all cells in matrix WITHOUT water, and therefore potential locations for plants
  total.number.plants <- sum(number.plants)
  #Adds up the initial population values for all the plant species to give total population (plants of any species)
  if(total.number.plants > length(terrain.locations.minus.water)){
    stop("There are more plants to seed than there are locations on the terrain to put plants!", "\n",
         "  You currently have ", total.number.plants, " plants, and only ", length(terrain.locations.minus.water), " places to put them!")
  }
  #Checks to see if there are too many plants to fit on your terrain
  locations.for.plants.to.go <- sample(terrain.locations.minus.water, total.number.plants)
  #Draws a random sample of locations in which to put the total number of plants you said you wanted to seed
  
  number.plants <- setNames(number.plants, setup.plants$names)
  #Takes the vector containing number of plants of each species to seed, and names them appropriately
  plants.to.add <- character(0)
  for(i in 1:length(number.plants)){
    plants.to.add <- c(plants.to.add, rep(names(number.plants[i]), number.plants[i]))
  }
  #Creates a vector of plants, with each plant species repeated the number of times specified by the user
  random.ordering.for.plants <- sample(1:length(plants.to.add), length(plants.to.add))
  #creates a vector that will become the indices for the shuffled plants.to.add vector (to eliminate bias in seeding)
  shuffled.plants.to.add <- character(0)
  for(i in 1:length(plants.to.add)){
    shuffled.plants.to.add[i] <- plants.to.add[random.ordering.for.plants[i]]
  }
  #SHUFFLES the vector of plants to add randomly to the terrain because I hate bias and want things seeded randomly.
  plant.matrix <- matrix(nrow = nrow(terrain), ncol = ncol(terrain), "")
  #Creates the final plant matrix, into which plants and water will be seeded
  plant.matrix[water.locations] <- NA
  #Sets all water locations to NA
  plant.matrix[locations.for.plants.to.go] <- shuffled.plants.to.add
  #Puts the plants into the matrix, and none should fall on top of each other or on water!
  return(plant.matrix)
}

#Hooray! Now to put into the whole ecosystem function:

run.plant.ecosystem <- function(terrain, setup.plants, numb.plants.per.sp, timesteps){
  plant.array <- array(dim = c(nrow(terrain), ncol(terrain), timesteps + 1))
  #Creates plant array to put stuff into.
  plant.array[,,1] <- seed.plants(terrain, setup.plants, numb.plants.per.sp)
  return(plant.array)
}

#Test:

terrain <- matrix(nrow = 9, ncol = 9, sample(27, 27))
terrain[which(terrain < 10)] <- NA
survive <- c(0.5, 0.9, 0.01)
names <- c("coin", "super", "sucky")
survive <- setNames(survive, names)
setup.plants <- list(survive = survive, etc = "blah", blargh = "fatcat", names = names)
numb.plants.per.sp <- c(5, 2, 7)

run.plant.ecosystem(terrain, setup.plants, numb.plants.per.sp, 4)

#Oh hell yes. Works on first try (minus the copy errors in object names)
#Test error message:

numb.plants.per.sp <- c(5, 50, 40)
run.plant.ecosystem(terrain, setup.plants, numb.plants.per.sp, 4)

#Cool.Now add timesteps

run.plant.ecosystem <- function(terrain, setup.plants, numb.plants.per.sp, timesteps){
  plant.array <- array(dim = c(nrow(terrain), ncol(terrain), timesteps + 1))
  #Creates plant array to put stuff into.
  plant.array[,,1] <- seed.plants(terrain, setup.plants, numb.plants.per.sp)
  for(i in 1:(timesteps)){
    plant.array[,,(i + 1)] <- plant.timestep(plant.array[,,i], setup.plants)
  }
  return(plant.array)
}

#IT WORRRRKS... SEE TEST:

terrain <- matrix(nrow = 9, ncol = 9, sample(27, 27))
terrain[which(terrain < 10)] <- NA
survive <- c(0.5, 0.9, 0.01)
names <- c("coin", "super", "sucky")
survive <- setNames(survive, names)
setup.plants <- list(survive = survive, etc = "blah", blargh = "fatcat", names = names)
numb.plants.per.sp <- c(5, 2, 7)

run.plant.ecosystem(terrain, setup.plants, numb.plants.per.sp, 8)

#Just as expected, sucky plants die out first, then coin (50/50 coin flip) plants, and lastly, super plants remain




#reproduction function
#want to determine the possible cells around the reproducing plant that can house an offspring
#make a vector of options, will be length 8 (three to left, three to right, one above, one below)
#the cell to the left is cell # minus ncol or nrow of matrix
#cell to the right is cell # plus ncol
#so c(cell#-nrow, cell#+nrow, cell#+1, cell#-1, cell#-nrow+1, cell#-nrow-1, cell#+nrow+1, cell#+nrow-1)
#then draw one from this vector to place the new plant in
#sample(potential.baby.location, 1)
#matrix[sample(potential.baby.location, 1)] <- matrix[i]

#test potential for-loop:
plant.matrix <- matrix(nrow = 4, ncol = 4)

for(i in 1:nrow(plant.matrix)^2){
  potential.offspring.locations <- c((i - nrow(plant.matrix)), (i + nrow(plant.matrix)), (i + 1), (i - 1), (i - nrow(plant.matrix) + 1), 
                                     (i - nrow(plant.matrix) - 1), (i + nrow(plant.matrix) + 1), (i + nrow(plant.matrix) + 1))
  print(potential.offspring.locations)
}
#complicated to filter out those off the matrix because it wraps around edges

for(i in 1:nrow(plant.matrix)){
  for(j in 1:ncol(plant.matrix)){
    potential.offspring.locations <- as.matrix(expand.grid(i + c(-1, 0, 1), j + c(-1, 0, 1)))
    print(potential.offspring.locations)
  }
}
#Now, need to remove any row with the original plant inside (i + 0, j + 0)
#and remove any row with values less than 1 or greater than nrow(plant.matrix)

for(i in 1:nrow(plant.matrix)){
  for(j in 1:ncol(plant.matrix)){
    potential.offspring.locations <- as.matrix(expand.grid(i + c(-1, 0, 1), j + c(-1, 0, 1)))
    for(k in 1:9){
      if(potential.offspring.locations[k, 1] == i & potential.offspring.locations[k, 2] == j){
        potential.offspring.locations <- potential.offspring.locations[-k,]
      }
    }
    print(potential.offspring.locations)
  }
}
#error, subscripts out of bounds?

for(i in 1:nrow(plant.matrix)){
  for(j in 1:ncol(plant.matrix)){
    potential.offspring.locations <- as.matrix(expand.grid(i + c(-1, 0, 1), j + c(-1, 0, 1)))
    for(k in 1:9){
      print(potential.offspring.locations[k, 1])
      print(potential.offspring.locations[k, 2])
    }
  }
}
#But this works...

plant.matrix <- matrix(nrow = 4, ncol = 4)

#matrix doesn't like changing sizes in the mdidle of a loop, perhaps?

for(i in 1:nrow(plant.matrix)){
  for(j in 1:ncol(plant.matrix)){
    potential.offspring.locations <- as.matrix(expand.grid(i + c(0, -1, 1), j + c(0, -1, 1)))
    potential.offspring.locations.two <- potential.offspring.locations
    for(k in 1:nrow(potential.offspring.locations)){
      if(potential.offspring.locations[k, 1] == i & potential.offspring.locations[k, 2] == j){
        potential.offspring.locations.two <- potential.offspring.locations[-k,]
      }
    }
    cat(i,j)
    print(potential.offspring.locations.two)
  }
}
#cool, working (and cat confirms that it's deleting the center point as it should)

for(i in 1:nrow(plant.matrix)){
  for(j in 1:ncol(plant.matrix)){
    potential.offspring.locations <- as.matrix(expand.grid(i + c(0, -1, 1), j + c(0, -1, 1)))
    potential.offspring.locations.two <- potential.offspring.locations
    potential.offspring.locations.three <- potential.offspring.locations.two
    for(k in 1:nrow(potential.offspring.locations)){
      if(potential.offspring.locations[k, 1] == i & potential.offspring.locations[k, 2] == j){
        potential.offspring.locations.two <- potential.offspring.locations[-k,]
      }
      if(potential.offspring.locations[k, 1] > nrow(plant.matrix) | potential.offspring.locations[k, 2] > nrow(plant.matrix)
         | potential.offspring.locations[k, 1] < 1 | potential.offspring.locations[k, 2] < 1){
          potential.offspring.locations.three <- potential.offspring.locations.two[-k,] 
       }
    }
    cat(i,j)
    print(potential.offspring.locations.three)
  }
}

#Doesn't work. Try splitting into vectors individually

offspring.location <- function(F0.row, F0.col, plant.matrix){
  potential.F1.locations <- as.matrix(expand.grid(F0.row + c(0, -1, 1), F0.col + c(0, -1, 1)))
  #Matrix storing all possible locations for plant offspring, INCLUDING the location of the parent
  potential.F1.locations.minus.center <- potential.F1.locations
  #Matrix storing potential locations for plant offspring MINUS the location of the parent
  for(k in 1:nrow(potential.F1.locations)){
    if(potential.F1.locations[k, 1] == F0.row & potential.F1.locations[k, 2] == F0.col){
      potential.F1.locations.minus.center <- potential.F1.locations[-k,]
    }
    #Loop that takes the list of all possible locations and removes the center point (the location of the parent plant)
    
  potential.F1.row <- potential.F1.locations.minus.center[,1]
  potential.F1.col <- potential.F1.locations.minus.center[,2]
  #Vector from the possible offspring location matrix storing the indices of potential rows and columns for offspring
  print(potential.F1.row)
  print(potential.F1.col)
  
  print(which(potential.F1.row < 1))
  print(nrow(plant.matrix))
  
  rows.to.remove <- c(which(potential.F1.row > nrow(plant.matrix)), which(potential.F1.row < 1))
  col.to.remove <- c(which(potential.F1.col > ncol(plant.matrix)), which(potential.F1.col < 1))
  #Vectors determining which row and column locations are off the grid (terrain), and need to be removed
  
  print(rows.to.remove)
  print(col.to.remove)
  
  potential.F1.row <- potential.F1.row[-c(rows.to.remove, col.to.remove)]
  potential.F1.col <- potential.F1.col[-c(rows.to.remove, col.to.remove)]
  #corrected vectors storing potential row/col locations for offspring, all invalid locations removed
  
  print(potential.F1.row)
  print(potential.F1.col)
  
  potential.location.index <- seq(from = 1, to = length(potential.F1.row), by = 1)
  offspring.location.index <- sample(potential.location.index, 1)
  #draws a random sample from the vector of potential F1 locations
  
  offspring.location <- c(potential.F1.row[offspring.location.index], potential.F1.col[offspring.location.index])
  return(offspring.location)
  }
}




#Final, de-bugged version below:

offspring.location <- function(F0.row, F0.col, plant.matrix){
  potential.F1.locations <- as.matrix(expand.grid(F0.row + c(0, -1, 1), F0.col + c(0, -1, 1)))
  #Matrix storing all possible locations for plant offspring, INCLUDING the location of the parent
  potential.F1.locations.minus.center <- potential.F1.locations
  #Matrix storing potential locations for plant offspring MINUS the location of the parent
  for(k in 1:nrow(potential.F1.locations)){
    if(potential.F1.locations[k, 1] == F0.row & potential.F1.locations[k, 2] == F0.col){
      potential.F1.locations.minus.center <- potential.F1.locations[-k,]
    }
    #Loop that takes the list of all possible locations and removes the center point (the location of the parent plant)
    
    potential.F1.row <- potential.F1.locations.minus.center[,1]
    potential.F1.col <- potential.F1.locations.minus.center[,2]
    #Vector from the possible offspring location matrix storing the indices of potential rows and columns for offspring
    
    rows.to.remove <- c(which(potential.F1.row > nrow(plant.matrix)), which(potential.F1.row < 1))
    col.to.remove <- c(which(potential.F1.col > ncol(plant.matrix)), which(potential.F1.col < 1))
    #Vectors determining which row and column locations are off the grid (terrain), and need to be removed
    
    if(length(rows.to.remove) > 0 | length(col.to.remove > 0)){
      potential.F1.row <- potential.F1.row[-c(rows.to.remove, col.to.remove)]
      potential.F1.col <- potential.F1.col[-c(rows.to.remove, col.to.remove)]
    }
    #corrected vectors storing potential row/col locations for offspring, all invalid locations removed
    
    potential.location.index <- seq(from = 1, to = length(potential.F1.row), by = 1)
    offspring.location.index <- sample(potential.location.index, 1)
    #draws a random sample from the vector of potential F1 locations
    
    offspring.location <- c(potential.F1.row[offspring.location.index], potential.F1.col[offspring.location.index])
    return(offspring.location)
  }
}

#need to determine whether randomly drawn prob is less than repro prob. If so, then run the above stuff

#reproduction function should go AFTER the survival function is complete, this seems simpler

#if two plants are next to each other, don't want one to reproduce over the top of its neighbor...
#then have the loop move to the neighbor's cell (now filled with the offspring of the first cell)...
#And then have the offspring "reproduce" into another cell... 

#need to make sure offspring can't reproduce, so need to put offspring into NEW matrix but draw from the ORIGINAL each time
#This will prevent mixing of generations

reproduction <- function(plant.matrix, reproduce){
  new.plant.matrix <- plant.matrix
  #creates a new matrix for the "next generation" to be seeded into without messing up the original matrix I'm drawing from
  for(i in 1:nrow(plant.matrix)^2){
    potential.offspring.locations <- c((i - nrow(plant.matrix)), (i + nrow(plant.matrix)), (i + 1), (i - 1), (i - nrow(plant.matrix) + 1), 
      (i - nrow(plant.matrix) - 1), (i + nrow(plant.matrix) + 1), (i + nrow(plant.matrix) + 1))
    
    random.draw <- runif(1)
    if(random.draw <= setup.plants$reproduce[plant.matrix[i]]){
      #stuff to determine where thing goes
    }
  }
}  

#Try again

reproduction <- function(F0.row, F0.col, plant.matrix, setup.plants){
  new.plant.matrix <- plant.matrix
  #creates a new matrix for the "next generation" to be seeded into without messing up the original matrix I'm drawing from
  cell <- plant.matrix[F0.row, F0.col]
  if(is.na(plant.matrix[cell]) == TRUE){
    cell <- NA
  } else if(cell == ""){
    cell <- ""
  } else if(is.na(setup.plants$reproduce[cell]) == TRUE){
    stop("You just discovered a new species of plant! Whatever is in this cell shouldn't exist... try again.")
  } else {
    if(runif(1) <= setup.plants$reproduce[cell]){
      offspring.location <- offspring.location(F0.row, F0.col, plant.matrix)
      if(is.na(matrix[offspring.location] == FALSE)){
        new.plant.matrix[offspring.location] <- cell
      }
    }
  }
  return(new.plant.matrix)
}



#New plant.timestep function with reproduction inside

plant.timestep <- function(plant.matrix, setup.plants){
  new.plant.matrix <- plant.matrix
  repro.plant.matrix <- new.plant.matrix
  for(i in 1:nrow(plant.matrix)){
    for(j in 1:ncol(plant.matrix)){
      new.plant.matrix[i, j] <- survival(plant.matrix[i,j], setup.plants)
    }
  }
  for(i in 1:nrow(new.plant.matrix)){
    for(j in 1:ncol(new.plant.matrix)){
      repro.plant.matrix <- reproduction(i, j, new.plant.matrix, setup.plants)
    }
  }
  return(repro.plant.matrix)
}

#new test:
  
reproduction <- function(plant.matrix, setup.plants){
  repro.plant.matrix <- plant.matrix
  #creates a new matrix for the "next generation" to be seeded into without messing up the original matrix I'm drawing from
  for(i in nrow(plant.matrix)){
    for(j in ncol(plant.matrix)){
      cell <- plant.matrix[i, j]
      print(c(i,j))
      if(is.na(plant.matrix[cell]) == TRUE){
        cell <- NA
      } else if(cell == ""){
        cell <- ""
      } else if(is.na(setup.plants$reproduce[cell]) == TRUE){
        stop("You just discovered a new species of plant! Whatever is in this cell shouldn't exist... try again.")
      } else {
        if(runif(1) <= setup.plants$reproduce[cell]){
          offspring.location <- offspring.location(F0.row, F0.col, plant.matrix)
          print(offspring.location)
          if(is.na(matrix[offspring.location] == FALSE)){
            new.plant.matrix[offspring.location] <- cell
          }
        }
      }
    }
  }
  print(repro.plant.matrix)
  return(repro.plant.matrix)
}
  
plant.timestep <- function(plant.matrix, setup.plants){
  new.plant.matrix <- plant.matrix
  for(i in 1:nrow(plant.matrix)){
    for(j in 1:ncol(plant.matrix)){
      new.plant.matrix[i, j] <- survival(plant.matrix[i,j], setup.plants)
    }
  }
  repro.plant.matrix <- reproduction(new.plant.matrix, setup.plants)
  return(repro.plant.matrix)
}


#New new test: OH MY GOD. UGHHHHHHHHHHHHHHHHHHH STUPIDITY.PERHAPS A RANGEEEE OF I,J VALUES WOULD HELP.............

reproduction <- function(plant.matrix, setup.plants){
  repro.plant.matrix <- plant.matrix
  #creates a new matrix for the "next generation" to be seeded into without messing up the original matrix I'm drawing from
  for(i in 1:nrow(plant.matrix)){
    for(j in 1:ncol(plant.matrix)){
      cell <- plant.matrix[i, j]
      if(is.na(cell) == TRUE){
        cell <- NA
        print("first works")
      } else if(cell == ""){
        cell <- ""
        print("second works")
      } else if(is.na(setup.plants$reproduce[cell]) == TRUE){
        stop("You just discovered a new species of plant! Whatever is in this cell shouldn't exist... try again.")
      } else {
        if(runif(1) <= setup.plants$reproduce[cell]){
          print("third works")
          offspring.location <- offspring.location(i, j, plant.matrix)
          print(offspring.location)
          if(is.na(plant.matrix[offspring.location[1], offspring.location[2]] == FALSE)){
            new.plant.matrix[offspring.location] <- cell
          }
        }
      }
    }
  }
  return(repro.plant.matrix)
}



#Without all the print stuff, FUNCTIONAL version:

reproduction <- function(plant.matrix, setup.plants){
  repro.plant.matrix <- plant.matrix
  #creates a new matrix for the "next generation" to be seeded into without messing up the original matrix I'm drawing from
  for(i in 1:nrow(plant.matrix)){
    for(j in 1:ncol(plant.matrix)){
      cell <- plant.matrix[i, j]
      if(is.na(cell) == TRUE){
        cell <- NA
      } else if(cell == ""){
        cell <- ""
      } else if(is.na(setup.plants$reproduce[cell]) == TRUE){
        stop("You just discovered a new species of plant! Whatever is in this cell shouldn't exist... try again.")
      } else {
        if(runif(1) <= setup.plants$reproduce[cell]){
          offspring.location <- offspring.location(i, j, plant.matrix)
          if(is.na(plant.matrix[offspring.location[1], offspring.location[2]] == TRUE)){
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- NA
          } else {
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- cell
          }
        }
      }
    }
  }
  return(repro.plant.matrix)
}

#Working, but why filling only the NA spots with a baby plant?!
#Needed an else-statement. Doesn't work with if statement only. For some reason doesn't check true/false properly.



#Competition function:

#Should go in reproduction function within the reproduction function at the end where offspring location is drawn
#only need to run the function if the cell location is NOT NA or "", so should go after that
#Thus, need 1) if NA, do this, else if "" do this, else compete function
#function will need an input of cell and plant.matrix[potential offspring location]
#Will return the content of the cell
#so plant.matrix[potential offspring location] <- compete(cell, plant.matrix[potential offspring location])

compete <- function(parent.cell, potential.offspring.cell){
  cat(parent.cell, "parent cell", "\n")
  cat(potential.offspring.cell, "potential offspring cell", "\n")
  print(comp.matrix[parent.cell, potential.offspring.cell])
  winner <- sample(c(parent.cell, potential.offspring.cell), 1, prob = comp.matrix[parent.cell, potential.offspring.cell])
  return(winner)
}

#Test stuff:

reproduction <- function(plant.matrix, setup.plants){
  repro.plant.matrix <- plant.matrix
  #creates a new matrix for the "next generation" to be seeded into without messing up the original matrix I'm drawing from
  for(i in 1:nrow(plant.matrix)){
    for(j in 1:ncol(plant.matrix)){
      cell <- plant.matrix[i, j]
      if(is.na(cell) == TRUE){
        cell <- NA
      } else if(cell == ""){
        cell <- ""
      } else if(is.na(setup.plants$reproduce[cell]) == TRUE){
        stop("You just discovered a new species of plant! Whatever is in this cell shouldn't exist... try again.")
      } else if(is.na(setup.plants$reproduce[cell]) == FALSE){
        if(runif(1) <= setup.plants$reproduce[cell]){
          print(cell)
          offspring.location <- offspring.location(i, j, plant.matrix)
          if(is.na(plant.matrix[offspring.location[1], offspring.location[2]] == TRUE)){
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- NA
          } else if(plant.matrix[offspring.location[1], offspring.location[2]] == ""){
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- ""
          } else {
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- compete(cell, repro.plant.matrix[offspring.location[1], offspring.location[2]])
          }
        }
      }
    }
  }
  return(repro.plant.matrix)
}

#problem: cell is somehow being "" sometimes, rather than always a plant name... messing up the competition function


#Another try....

reproduction <- function(plant.matrix, setup.plants){
  repro.plant.matrix <- plant.matrix
  #creates a new matrix for the "next generation" to be seeded into without messing up the original matrix I'm drawing from
  for(i in 1:nrow(plant.matrix)){
    for(j in 1:ncol(plant.matrix)){
      if(is.na(setup.plants$reproduce[plant.matrix[i, j]]) == FALSE){
        if(runif(1) <= setup.plants$reproduce[plant.matrix[i, j]]){
          cat(plant.matrix[i, j], "repro function", "\n")
          offspring.location <- offspring.location(i, j, plant.matrix)
          if(is.na(setup.plants$reproduce[plant.matrix[offspring.location[1], offspring.location[2]]]) == FALSE){
            cat("part of loop", plant.matrix[i, j])
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- compete(plant.matrix[i, j], repro.plant.matrix[offspring.location[1], offspring.location[2]], setup.plants)
          }
        }
      }
    }
  }
  return(repro.plant.matrix)
}


#Problemmmmmm I don't have setup.plants in my function....... also comp.matrix is compete.matrix in setup.plants, so...


compete <- function(parent.cell, potential.offspring.cell, setup.plants){
  cat(parent.cell, "parent cell", "\n")
  cat(potential.offspring.cell, "potential offspring cell", "\n")
  print(setup.plants$compete.matrix[parent.cell, potential.offspring.cell])
  winner <- sample(c(parent.cell, potential.offspring.cell), 1, prob = c(setup.plants$compete.matrix[parent.cell, potential.offspring.cell], (1 - setup.plants$compete.matrix[parent.cell, potential.offspring.cell])))
  return(winner)
}

#Okay. Mostly working. Re-try with regular reproduction function. 

reproduction <- function(plant.matrix, setup.plants){
  repro.plant.matrix <- plant.matrix
  #creates a new matrix for the "next generation" to be seeded into without messing up the original matrix I'm drawing from
  for(i in 1:nrow(plant.matrix)){
    for(j in 1:ncol(plant.matrix)){
      cell <- plant.matrix[i, j]
      if(is.na(cell) == TRUE){
        cell <- NA
      } else if(cell == ""){
        cell <- ""
      } else if(is.na(setup.plants$reproduce[cell]) == TRUE){
        stop("You just discovered a new species of plant! Whatever is in this cell shouldn't exist... try again.")
      } else if(is.na(setup.plants$reproduce[cell]) == FALSE){
        if(runif(1) <= setup.plants$reproduce[cell]){
          offspring.location <- offspring.location(i, j, plant.matrix)
          if(is.na(plant.matrix[offspring.location[1], offspring.location[2]] == TRUE)){
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- NA
          } else if(repro.plant.matrix[offspring.location[1], offspring.location[2]] == ""){
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- cell
          } else {
            repro.plant.matrix[offspring.location[1], offspring.location[2]] <- compete(cell, repro.plant.matrix[offspring.location[1], offspring.location[2]], setup.plants)
          }
        }
      }
    }
  }
  return(repro.plant.matrix)
}



compete <- function(parent.cell, potential.offspring.cell, setup.plants){
  winner <- sample(c(parent.cell, potential.offspring.cell), 1, prob = c(setup.plants$compete.matrix[parent.cell, potential.offspring.cell], (1 - setup.plants$compete.matrix[parent.cell, potential.offspring.cell])))
  return(winner)
}