#' Simulate a Plant Ecosystem
#'
#' Setup.plants: makes a list of objects needed to run the plant ecosystem function.
#'
#' @param reproduce A vector of length n storing reproduction probabilities for n plant species. Must be values between 0 and 1.
#' @param survive A vector of length n storing survival probabilities for n plant species. Must be values between 0 and 1.
#' @param compete.matrix An nxn matrix storing the probability the plant in row i will outcompete the plant in column j, given n plant species. Must be values between 0 and 1.
#' @param names A vector of length n storing the names of the n plants in the ecosystem. If no names are given, they will be automatically assigned.
#' @return A list containing all survival, reproduction, and survival values for the user's plant ecosystem.
#'
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
  if(any(reproduce > 1)| any(reproduce < 0) | any(survive > 1) | any(survive < 0) | any(compete.matrix) > 1 | any(compete.matrix < 0)){
    stop("Reproduction, survival and competition probabilities must be values between zero and one!")
  }
  reproduce <- setNames(reproduce, names)
  survive <- setNames(survive, names)
  rownames(compete.matrix) <- names
  colnames(compete.matrix) <- names

  return(list(reproduce = reproduce, survive = survive, compete.matrix = compete.matrix, names = names))
}

#survival function: determines if plants live or die:

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

#Seed.plants: fills the terrain with plants...
#AS MANY AS YOU SPECIFY, NO MORE, NO LESS.
#NONE WILL FALL ON WATER AND DIE; 100% SEEDED ON LAND.
#ALL SEEDED RANDOMLY, NO BIAS TOWARDS THOSE SEEDED FIRST OR LAST

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
  #creates a vector that will become the indices for the shuffled plants.to.add vector (to eliminate all possible bias in seeding)
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


#' run.plant.ecosystem: makes an array showing changes in the plant ecosystem (location and abundance) through time
#'
#' @param terrain A matrix generated using the terrain function into which plants can be placed
#' @param setup.plants A list generated from the setup.plants function using the survival, reproduction, and competition probabilities for each plant wanted in the ecosystem
#' @param numb.plants.per.sp A vector of length n storing the values for the initial population size desired for each of n plant species.
#' @param timesteps A numeric value representing the number of timesteps over which the user wishes to simulate their ecosystem.

run.plant.ecosystem <- function(terrain, setup.plants, numb.plants.per.sp, timesteps){
  plant.array <- array(dim = c(nrow(terrain), ncol(terrain), timesteps + 1))
  #Creates plant array to put stuff into.
  plant.array[,,1] <- seed.plants(terrain, setup.plants, numb.plants.per.sp)
  for(i in 1:(timesteps)){
    plant.array[,,(i + 1)] <- plant.timestep(plant.array[,,i], setup.plants)
  }
  return(plant.array)
}

#Drawing a random location for the F1 generation offspring to be placed into:

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

#Revised plant.timestep function including reproduction:

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

#Competition function:

compete <- function(parent.cell, potential.offspring.cell, setup.plants){
  winner <- sample(c(parent.cell, potential.offspring.cell), 1, prob = c(setup.plants$compete.matrix[parent.cell, potential.offspring.cell], (1 - setup.plants$compete.matrix[parent.cell, potential.offspring.cell])))
  return(winner)
}

#Revised reproduction function including competition element:

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
