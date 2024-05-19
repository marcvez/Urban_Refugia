# UR source-sink model

# Demographic parameters and predation preassures

# Lifespan (years)
max_lifespan <- 18

# Sexual maturity (years)
sex_mat <- 2

# Number of eggs per laying (2.30 +- 0.55)
n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))

# Laying episodes per year (if females are old, they could lay 2 times in a year)
n_laying <- 1

# Eggs hatching rate (%)
# This values are from captivity. In nature this results are expected to be higher
hatch_rate <- runif(1, 0.60, 0.75)

# Annual survival (% of survival, a little bit stochastic)
surv <- runif(1, 0.75, 0.83)

# carrying capacity (nº ind/ha)
# This value comes from the number of lizards observed (60 ind) in 1919.6 m2 of optimal habitat in Chalets
k <- 312

# emigration (peri-urban -> urban)
# fixed amount of individuals
emigration <- k/10

# snake predation preassure (% total population/year)
snake_pred <- 0.2

# cat predation preassure (% total population/year)
cat_pred <- 0.05

# urbanisation index (0 for peri-urban, 1 for urban)
urban_index <- 1

# urban protection against snakes (counter snake effect)
urban_prot <- 0.2



# Part 1: Simulate population lizards

# initial lizard population
n_lizards <- k

lizard_pop <- array(data = NA, dim = c(100000, 4, 10))

dimnames(lizard_pop)[[2]] <- c("ID", "Age", "Sex", "Status")

for(i in 1:k){
  
  lizard_pop[i, 1, 1] <- i
  
  lizard_pop[i, 2, 1] <- as.numeric(trunc(abs(rnorm(1, mean = 9, sd = 4))))
  
  lizard_pop[i, 3, 1] <- sample(c("Male", "Female"), 1)
  
  lizard_pop[i, 4, 1] <- 1
  
}


# simulate years
for(i in 2:10){
  
  last_ind <- sum(complete.cases(lizard_pop[, 1, i - 1]))
  
  # for each individual
  for(j in 1:last_ind){
    
    # if the lizard is dead, the information remains the same
    if(as.numeric(lizard_pop[j, 4, i - 1]) == 0){
      
      lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
      
      lizard_pop[j, 2, i] <- lizard_pop[j, 2, i - 1]
      
      lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
      
      lizard_pop[j, 4, i] <- lizard_pop[j, 4, i - 1]
      
      # if the lizard is alive but it's 18 years old, it dies:
    } else if (as.numeric(lizard_pop[j, 2, i - 1]) == 18){
      
      lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
      
      lizard_pop[j, 2, i] <- lizard_pop[j, 2, i - 1]
      
      lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
      
      lizard_pop[j, 4, i] <- 0
      
    } else {
      
      # probability of survival
      # surv_prob <- runif(1, 0.75, 0.83)
      
      surv_prob <- 1
      
      surv_random <- runif(1, 0, 1)
      
      # dead
      if(surv_random > surv_prob){
        
        lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
        
        lizard_pop[j, 2, i] <- lizard_pop[j, 2, i - 1]
        
        lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
        
        lizard_pop[j, 4, i] <- 0
        
        # alive
      } else {
        
        lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
        
        lizard_pop[j, 2, i] <- as.numeric(lizard_pop[j, 2, i - 1]) + 1
        
        lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
        
        lizard_pop[j, 4, i] <- 1
        
      }
      
    }
    
    # if the individual we are testing was alive in the previous time step and it's a female, and it's older than 2 years old
    if(lizard_pop[j, 3, i - 1] == "Female" && as.numeric(lizard_pop[j, 4, i - 1]) == 1 && as.numeric(lizard_pop[j, 2, i - 1]) >= 2){
      
      # if the female is old (5 last years)
      if(as.numeric(lizard_pop[j, 2, i - 1]) > (18 - 5)){
        
        # it lays eggs two times in one year
        for(p in 1:2){
          
          # number of eggs layed
          n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
          
          # at least one egg layed
          if (n_eggs > 0){
            
            # probability of hatching of each egg, produces a new individual
            for(m in 1:n_eggs){
              
              # hatch_prob <- runif(1, 0.60, 0.75)
              
              hatch_prob <- 1
              
              hatch_random <- runif(1, 0, 1)
              
              if(hatch_random < hatch_prob){
                
                last_ind <- last_ind + 1
                
                lizard_pop[last_ind, 1, i] <- last_ind
                
                lizard_pop[last_ind, 2, i] <- 0
                
                lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                
                lizard_pop[last_ind, 4, i] <- 1
                
                # no eggs layed
              } else {
                
                
              }
              
            }
            
          }
          
        }
        
        # "young" female, only one laying episode
      } else {
        
        # number of eggs layed
        n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
        
        # at least one egg layed
        if (n_eggs > 0){
          
          for(m in 1:n_eggs){
            
            # hatch_prob <- runif(1, 0.60, 0.75)
            
            hatch_prob <- 1
            
            hatch_random <- runif(1, 0, 1)
            
            if(hatch_random < hatch_prob){
              
              last_ind <- last_ind + 1
              
              lizard_pop[last_ind, 1, i] <- last_ind
              
              lizard_pop[last_ind, 2, i] <- 0
              
              lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
              
              lizard_pop[last_ind, 4, i] <- 1
              
            } else {
              
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
            
}



# Part 2: Integrate inside function

lizard_projection <- function(initial_pop = 100, n_generations = 10){
  
  n_lizards <- initial_pop
  
  lizard_pop <- array(data = NA, dim = c(999999, 4, n_generations))
  
  dimnames(lizard_pop)[[2]] <- c("ID", "Age", "Sex", "Status")
  
  for(i in 1:n_lizards){
    
    lizard_pop[i, 1, 1] <- i
    
    lizard_pop[i, 2, 1] <- as.numeric(trunc(abs(rnorm(1, mean = 9, sd = 4))))
    
    lizard_pop[i, 3, 1] <- sample(c("Male", "Female"), 1)
    
    lizard_pop[i, 4, 1] <- 1
    
  }
  
  
  # simulate years
  for(i in 2:n_generations){
    
    last_ind <- sum(complete.cases(lizard_pop[, 1, i - 1]))
    
    # for each individual
    for(j in 1:last_ind){
      
      # if the lizard is dead, the information remains the same
      if(as.numeric(lizard_pop[j, 4, i - 1]) == 0){
        
        lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
        
        lizard_pop[j, 2, i] <- lizard_pop[j, 2, i - 1]
        
        lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
        
        lizard_pop[j, 4, i] <- lizard_pop[j, 4, i - 1]
        
        # if the lizard is alive but it's 18 years old, it dies:
      } else if (as.numeric(lizard_pop[j, 2, i - 1]) == 18){
        
        lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
        
        lizard_pop[j, 2, i] <- lizard_pop[j, 2, i - 1]
        
        lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
        
        lizard_pop[j, 4, i] <- 0
        
      } else {
        
        # probability of survival
        surv_prob <- runif(1, 0.75, 0.83)
        
        surv_random <- runif(1, 0, 1)
        
        # dead
        if(surv_random > surv_prob){
          
          lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
          
          lizard_pop[j, 2, i] <- lizard_pop[j, 2, i - 1]
          
          lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
          
          lizard_pop[j, 4, i] <- 0
          
          # alive
        } else {
          
          lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
          
          lizard_pop[j, 2, i] <- as.numeric(lizard_pop[j, 2, i - 1]) + 1
          
          lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
          
          lizard_pop[j, 4, i] <- 1
          
        }
        
      }
      
      # if the individual we are testing was alive in the previous time step and it's a female, and it's older than 2 years old
      if(lizard_pop[j, 3, i - 1] == "Female" && as.numeric(lizard_pop[j, 4, i - 1]) == 1 && as.numeric(lizard_pop[j, 2, i - 1]) >= 2){
        
        # if the female is old (5 last years)
        if(as.numeric(lizard_pop[j, 2, i - 1]) > (18 - 5)){
          
          # it lays eggs two times in one year
          for(p in 1:2){
            
            # number of eggs layed
            n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
            
            # at least one egg layed
            if (n_eggs > 0){
              
              # probability of hatching of each egg, produces a new individual
              for(m in 1:n_eggs){
                
                hatch_prob <- runif(1, 0.60, 0.75)
                
                hatch_random <- runif(1, 0, 1)
                
                if(hatch_random < hatch_prob){
                  
                  last_ind <- last_ind + 1
                  
                  lizard_pop[last_ind, 1, i] <- last_ind
                  
                  lizard_pop[last_ind, 2, i] <- 0
                  
                  lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                  
                  lizard_pop[last_ind, 4, i] <- 1
                  
                  # no eggs layed
                } else {
                  
                  
                }
                
              }
              
            }
            
          }
          
          # "young" female, only one laying episode
        } else {
          
          # number of eggs layed
          n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
          
          # at least one egg layed
          if (n_eggs > 0){
            
            for(m in 1:n_eggs){
              
              hatch_prob <- runif(1, 0.60, 0.75)
              
              hatch_random <- runif(1, 0, 1)
              
              if(hatch_random < hatch_prob){
                
                last_ind <- last_ind + 1
                
                lizard_pop[last_ind, 1, i] <- last_ind
                
                lizard_pop[last_ind, 2, i] <- 0
                
                lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                
                lizard_pop[last_ind, 4, i] <- 1
                
              } else {
                
                
              }
              
            }
            
          }
          
        }
        
      }
      
    }
    
    assign("lizard_pop", lizard_pop, envir=globalenv())
    
  }
  
  n_ind_pop <- as.data.frame(matrix(data = NA, ncol = n_generations, nrow = 1))
  
  for(i in 1:n_generations){
    
    n_ind_pop[i] <- sum(lizard_pop[, 4, i] == 1, na.rm = T)
    
  }
  
  assign("n_ind_pop", n_ind_pop, envir=globalenv())
  
  plot(NULL, xlim = c(1, n_generations), ylim = c(0, max(n_ind_pop)), xlab = "Nº generations", ylab = "Nº ind")
  
  lines(1:n_generations, n_ind_pop, type = "l")
  
}

lizard_projection(100, 10)



# part 3: upgrade base function with densodependency and external mortality

lizard_projection <- function(initial_pop = 100, n_generations = 10, cat_pred = 0.10, snake_pred = 0.20, snake_intro = 10, k = 312, urban_index = 1, emigration_perc = 0.1) {
  
  # Initial population and array
  n_lizards <- initial_pop
  lizard_pop <- array(data = NA, dim = c(999999, 4, n_generations))
  dimnames(lizard_pop)[[2]] <- c("ID", "Age", "Sex", "Status")
  
  # First individuals
  for (i in 1:n_lizards) {
    lizard_pop[i, 1, 1] <- i
    lizard_pop[i, 2, 1] <- as.numeric(trunc(abs(rnorm(1, mean = 9, sd = 4))))
    lizard_pop[i, 3, 1] <- sample(c("Male", "Female"), 1)
    lizard_pop[i, 4, 1] <- 1
  }
  
  # Time simulation
  for (i in 2:n_generations) {
    
    # Keep tracking of number of rows filled
    last_ind <- sum(complete.cases(lizard_pop[, 1, i - 1]))
    
    # Select previous generation
    lizards_generation <- as.data.frame(lizard_pop[, , i - 1])
    
    # Select only alive individuals
    lizards_alive <- subset(lizards_generation, as.numeric(Status) == 1)
    
    # Cat mortality
    if (nrow(lizards_alive) > 0) {
      dead_ind_cat <- sample(1:nrow(lizards_alive), trunc(nrow(lizards_alive) * cat_pred), replace = FALSE)
      dead_ids_cat <- lizards_alive[dead_ind_cat, "ID"]
      for (id in dead_ids_cat) {
        idx <- which(lizard_pop[, 1, i - 1] == id)
        lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
        lizard_pop[idx, 4, i] <- 0
      }
    }
    
    # Select individuals still alive after cat mortality
    lizards_alive_after_cats <- subset(as.data.frame(lizard_pop[, , i - 1]), as.numeric(Status) == 1 & is.na(lizard_pop[, 4, i]))
    
    # Snake mortality (starting from snake_intro)
    # Full urbanisation counters snake effect
    if (i >= (snake_intro + 1)) {
      if (nrow(lizards_alive_after_cats) > 0) {
        dead_ind_snake <- sample(1:nrow(lizards_alive_after_cats), trunc(nrow(lizards_alive_after_cats) * (snake_pred - (urban_index * snake_pred))), replace = FALSE)
        dead_ids_snake <- lizards_alive_after_cats[dead_ind_snake, "ID"]
        for (id in dead_ids_snake) {
          idx <- which(lizard_pop[, 1, i - 1] == id)
          lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
          lizard_pop[idx, 4, i] <- 0
        }
      }
    }
    
    # Select individuals still alive after snake mortality
    lizards_alive_after_snakes <- subset(as.data.frame(lizard_pop[, , i - 1]), as.numeric(Status) == 1 & is.na(lizard_pop[, 4, i]))
    
    # Denso dependency
    n_alive <- nrow(lizards_alive_after_snakes)
    
    if (n_alive > k) {
      surplus <- n_alive - k
      surplus_ind <- sample(1:nrow(lizards_alive_after_snakes), surplus, replace = FALSE)
      surplus_ids <- lizards_alive_after_snakes[surplus_ind, "ID"]
      for (id in surplus_ids) {
        idx <- which(lizard_pop[, 1, i - 1] == id)
        lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
        lizard_pop[idx, 4, i] <- 0
      }
    }
    
    # The rest of individuals
    for (j in 1:last_ind) {
      
      # If there is an NA, jump to next loop
      if (is.na(lizard_pop[j, 4, i - 1])) {
        next
      }
      
      # If the individual is already dead in this generation (external death causes), jump to the next loop
      if (!is.na(lizard_pop[j, 4, i]) && as.numeric(lizard_pop[j, 4, i]) == 0) {
        next
      }
      
      # If lizard was dead in the previous generation, mark it as dead
      if (as.numeric(lizard_pop[j, 4, i - 1]) == 0) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        next
      }
      
      # If the lizard is alive, but it's older than 18 years old, it dies
      if (as.numeric(lizard_pop[j, 2, i - 1]) == 18) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        lizard_pop[j, 4, i] <- 0
        next
      }
      
      # Survival probability
      surv_prob <- runif(1, 0.75, 0.83)
      surv_random <- runif(1, 0, 1)
      
      # If it fails, the lizard is marked as dead
      if (surv_random > surv_prob) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        lizard_pop[j, 4, i] <- 0
        
      } else {
        # If the lizard survives, it gets older and survives
        lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
        lizard_pop[j, 2, i] <- as.numeric(lizard_pop[j, 2, i - 1]) + 1
        lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
        lizard_pop[j, 4, i] <- 1
      }
      
      # Reproduction
      # If the individual is a female, older than 2 years old and alive
      if (lizard_pop[j, 3, i - 1] == "Female" && as.numeric(lizard_pop[j, 4, i - 1]) == 1 && as.numeric(lizard_pop[j, 2, i - 1]) >= 2) {
        
        if (as.numeric(lizard_pop[j, 2, i - 1]) > (18 - 5)) {
          # If it's an old female, it lays eggs two times
          
          for (p in 1:2) {
            
            # Number of eggs laid
            n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
            
            if (n_eggs > 0) {
              
              for (m in 1:n_eggs) {
                
                # Hatching probability
                hatch_prob <- runif(1, 0.60, 0.75)
                hatch_random <- runif(1, 0, 1)
                
                # If the egg hatches
                if (hatch_random < hatch_prob) {
                  
                  # Add a new row
                  last_ind <- last_ind + 1
                  lizard_pop[last_ind, 1, i] <- last_ind
                  lizard_pop[last_ind, 2, i] <- 0
                  lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                  lizard_pop[last_ind, 4, i] <- 1
                }
              }
            }
          }
          
        } else {
          
          # If the female is young, only one laying
          n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
          
          if (n_eggs > 0) {
            
            for (m in 1:n_eggs) {
              
              hatch_prob <- runif(1, 0.60, 0.75)
              hatch_random <- runif(1, 0, 1)
              
              if (hatch_random < hatch_prob) {
                
                last_ind <- last_ind + 1
                lizard_pop[last_ind, 1, i] <- last_ind
                lizard_pop[last_ind, 2, i] <- 0
                lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                lizard_pop[last_ind, 4, i] <- 1
              }
            }
          }
        }
      }
    }
    
    # Emigration (percentage of k)
    
    emigration <- trunc(k * emigration_perc)
    
    for (e in 1:emigration) {
      last_ind <- last_ind + 1
      lizard_pop[last_ind, 1, i] <- last_ind
      lizard_pop[last_ind, 2, i] <- as.numeric(trunc(abs(rnorm(1, mean = 9, sd = 4))))
      lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
      lizard_pop[last_ind, 4, i] <- 1
    }
    
    assign("lizard_pop", lizard_pop, envir=globalenv())
    
  }
  
  # Track nº of individuals per generation
  n_ind_pop <- as.data.frame(matrix(data = NA, ncol = n_generations, nrow = 1))
  for (i in 1:n_generations) {
    n_ind_pop[i] <- sum(lizard_pop[, 4, i] == 1, na.rm = TRUE)
  }
  
  colnames(n_ind_pop) <- paste0("Generation_", 1:n_generations)
  
  assign("n_ind_pop", n_ind_pop, envir=globalenv())
  
  # plot nº of individuals per generation
  plot(NULL, xlim = c(1, n_generations), ylim = c(0, max(n_ind_pop, na.rm = TRUE)), xlab = "Years", ylab = "Nº ind")
  abline(v = snake_intro, col = "red", lty = "dashed")
  lines(1:n_generations, n_ind_pop, type = "l")
  
  return(list(population_data = lizard_pop, n_individuals = n_ind_pop))
  
}


lizard_projection(initial_pop = 156, 
                  n_generations = 20, 
                  cat_pred = 0.1, 
                  snake_pred = 0.5, 
                  snake_intro = 20, 
                  k = 312, 
                  urban_index = 0.5,
                  emigration_perc = 0)



# Part 4: final plots

lizard_projection <- function(initial_pop = 100, n_generations = 10, cat_pred = 0.10, snake_pred = 0.20, snake_intro = 10, k = 312, urban_index = 1, emigration_perc = 0.1) {
  
  # Initial population and array
  n_lizards <- initial_pop
  lizard_pop <- array(data = NA, dim = c(999999, 4, n_generations))
  dimnames(lizard_pop)[[2]] <- c("ID", "Age", "Sex", "Status")
  
  # First individuals
  for (i in 1:n_lizards) {
    lizard_pop[i, 1, 1] <- i
    lizard_pop[i, 2, 1] <- as.numeric(trunc(abs(rnorm(1, mean = 9, sd = 4))))
    lizard_pop[i, 3, 1] <- sample(c("Male", "Female"), 1)
    lizard_pop[i, 4, 1] <- 1
  }
  
  # Time simulation
  for (i in 2:n_generations) {
    
    # Keep tracking of number of rows filled
    last_ind <- sum(complete.cases(lizard_pop[, 1, i - 1]))
    
    # Select previous generation
    lizards_generation <- as.data.frame(lizard_pop[, , i - 1])
    
    # Select only alive individuals
    lizards_alive <- subset(lizards_generation, as.numeric(Status) == 1)
    
    # Cat mortality
    if (nrow(lizards_alive) > 0) {
      dead_ind_cat <- sample(1:nrow(lizards_alive), trunc(nrow(lizards_alive) * cat_pred), replace = FALSE)
      dead_ids_cat <- lizards_alive[dead_ind_cat, "ID"]
      for (id in dead_ids_cat) {
        idx <- which(lizard_pop[, 1, i - 1] == id)
        lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
        lizard_pop[idx, 4, i] <- 0
      }
    }
    
    # Select individuals still alive after cat mortality
    lizards_alive_after_cats <- subset(as.data.frame(lizard_pop[, , i - 1]), as.numeric(Status) == 1 & is.na(lizard_pop[, 4, i]))
    
    # Snake mortality (starting from snake_intro)
    # Full urbanisation counters snake effect
    if (i >= (snake_intro + 1)) {
      if (nrow(lizards_alive_after_cats) > 0) {
        dead_ind_snake <- sample(1:nrow(lizards_alive_after_cats), trunc(nrow(lizards_alive_after_cats) * (snake_pred - (urban_index * snake_pred))), replace = FALSE)
        dead_ids_snake <- lizards_alive_after_cats[dead_ind_snake, "ID"]
        for (id in dead_ids_snake) {
          idx <- which(lizard_pop[, 1, i - 1] == id)
          lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
          lizard_pop[idx, 4, i] <- 0
        }
      }
    }
    
    # Select individuals still alive after snake mortality
    lizards_alive_after_snakes <- subset(as.data.frame(lizard_pop[, , i - 1]), as.numeric(Status) == 1 & is.na(lizard_pop[, 4, i]))
    
    # Denso dependency
    n_alive <- nrow(lizards_alive_after_snakes)
    
    if (n_alive > k) {
      surplus <- n_alive - k
      surplus_ind <- sample(1:nrow(lizards_alive_after_snakes), surplus, replace = FALSE)
      surplus_ids <- lizards_alive_after_snakes[surplus_ind, "ID"]
      for (id in surplus_ids) {
        idx <- which(lizard_pop[, 1, i - 1] == id)
        lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
        lizard_pop[idx, 4, i] <- 0
      }
    }
    
    # The rest of individuals
    for (j in 1:last_ind) {
      
      # If there is an NA, jump to next loop
      if (is.na(lizard_pop[j, 4, i - 1])) {
        next
      }
      
      # If the individual is already dead in this generation (external death causes), jump to the next loop
      if (!is.na(lizard_pop[j, 4, i]) && as.numeric(lizard_pop[j, 4, i]) == 0) {
        next
      }
      
      # If lizard was dead in the previous generation, mark it as dead
      if (as.numeric(lizard_pop[j, 4, i - 1]) == 0) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        next
      }
      
      # If the lizard is alive, but it's older than 18 years old, it dies
      if (as.numeric(lizard_pop[j, 2, i - 1]) == 18) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        lizard_pop[j, 4, i] <- 0
        next
      }
      
      # Survival probability
      surv_prob <- runif(1, 0.75, 0.83)
      surv_random <- runif(1, 0, 1)
      
      # If it fails, the lizard is marked as dead
      if (surv_random > surv_prob) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        lizard_pop[j, 4, i] <- 0
        
      } else {
        # If the lizard survives, it gets older and survives
        lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
        lizard_pop[j, 2, i] <- as.numeric(lizard_pop[j, 2, i - 1]) + 1
        lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
        lizard_pop[j, 4, i] <- 1
      }
      
      # Reproduction
      # If the individual is a female, older than 2 years old and alive
      if (lizard_pop[j, 3, i - 1] == "Female" && as.numeric(lizard_pop[j, 4, i - 1]) == 1 && as.numeric(lizard_pop[j, 2, i - 1]) >= 2) {
        
        if (as.numeric(lizard_pop[j, 2, i - 1]) > (18 - 5)) {
          # If it's an old female, it lays eggs two times
          
          for (p in 1:2) {
            
            # Number of eggs laid
            n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
            
            if (n_eggs > 0) {
              
              for (m in 1:n_eggs) {
                
                # Hatching probability
                hatch_prob <- runif(1, 0.60, 0.75)
                hatch_random <- runif(1, 0, 1)
                
                # If the egg hatches
                if (hatch_random < hatch_prob) {
                  
                  # Add a new row
                  last_ind <- last_ind + 1
                  lizard_pop[last_ind, 1, i] <- last_ind
                  lizard_pop[last_ind, 2, i] <- 0
                  lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                  lizard_pop[last_ind, 4, i] <- 1
                }
              }
            }
          }
          
        } else {
          
          # If the female is young, only one laying
          n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
          
          if (n_eggs > 0) {
            
            for (m in 1:n_eggs) {
              
              hatch_prob <- runif(1, 0.60, 0.75)
              hatch_random <- runif(1, 0, 1)
              
              if (hatch_random < hatch_prob) {
                
                last_ind <- last_ind + 1
                lizard_pop[last_ind, 1, i] <- last_ind
                lizard_pop[last_ind, 2, i] <- 0
                lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                lizard_pop[last_ind, 4, i] <- 1
              }
            }
          }
        }
      }
    }
    
    # Emigration
    emigration <- trunc(k * emigration_perc)
    
    for (e in 1:emigration) {
      last_ind <- last_ind + 1
      lizard_pop[last_ind, 1, i] <- last_ind
      lizard_pop[last_ind, 2, i] <- as.numeric(trunc(abs(rnorm(1, mean = 9, sd = 4))))
      lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
      lizard_pop[last_ind, 4, i] <- 1
    }
    
    assign("lizard_pop", lizard_pop, envir=globalenv())
    
  }
  
  # Track nº of individuals per generation
  n_ind_pop <- as.data.frame(matrix(data = NA, ncol = n_generations, nrow = 1))
  for (i in 1:n_generations) {
    n_ind_pop[i] <- sum(lizard_pop[, 4, i] == 1, na.rm = TRUE)
  }
  
  colnames(n_ind_pop) <- paste0("Generation_", 1:n_generations)
  
  assign("n_ind_pop", n_ind_pop, envir=globalenv())
  
  # plot nº of individuals per generation
  plot(NULL, xlim = c(1, n_generations), ylim = c(0, max(n_ind_pop, na.rm = TRUE)), xlab = "Years", ylab = "Nº ind")
  abline(v = snake_intro, col = "red", lty = "dashed")
  lines(1:n_generations, n_ind_pop, type = "l")
  
  return(list(population_data = lizard_pop, n_individuals = n_ind_pop))
  
}

# same funciton, but without drawing the plot at the end of the simulation
lizard_projection_no_plot <- function(initial_pop = 100, n_generations = 10, cat_pred = 0.10, snake_pred = 0.20, snake_intro = 10, k = 312, urban_index = 1, emigration_perc = 0.1) {
  
  # Initial population and array
  n_lizards <- initial_pop
  lizard_pop <- array(data = NA, dim = c(999999, 4, n_generations))
  dimnames(lizard_pop)[[2]] <- c("ID", "Age", "Sex", "Status")
  
  # First individuals
  for (i in 1:n_lizards) {
    lizard_pop[i, 1, 1] <- i
    lizard_pop[i, 2, 1] <- as.numeric(trunc(abs(rnorm(1, mean = 9, sd = 4))))
    lizard_pop[i, 3, 1] <- sample(c("Male", "Female"), 1)
    lizard_pop[i, 4, 1] <- 1
  }
  
  # Time simulation
  for (i in 2:n_generations) {
    
    # Keep tracking of number of rows filled
    last_ind <- sum(complete.cases(lizard_pop[, 1, i - 1]))
    
    # Select previous generation
    lizards_generation <- as.data.frame(lizard_pop[, , i - 1])
    
    # Select only alive individuals
    lizards_alive <- subset(lizards_generation, as.numeric(Status) == 1)
    
    # Cat mortality
    if (nrow(lizards_alive) > 0) {
      dead_ind_cat <- sample(1:nrow(lizards_alive), trunc(nrow(lizards_alive) * cat_pred), replace = FALSE)
      dead_ids_cat <- lizards_alive[dead_ind_cat, "ID"]
      for (id in dead_ids_cat) {
        idx <- which(lizard_pop[, 1, i - 1] == id)
        lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
        lizard_pop[idx, 4, i] <- 0
      }
    }
    
    # Select individuals still alive after cat mortality
    lizards_alive_after_cats <- subset(as.data.frame(lizard_pop[, , i - 1]), as.numeric(Status) == 1 & is.na(lizard_pop[, 4, i]))
    
    # Snake mortality (starting from snake_intro)
    # Full urbanisation counters snake effect
    if (i >= (snake_intro + 1)) {
      if (nrow(lizards_alive_after_cats) > 0) {
        dead_ind_snake <- sample(1:nrow(lizards_alive_after_cats), trunc(nrow(lizards_alive_after_cats) * (snake_pred - (urban_index * snake_pred))), replace = FALSE)
        dead_ids_snake <- lizards_alive_after_cats[dead_ind_snake, "ID"]
        for (id in dead_ids_snake) {
          idx <- which(lizard_pop[, 1, i - 1] == id)
          lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
          lizard_pop[idx, 4, i] <- 0
        }
      }
    }
    
    # Select individuals still alive after snake mortality
    lizards_alive_after_snakes <- subset(as.data.frame(lizard_pop[, , i - 1]), as.numeric(Status) == 1 & is.na(lizard_pop[, 4, i]))
    
    # Denso dependency
    n_alive <- nrow(lizards_alive_after_snakes)
    
    if (n_alive > k) {
      surplus <- n_alive - k
      surplus_ind <- sample(1:nrow(lizards_alive_after_snakes), surplus, replace = FALSE)
      surplus_ids <- lizards_alive_after_snakes[surplus_ind, "ID"]
      for (id in surplus_ids) {
        idx <- which(lizard_pop[, 1, i - 1] == id)
        lizard_pop[idx, , i] <- lizard_pop[idx, , i - 1]
        lizard_pop[idx, 4, i] <- 0
      }
    }
    
    # The rest of individuals
    for (j in 1:last_ind) {
      
      # If there is an NA, jump to next loop
      if (is.na(lizard_pop[j, 4, i - 1])) {
        next
      }
      
      # If the individual is already dead in this generation (external death causes), jump to the next loop
      if (!is.na(lizard_pop[j, 4, i]) && as.numeric(lizard_pop[j, 4, i]) == 0) {
        next
      }
      
      # If lizard was dead in the previous generation, mark it as dead
      if (as.numeric(lizard_pop[j, 4, i - 1]) == 0) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        next
      }
      
      # If the lizard is alive, but it's older than 18 years old, it dies
      if (as.numeric(lizard_pop[j, 2, i - 1]) == 18) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        lizard_pop[j, 4, i] <- 0
        next
      }
      
      # Survival probability
      surv_prob <- runif(1, 0.75, 0.83)
      surv_random <- runif(1, 0, 1)
      
      # If it fails, the lizard is marked as dead
      if (surv_random > surv_prob) {
        lizard_pop[j, , i] <- lizard_pop[j, , i - 1]
        lizard_pop[j, 4, i] <- 0
        
      } else {
        # If the lizard survives, it gets older and survives
        lizard_pop[j, 1, i] <- lizard_pop[j, 1, i - 1]
        lizard_pop[j, 2, i] <- as.numeric(lizard_pop[j, 2, i - 1]) + 1
        lizard_pop[j, 3, i] <- lizard_pop[j, 3, i - 1]
        lizard_pop[j, 4, i] <- 1
      }
      
      # Reproduction
      # If the individual is a female, older than 2 years old and alive
      if (lizard_pop[j, 3, i - 1] == "Female" && as.numeric(lizard_pop[j, 4, i - 1]) == 1 && as.numeric(lizard_pop[j, 2, i - 1]) >= 2) {
        
        if (as.numeric(lizard_pop[j, 2, i - 1]) > (18 - 5)) {
          # If it's an old female, it lays eggs two times
          
          for (p in 1:2) {
            
            # Number of eggs laid
            n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
            
            if (n_eggs > 0) {
              
              for (m in 1:n_eggs) {
                
                # Hatching probability
                hatch_prob <- runif(1, 0.60, 0.75)
                hatch_random <- runif(1, 0, 1)
                
                # If the egg hatches
                if (hatch_random < hatch_prob) {
                  
                  # Add a new row
                  last_ind <- last_ind + 1
                  lizard_pop[last_ind, 1, i] <- last_ind
                  lizard_pop[last_ind, 2, i] <- 0
                  lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                  lizard_pop[last_ind, 4, i] <- 1
                }
              }
            }
          }
          
        } else {
          
          # If the female is young, only one laying
          n_eggs <- trunc(abs(rnorm(1, 2.30, 0.55)))
          
          if (n_eggs > 0) {
            
            for (m in 1:n_eggs) {
              
              hatch_prob <- runif(1, 0.60, 0.75)
              hatch_random <- runif(1, 0, 1)
              
              if (hatch_random < hatch_prob) {
                
                last_ind <- last_ind + 1
                lizard_pop[last_ind, 1, i] <- last_ind
                lizard_pop[last_ind, 2, i] <- 0
                lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
                lizard_pop[last_ind, 4, i] <- 1
              }
            }
          }
        }
      }
    }
    
    # Emigration
    emigration <- trunc(k * emigration_perc)
    
    for (e in 1:emigration) {
      last_ind <- last_ind + 1
      lizard_pop[last_ind, 1, i] <- last_ind
      lizard_pop[last_ind, 2, i] <- as.numeric(trunc(abs(rnorm(1, mean = 9, sd = 4))))
      lizard_pop[last_ind, 3, i] <- sample(c("Male", "Female"), 1)
      lizard_pop[last_ind, 4, i] <- 1
    }
    
    assign("lizard_pop", lizard_pop, envir=globalenv())
    
  }
  
  # Track nº of individuals per generation
  n_ind_pop <- as.data.frame(matrix(data = NA, ncol = n_generations, nrow = 1))
  for (i in 1:n_generations) {
    n_ind_pop[i] <- sum(lizard_pop[, 4, i] == 1, na.rm = TRUE)
  }
  
  colnames(n_ind_pop) <- paste0("Generation_", 1:n_generations)
  
  assign("n_ind_pop", n_ind_pop, envir=globalenv())
  
}



lizard_multiplot <- function(n_rep = 20,
                             initial_pop = 100, 
                             n_generations = 10, 
                             cat_pred = 0.10,
                             snake_pred = 0.50, 
                             snake_intro = 5, 
                             k = 312, 
                             urban_index = 1, 
                             emigration_perc = 0.1){
  
  
  # create new data frame, which is going to store the population numbers for each simulation, and the mean
  all_simulations <- as.data.frame(matrix(NA, nrow = n_rep + 1, ncol = n_generations))
  
  for(x in 1:n_rep){
    
    # run the simulation x times
    lizard_projection_no_plot(initial_pop = initial_pop, 
                      n_generations = n_generations, 
                      cat_pred = cat_pred, 
                      snake_pred = snake_pred, 
                      snake_intro = snake_intro, 
                      k = k, 
                      urban_index = urban_index,
                      emigration_perc = emigration_perc)
    
    # store population number in correspondant row
    all_simulations[x, ] <- n_ind_pop
    
  }
  
  # mean population number per generation across several simulations
  all_simulations[n_rep + 1, ] <- colMeans(all_simulations[1:n_rep, ])
  
  assign("all_simulations", all_simulations, envir=globalenv())
  
  
  # plot nº of individuals per generation
  plot(NULL, xlim = c(1, n_generations), ylim = c(0, max(all_simulations, na.rm = TRUE)), xlab = "Years", ylab = "Nº ind", xaxt = "n")
  axis(1, at = 1:n_generations, labels = (n_generations - 1):0)
  
  for(x in 1:n_rep){
    
    lines(1:n_generations, all_simulations[x, ], type = "l", col = "grey", lwd = 1)
    
  }
  
  lines(1:n_generations, all_simulations[n_rep + 1, ], type = "l", col = "black", lwd = 3)
  
  abline(v = snake_intro, col = "red", lty = "dashed")
  
  
}


lizard_multiplot(n_rep = 5,
                 initial_pop = 100, 
                 n_generations = 10, 
                 cat_pred = 0.10,
                 snake_pred = 0.50, 
                 snake_intro = 5, 
                 k = 312, 
                 emigration = 30, 
                 urban_index = 1)


# Peri-urban setting with snake introduction
lizard_projection(initial_pop = 156, 
                  n_generations = 20, 
                  cat_pred = 0.10,
                  snake_pred = 0.50, 
                  snake_intro = 10, 
                  k = 312, 
                  urban_index = 0, 
                  emigration_perc = 0)

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 50% carrying capacity
                 n_generations = 20, # number of generations the simulation is going to last
                 cat_pred = 0.10, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 10, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 0, # Urbanisation index
                 emigration_perc = 0) # % of individuals of the carrying capacity that comes towards this habitat


# Peri-urban setting without snake introduction
lizard_projection(initial_pop = 156, 
                  n_generations = 20, 
                  cat_pred = 0.10,
                  snake_pred = 0.50, 
                  snake_intro = 30, 
                  k = 312, 
                  urban_index = 0, 
                  emigration_perc = 0)

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 75% carrying capacity
                 n_generations = 20, # number of generations the simulation is going to last
                 cat_pred = 0.10, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 30, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 0, # Urbanisation index
                 emigration_perc = 0) # % of individuals of the carrying capacity that comes towards this habitat



# Urban setting without snake and without emigration
lizard_projection(initial_pop = 156, 
                  n_generations = 40, 
                  cat_pred = 0.20, # cat preassure is the double as in peri-urban settings
                  snake_pred = 0.50, 
                  snake_intro = 50, 
                  k = 312, 
                  urban_index = 1, 
                  emigration_perc = 0)

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 50% carrying capacity
                 n_generations = 40, # number of generations the simulation is going to last
                 cat_pred = 0.20, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 50, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 1, # Urbanisation index
                 emigration_perc = 0) # % of individuals of the carrying capacity that comes towards this habitat


# Urban setting without snake with emigration
lizard_projection(initial_pop = 156, 
                  n_generations = 40, 
                  cat_pred = 0.20, # cat preassure is the double as in peri-urban settings
                  snake_pred = 0.50, 
                  snake_intro = 50, 
                  k = 312, 
                  urban_index = 1, 
                  emigration_perc = 0.05) # emigration is 5% of total carrying capacity

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 50% carrying capacity
                 n_generations = 40, # number of generations the simulation is going to last
                 cat_pred = 0.20, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 50, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 1, # Urbanisation index
                 emigration_perc = 0.05) # % of individuals of the carrying capacity that comes towards this habitat


# Urban setting with snake with emigration, no urban protection
lizard_projection(initial_pop = 156, 
                  n_generations = 40, 
                  cat_pred = 0.20, # cat preassure is the double as in peri-urban settings
                  snake_pred = 0.50, 
                  snake_intro = 20, 
                  k = 312, 
                  urban_index = 0, 
                  emigration_perc = 0.05) # emigration is 5% of total carrying capacity

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 50% carrying capacity
                 n_generations = 40, # number of generations the simulation is going to last
                 cat_pred = 0.20, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 20, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 0, # Urbanisation index
                 emigration_perc = 0.05) # % of individuals of the carrying capacity that comes towards this habitat


# Urban setting with snake with emigration, no urban protection
lizard_projection(initial_pop = 156, 
                  n_generations = 40, 
                  cat_pred = 0.20, # cat preassure is the double as in peri-urban settings
                  snake_pred = 0.50, 
                  snake_intro = 20, 
                  k = 312, 
                  urban_index = 0, 
                  emigration_perc = 0.05) # emigration is 5% of total carrying capacity

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 50% carrying capacity
                 n_generations = 40, # number of generations the simulation is going to last
                 cat_pred = 0.20, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 20, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 0, # Urbanisation index
                 emigration_perc = 0.05) # % of individuals of the carrying capacity that comes towards this habitat


# Urban setting with snake without emigration, no urban protection
lizard_projection(initial_pop = 156, 
                  n_generations = 40, 
                  cat_pred = 0.20, # cat preassure is the double as in peri-urban settings
                  snake_pred = 0.50, 
                  snake_intro = 20, 
                  k = 312, 
                  urban_index = 0, 
                  emigration_perc = 0) # emigration is 5% of total carrying capacity

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 50% carrying capacity
                 n_generations = 40, # number of generations the simulation is going to last
                 cat_pred = 0.20, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 20, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 0, # Urbanisation index
                 emigration_perc = 0) # % of individuals of the carrying capacity that comes towards this habitat


# Urban setting with snake without emigration, no urban protection
lizard_projection(initial_pop = 156, 
                  n_generations = 40, 
                  cat_pred = 0.20, # cat preassure is the double as in peri-urban settings
                  snake_pred = 0.50, 
                  snake_intro = 20, 
                  k = 312, 
                  urban_index = 1, 
                  emigration_perc = 0) # emigration is 5% of total carrying capacity

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 50% carrying capacity
                 n_generations = 40, # number of generations the simulation is going to last
                 cat_pred = 0.20, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 20, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 1, # Urbanisation index
                 emigration_perc = 0) # % of individuals of the carrying capacity that comes towards this habitat


# Urban setting with snake without emigration, no urban protection
lizard_projection(initial_pop = 156, 
                  n_generations = 40, 
                  cat_pred = 0.20, # cat preassure is the double as in peri-urban settings
                  snake_pred = 0.50, 
                  snake_intro = 20, 
                  k = 312, 
                  urban_index = 1, 
                  emigration_perc = 0.05) # emigration is 5% of total carrying capacity

lizard_multiplot(n_rep = 10, # number of replicates
                 initial_pop = 156, # 50% carrying capacity
                 n_generations = 40, # number of generations the simulation is going to last
                 cat_pred = 0.20, # % of the total population that cats kill every year
                 snake_pred = 0.50, # % of the total population that snakes kill every year
                 snake_intro = 20, # Year from the present that the snake has been introduced
                 k = 312, # Carrying capacity
                 urban_index = 1, # Urbanisation index
                 emigration_perc = 0.05) # % of individuals of the carrying capacity that comes towards this habitat

