
# Example input, if needed
# day20 <- readLines("./puzzle_input/input_day20_exp2.txt")

day20 <- readLines("./puzzle_input/input_day20.txt")

day20 <- strsplit(day20, " -> ")

switches <- sapply(day20, function(x) x[1])

outputs <- strsplit(sapply(day20, function(x) x[2]), ", ")

switchtypes <- 
  ifelse(switches == "broadcaster", "broadcaster", substr(switches, 1, 1))

switchnames <- 
  ifelse(switches == "broadcaster", "broadcaster", substr(switches, 2, nchar(switches)))

inputs <- vector("list", length(switchnames))

for (i in seq_along(switchnames)) {
  inputs[[i]] <- switchnames[sapply(outputs, function(x) switchnames[i] %in% x)]
}

switch_status <- vector("list", length(switchnames))

for (i in seq_along(switch_status)) {
  
  if (switchtypes[i] == "&") {
    switch_status[[i]] <- rep("low", length(inputs[[i]]))
    names(switch_status[[i]]) <- inputs[[i]]
  } else if (switchtypes[i] == "%") {
    switch_status[[i]] <- "off"
  } else {
    switch_status[[i]] <- "low"
  }
  
}

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

pulse_counter_low <- 0
pulse_counter_high <- 0
n_steps <- 1000

pb <- txtProgressBar(min = 0, max = n_steps, initial = 0, style = 3) 

for (i_step in 1:n_steps) {
  
  switch_queue <- "broadcaster"
  pulse_queue <- "low"
  
  while (length(switch_queue) != 0) {
    
    temp_out <- outputs[switchnames == switch_queue[1]][[1]]
    
    if (switch_queue[1] == "broadcaster") {
      
      switch_queue <- c(switch_queue[-1], temp_out)
      pulse_queue <- c(pulse_queue[-1], rep(pulse_queue[1], length(temp_out)))
      pulse_counter_low <- pulse_counter_low + 1 # button press
      
    } else if (switchtypes[switchnames == switch_queue[1]] == "%") {
      
      if (pulse_queue[1] == "low") {
        
        pulse_counter_low <- pulse_counter_low + 1
        
        temp_status <- switch_status[switchnames == switch_queue[1]][[1]]
        
        if (temp_status == "off") {
          
          switch_status[switchnames == switch_queue[1]][[1]] <- "on"
          
          # Are there & outputs that need to remember the last pulse?
          for (i in temp_out) {
            
            if (switchtypes[switchnames == i] == "&") {
              
              switch_status[switchnames == i][[1]][switch_queue[1]] <- "high"
              
            }
            
          }
          
          switch_queue <- c(switch_queue[-1], temp_out)
          pulse_queue <- c(pulse_queue[-1], rep("high", length(temp_out)))
          
        } else {
          
          switch_status[switchnames == switch_queue[1]][[1]] <- "off"
          
          # Are there & outputs that need to remember the last pulse?
          for (i in temp_out) {
            
            if (switchtypes[switchnames == i] == "&") {
              
              switch_status[switchnames == i][[1]][switch_queue[1]] <- "low"
              
            }
            
          }
          
          switch_queue <- c(switch_queue[-1], temp_out)
          pulse_queue <- c(pulse_queue[-1], rep("low", length(temp_out)))
          
        }
        
      } else {
        
        pulse_counter_high <- pulse_counter_high + 1
        switch_queue <- switch_queue[-1]
        pulse_queue <- pulse_queue[-1]
        
      }
      
    } else if (switchtypes[switchnames == switch_queue[1]] == "&") {
      
      if (pulse_queue[1] == "low") {
        
        pulse_counter_low <- pulse_counter_low + 1
        
      } else {
        
        pulse_counter_high <- pulse_counter_high + 1
        
      }
      
      if (all(switch_status[switchnames == switch_queue[1]][[1]] == "high")) {
        
        # Are there & outputs that need to remember the last pulse?
        for (i in temp_out) {
          
          if (length(switchtypes[switchnames == i]) == 0) {
            
            pulse_counter_low <- pulse_counter_low + 1
            temp_out <- temp_out[temp_out != i]
            
          } else {
            
            if (switchtypes[switchnames == i] == "&") {
              
              switch_status[switchnames == i][[1]][switch_queue[1]] <- "low"
              
            } 
            
          }
          
        }
        
        switch_queue <- c(switch_queue[-1], temp_out)
        pulse_queue <- c(pulse_queue[-1], rep("low", length(temp_out)))
        
      } else {
        
        for (i in temp_out) {
          
          if (length(switchtypes[switchnames == i]) == 0) {
            
            pulse_counter_high <- pulse_counter_high + 1
            temp_out <- temp_out[temp_out != i]
            
          } else {
            
            if (switchtypes[switchnames == i] == "&") {
              
              switch_status[switchnames == i][[1]][switch_queue[1]] <- "high"
              
            } 
            
          }
          
        }
        
        switch_queue <- c(switch_queue[-1], temp_out)
        pulse_queue <- c(pulse_queue[-1], rep("high", length(temp_out)))
        
      }
      
    }
    
  }
  
  setTxtProgressBar(pb, i_step)
  
}

close(pb)

pulse_counter_low * pulse_counter_high
# 712543680

Sys.time() - start
