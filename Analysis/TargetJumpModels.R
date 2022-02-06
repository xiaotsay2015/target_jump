call_tj_models <- function(){
  
  ##### FIT TOGETHER #######
  
  SPEinvariantNoTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_intercept, my_slope, all_data_show){
    
    # my_intercept & my_slope dummy var
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- my_spe_adapt[ci]
      my_tgteffect$TE[idx] <- 0
      my_tgteffect$DeltaHand[idx] <-  my_spe_adapt[ci]
      
      # target jump = 1 is jump in place 
      #my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 0]
    
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
    
  }
  
  SPEinvariantLinearTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_intercept, my_slope, all_data_show){
    
    # my_intercept dummy var
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- my_spe_adapt[ci]
      my_tgteffect$TE[idx] <-  (my_jump_dist  + my_clamp_size[ci]) * my_slope
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # target jump = 1 is jump in place 
      #my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 0]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  
  SPERewardNoTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_reward_amp, my_sd, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      # note clamp_size is positive, so that's why its + my clampsize (two negatives)
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx]  <- my_spe_adapt[ci] - my_reward_amp * exp( - (my_jump_dist + my_clamp_size[ci])^2 / (2 * my_sd ^2)  ) 
      my_tgteffect$TE[idx]  <- 0
      my_tgteffect$DeltaHand[idx]  <- my_tgteffect$SPEonly[idx]  + my_tgteffect$TE[idx] 
      
      # target jump = 1 is jump in place 
      #my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 0]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  SPERewardLinearTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_reward_amp, my_slope, my_sd, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      # note clamp_size is positive, so that's why its + my clampsize (two negatives)
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- my_spe_adapt[ci] - my_reward_amp * exp( - (my_jump_dist + my_clamp_size[ci])^2 / (2 * my_sd ^2)  ) 
      my_tgteffect$TE[idx] <-  (my_jump_dist  + my_clamp_size[ci]) * my_slope
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # target jump = 1 is jump in place 
      #my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 0]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  
  SPEdisruptNoTe <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_sd, my_jump_cost, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- (my_spe_adapt[ci] - my_jump_cost ) * exp( - (my_jump_dist - 0 )^2 / (2 * my_sd^2)  )
      my_tgteffect$TE[idx] <- 0
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # coded target jump = 1 as jump in place, and jump = as no jump 
      #my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0]
      my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0] <- my_spe_adapt[ci]
      
      
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
    
  }
  
  SPEdisruptTElinear <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_sd, my_slope, my_jump_cost, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- (my_jump_cost * my_spe_adapt[ci] - my_clamp_size[ci] * my_slope) * exp( - (my_jump_dist - 0 )^2 / (2 * my_sd^2)  )
      my_tgteffect$TE[idx] <- (my_jump_dist  + my_clamp_size[ci]) * my_slope
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # coded target jump = 1 as jump in place, and jump = as no jump 
      #my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0]
      my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0] <- my_spe_adapt[ci]
      
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  
  SPEinvariantSinTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_slope, my_sd, all_data_show){
    
    # my_slope dummy var
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- my_spe_adapt[ci]
      my_tgteffect$TE[idx] <- ((my_jump_dist  + my_clamp_size[ci]) * my_slope) * exp( - (my_jump_dist)^2 / (2 * my_sd^2)  )
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # target jump = 1 is jump in place 
      #my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 0]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  
  SPERewardSinTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_reward_amp, my_slope, my_sd, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      # note clamp_size is positive, so that's why its + my clampsize (two negatives)
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- my_spe_adapt[ci] - my_reward_amp * exp( - (my_jump_dist + my_clamp_size[ci])^2 / (2 * my_sd ^2)  ) 
      my_tgteffect$TE[idx] <- ( (my_jump_dist  + my_clamp_size[ci]) * my_slope ) * exp( - (my_jump_dist)^2 / (2 * my_sd^2)  ) 
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # target jump = 1 is jump in place 
      #my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 0]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  SPEdisruptSinTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_sd, my_slope, my_jump_cost, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      my_intercept <- ((my_clamp_size[ci]) * my_slope ) * exp( - (0)^2 / (2 * my_sd^2)  ) 
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- ( my_jump_cost * my_spe_adapt[ci] - my_intercept) * exp( - (my_jump_dist - 0 )^2 / (2 * my_sd^2)  )
      my_tgteffect$TE[idx] <- ((my_jump_dist  + my_clamp_size[ci]) * my_slope ) * exp( - (my_jump_dist)^2 / (2 * my_sd^2)  ) 
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # coded target jump = 1 as jump in place, and jump = as no jump 
      #my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0]
      my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0] <- my_spe_adapt[ci]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  
  SPEinvariantSatTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_intercept, my_slope, my_sat, all_data_show){
    
    # my_intercept dummy var
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- my_spe_adapt[ci]
      my_tgteffect$TE[idx] <-  (my_jump_dist  + my_clamp_size[ci]) * my_slope
      my_tgteffect$TE[my_tgteffect$JumpDistance >= (my_sat  - my_clamp_size ) ] <- my_clamp_size[ci] * my_slope + my_slope * (my_sat  - my_clamp_size[ci] )
      my_tgteffect$TE[my_tgteffect$JumpDistance <= (-my_sat - my_clamp_size ) ] <- my_clamp_size[ci] * my_slope + my_slope * (-my_sat - my_clamp_size[ci] )
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # target jump = 1 is jump in place 
      #my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 0]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  SPERewardSatTE <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_reward_amp, my_slope, my_sd, my_sat, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      # note clamp_size is positive, so that's why its + my clampsize (two negatives)
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- my_spe_adapt[ci] - my_reward_amp * exp( - (my_jump_dist + my_clamp_size[ci])^2 / (2 * my_sd ^2)  ) 
      my_tgteffect$TE[idx] <-  (my_jump_dist  + my_clamp_size[ci]) * my_slope
      my_tgteffect$TE[my_tgteffect$JumpDistance >= (my_sat  - my_clamp_size ) ] <- my_clamp_size[ci] * my_slope + my_slope * (my_sat  - my_clamp_size[ci] )
      my_tgteffect$TE[my_tgteffect$JumpDistance <= (-my_sat - my_clamp_size ) ] <- my_clamp_size[ci] * my_slope + my_slope * (-my_sat - my_clamp_size[ci] )
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # target jump = 1 is jump in place 
      #my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[my_tgteffect$JumpDistance == 0]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  SPEdisruptTESat <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_sd, my_slope, my_jump_cost, my_sat, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- (my_jump_cost * my_spe_adapt[ci] - my_clamp_size[ci] * my_slope) * exp( - (my_jump_dist - 0 )^2 / (2 * my_sd^2)  )
      my_tgteffect$TE[idx] <- (my_jump_dist  + my_clamp_size[ci]) * my_slope
      my_tgteffect$TE[my_tgteffect$JumpDistance >= (my_sat  - my_clamp_size ) ] <- my_clamp_size[ci] * my_slope + my_slope * ( my_sat  - my_clamp_size[ci] )
      my_tgteffect$TE[my_tgteffect$JumpDistance <= (-my_sat - my_clamp_size ) ] <- my_clamp_size[ci] * my_slope + my_slope * (-my_sat - my_clamp_size[ci] )
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # coded target jump = 1 as jump in place, and jump = as no jump 
      #my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0]
      my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0] <- my_spe_adapt[ci]
      
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  signaldepnoise <- function(my_clamp_size, my_jump_dist, te_scale, spe_scale, prior_sd, all_data_show){
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      spe_sd <- abs ( my_clamp_size[ci] ) * spe_scale
      te_sd <- abs( ( my_jump_dist + my_clamp_size[ci]  )) * te_scale
      my_tgteffect$SPEonly[idx] <- my_clamp_size[ci] * prior_sd^2 / (prior_sd^2 + spe_sd^2) 
      my_tgteffect$TE[idx] <- (my_jump_dist + my_clamp_size[ci]) * prior_sd^2 / (te_sd^2 + prior_sd^2)
      my_tgteffect$DeltaHand[idx] <- ( my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx] ) 
      
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
    
  }
  
  ##### FIT SEPARATELY #######
  
  SPEdisruptTElinear_sep <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_sd, my_slope, my_jump_cost, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- ( my_jump_cost * my_spe_adapt[ci] - my_clamp_size[ci] * my_slope[ci]) * exp( - (my_jump_dist - 0 )^2 / (2 * (my_sd[ci] )^2)  )
      my_tgteffect$TE[idx] <- (my_jump_dist  + my_clamp_size[ci]) * my_slope[ci] 
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # coded target jump = 1 as jump in place, and jump = as no jump 
      # my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0]
      my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0] <- my_spe_adapt[ci]
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
  }
  
  SPEdisruptNoTe_sep <- function(my_clamp_size, my_jump_dist, my_spe_adapt, my_sd, my_jump_cost, all_data_show){
    
    my_columns <- c('clamp_size','JumpDistance', 'DeltaHand', 'TE', 'SPEonly')
    my_tgteffect <- setNames ( data.frame(matrix(NaN, ncol = length(my_columns), nrow = length(my_jump_dist) * length(my_clamp_size) )), my_columns)
    my_tgteffect$clamp_size <- rep( my_clamp_size, each = length(my_jump_dist) )
    my_tgteffect$JumpDistance <- rep( my_jump_dist, length(my_clamp_size) )
    
    for(ci in  1:length(my_clamp_size) ){
      idx <- my_tgteffect$clamp_size == my_clamp_size[ci]
      my_tgteffect$SPEonly[idx] <- ( my_jump_cost * my_spe_adapt[ci] ) * exp( - (my_jump_dist - 0 )^2 / (2 * my_sd[ci]^2)  )
      my_tgteffect$TE[idx] <- 0
      my_tgteffect$DeltaHand[idx] <- my_tgteffect$SPEonly[idx] + my_tgteffect$TE[idx]
      
      # coded target jump = 1 as jump in place, and jump = as no jump 
      #my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 1] <- my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0]
      my_tgteffect$DeltaHand[idx & my_tgteffect$JumpDistance == 0] <- my_spe_adapt[ci]
      
      
    }
    
    if(all_data_show){
      return(my_tgteffect)
    }else{
      return(my_tgteffect$DeltaHand)
    }
    
  }
  
  
  

  return(list(SPEinvariantNoTE, SPEinvariantLinearTE, SPEinvariantSinTE, SPEinvariantSatTE,
              SPERewardNoTE, SPERewardLinearTE, SPERewardSinTE, SPERewardSatTE, 
              SPEdisruptNoTe, SPEdisruptTElinear, SPEdisruptSinTE, SPEdisruptTESat, 
              SPEinvariantNoTE, SPEinvariantLinearTE, SPEinvariantSinTE, SPEinvariantSatTE, 
              signaldepnoise)) # SPEdisruptTElinear_sep, SPEdisruptNoTe_sep
}

