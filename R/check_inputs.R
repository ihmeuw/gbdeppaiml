check_inputs <- function(dt_obj){
  specfp <- attr(dt_obj, 'specfp')
  names <- names(specfp)
  copied <- c()
  for(name in names){
    dim_num <- dim(specfp[[name]])
    if(!any(dim_num == specfp[['SIM_YEARS']])){
      next
    }else{
      if(which(dim_num == specfp[['SIM_YEARS']]) == 2){
        stop_year <- specfp[[name]][,specfp[['SIM_YEARS']]]
        penul_year <- specfp[[name]][,specfp[['SIM_YEARS']] - 1]
        if(stop_year == penul_year){
          copied <- append(copied, name)
          next
        }
      }
      
      
      if(which(dim_num == specfp[['SIM_YEARS']]) == 3){
        stop_year <- specfp[[name]][,,specfp[['SIM_YEARS']]]
        penul_year <- specfp[[name]][,,specfp[['SIM_YEARS']] - 1]
        if(any(stop_year == penul_year)){
          copied <- append(copied, name)
          next
        }
      }
      
      if(which(dim_num == specfp[['SIM_YEARS']]) == 4){
        stop_year <- specfp[[name]][,,,specfp[['SIM_YEARS']]]
        penul_year <- specfp[[name]][,,,specfp[['SIM_YEARS']] - 1]
        if(any(stop_year == penul_year)){
          copied <- append(copied, name)
          next
        }
      }
      
      
    }
  }

  return(copied)
  
}



undebug(check_inputs)
check_inputs(dt)

