calculateFood = function(food_t0,pop_t0,pop_t1,elas,gdp_t0,gdp_t1,form){
  
  if(form == 0){
    # linear
    # result = (pop_t1/pop_t0) *   food_t0 + elas * (gdp_t1/gdp_t0)
    result = (pop_t1/pop_t0) * food_t0
    
  } else if(form == 1){
    # log-log
    # result = (pop_t1/pop_t0) *  food_t0 + exp(elas * log(gdp_t1/gdp_t0))
    result = (pop_t1/pop_t0) * food_t0 * exp(elas * log(gdp_t1/gdp_t0))
    
  } else if(form == 2){
    # semi-log
    # result = (pop_t1/pop_t0) *   food_t0 + elas * log(gdp_t1/gdp_t0)
    result = (pop_t1/pop_t0) * food_t0 * (1 + elas * log(gdp_t1/gdp_t0))
    
    
  } else{
    #log-inverse
    # result = (pop_t1/pop_t0) *  food_t0 + (exp(elas/(gdp_t1/gdp_t0)))
    result = (pop_t1/pop_t0) * food_t0 * (exp(elas*(1-1/(gdp_t1/gdp_t0))))
    
    }
  
  return(result)
}  


