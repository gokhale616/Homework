sapply(c("tidyverse", "parallel", "wrapr"), library, character.only = TRUE)


# create a grid of elements 

my_grid <- expand.grid(epsilon2 = seq(0, 1, by = 0.1), 
                       t_intro = seq(2001, 2006, by = 0.5), 
                       iota = seq(1, 100, by = 0.5)) 

    
# function takes in a counter - c calculates does the calculation in the mutate statements and returns 
# the resulting 1 row data frame

set.seed(986747881)
test_with_iota <- function(c, param_grid = my_grid) {
  
  res <- param_grid[c, ] %.>% 
    mutate(.,fake_res = (epsilon2 + t_intro) / rpois(1, iota))  
    
    return(res)
  
}


# apply's test_with_iota() rbinds all the results in a single dataframe.
res_df <- mclapply(1:nrow(my_grid), test_with_iota, mc.cores = 3) %.>% 
  do.call(rbind, .) 


# Number of NANs produced -generates the number of NAN's
res_df %.>% 
  mutate(., infinte_here = is.infinite(fake_res)) %.>% 
  select(infinte_here) %.>% 
  summary()











