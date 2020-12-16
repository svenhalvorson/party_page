library('tidyverse')
# let's just see if we can do part 2 with a recursive solution for kicks
bags_list = readLines('c:/Users/Sven/Documents/py_files/aoc_2020/inputs/d7.txt')

# Pick out the parents and children:
parents = str_split_fixed(
  bags_list,
  pattern = ' bags contain ',
  n = 2
)[,1]

children = str_split_fixed(
  bags_list,
  pattern = ' bags contain ',
  n = 2
)[,2]

# make the children into lists of names and values
split_children = function(x){
  
  if(str_detect(x[1], 'no other')){
    list(
      colors = NULL,
      n = NULL
    )
  } else{
    list(
      colors = str_remove_all(x, '[0-9]+ | bags?|\\.'),
      n = as.numeric(str_extract(x, '[0-9]+'))
    )
  }
}

parent_child = children %>% 
  lapply(str_split, pattern = ', ', simplify = TRUE) %>% 
  lapply(split_children)
names(parent_child) = parents

parent_child[1]
bags_list[1]
# so perhaps this was more data munging than needed but I
# find that these problems are quite difficult if I don't have 
# the data structured nicely.

# So now I want to write a function that can take in a color and find the
# count of children below it.
count_children = function(color, n){

  # Whatever color we called it on, get their children and counts:
  new_children = parent_child[[color]]$colors
  new_n = parent_child[[color]]$n
  
  # If we're at the leaf of the tree:
  if(length(new_children) < 1){
    return(n)
  } else{
    
    # otherwise, call the function 
    child_count = map2_dbl(
      .x = new_children,
      .y = new_n,
      .f = count_children
    )
    
    n + n*sum(child_count)
    
  }
  
}

# this should return 1 more than the total since the final
# line will count the gold bag itself:
count_children('shiny gold', 1)
