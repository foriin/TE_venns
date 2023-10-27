find_unique_elements <- function(lst) {
  lapply(seq_along(lst), function(i) {
    current_set <- lst[[i]]
    other_sets <- do.call(c, lst[-i])
    setdiff(current_set, other_sets)
  })
}
# Function to calculate all intersections
calculate_intersections <- function(sets) {
  n <- length(sets)
  if (n == 4){
    combs <- lapply(1:n, function(i) combn(1:n, i, simplify = FALSE))
    combs[[2]][[2]] <- c(2,3)
    combs[[2]][[3]] <- c(3,4)
    combs[[2]][[4]] <- c(1,3)
    combs[[2]][[5]] <- c(2,4)
    combs[[2]][[6]] <- c(1,4)
    combs[[3]][[2]] <- c(2,3,4)
    combs[[3]][[3]] <- c(1,3,4)
    combs[[3]][[4]] <- c(1,2,4)
  } else{
    combs <- lapply(1:n, function(i) combn(1:n, i, simplify = FALSE))
  }
  intersections <- lapply(combs, function(comb_set) {
    lapply(comb_set, function(indices) {
      Reduce(intersect, sets[indices])
    }) %>% find_unique_elements()
  })
  return(do.call(c, intersections))
}

# Function to generate hover texts
generate_hover_texts <- function(intersections, item_to_category) {
  lapply(intersections, function(intersect_set) {
    categories <- unique(item_to_category[intersect_set])
    paste(categories, collapse = "\n")
  })
}