
#replace age" and "age2 with poly(age, 2, raw = T) for estimation of polynomial terms in regression models

tp <- function(input_vector) {
  # Remove "age" and "age2" from the input vector
  modified_vector <- input_vector[!input_vector %in% c("age", "age2")]
  
  # Check if "age" or "age2" were present in the original vector
  if (any(c("age", "age2") %in% input_vector)) {
    # Add "I(age^2)" to the modified vector
    modified_vector <- c(modified_vector, "poly(age, 2, raw = TRUE)")
  }
  
  return(modified_vector)
}

