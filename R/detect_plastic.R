# CASE_WHEN: for multiple conditions

# Contract: Classifies plastic types as PETE, PE, PVC, PP, PS, and OTHER based on the chemical composition of a sample of the plastic in question.

#' Title
#'
#' @param C
#' @param H
#' @param O
#' @param Cl
#'
#' @return
#' @export
#'
#' @examples
detect_plastic <- function(C, H, O, Cl) {

  # error checking
  C = ifelse(
    C < 0,
    return("Carbon content must be a value of 0 or greater than 0. Check for NA values"),
    C
  )

  H = ifelse(
    H < 0,
    return("Hydrogen content must be a value of 0 or greater than 0. Check for NA values"),
    H
  )

  O = ifelse(
    O < 0,
    return("Oxygen content must be a value of 0 or greater than 0. Check for NA values"),
    O
  )

  Cl = ifelse(
    Cl < 0,
    return("Chlorine content must be a value of 0 or greater than 0. Check for NA values"),
    Cl
  )

  # case_when loop
  detect_plastic = case_when(
    C == 10 & H == 8 & O == 4 & Cl == 0 ~ "PETE",
    C == 8 & H == 8 & O == 0 & Cl == 0 ~ "PS",
    C == 3 & H == 6 & O == 0 & Cl == 0 ~ "PP",
    C == 2 & H == 4 & O == 0 & Cl == 0 ~ "PE",
    C == 2 & H == 3 & O == 0 & Cl == 1 ~ "PVC",
    TRUE ~ "other"
  )
}
