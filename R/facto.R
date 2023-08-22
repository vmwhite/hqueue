#' compute the factorial
#'
#' @param no :=  the number to take the factorial of
#'
#' @return the factorial of no, i.e. no!
#' @export
#'
#' @examples
#' val <- facto(3) #val is equal to 3!
facto <- function(no){
  # accept the input provided by the user and convert to integer
  fact = 1
  # checking whether the number is negative, zero or positive
  if(no < 0) {
    print(" The number is negative the factorial does not exist. ")
  } else if(no == 0) {
    return(fact)
  } else {
    for( i in 1:no) {
      fact = fact * i
    }
    return(fact)
  }
}
