# load required packages
library('assertthat')
library('roxygen2')

#'Implements SEDOL validation
#'@param inputString the string to validate
#'@return TRUE/FALSE value
#'
#'@author themellion
IsValidSEDOL <- function(inputString) {
  
  # check if argument is missing
  if(missing(inputString)){
    return(FALSE)
  }
  
  # check if argument is NULL
  if(is.null(inputString)){
    return(FALSE)
  }

  #check the length of the string provided
  assert_that(nchar(inputString) == 7, msg = 'Wrong length of argument provided')
  
  #convert to upper case
  inputString <- toupper(inputString)
  
  # split word into characters
  inputStringNum <- unlist(strsplit(inputString, ''))
  
  # check last character is a number
  assert_that(is.numeric(as.numeric(inputStringNum[7])), msg = 'Last character not numeric') 
  
  # convert characters to numerics
  a_list <- list()
  for (i in 1:length(inputStringNum)){
    a_list[i] <- match(inputStringNum[i], LETTERS) + 9
  }
  inputStringNum2 <- unlist(a_list)
  pos = which(is.na(inputStringNum2)) 
  inputStringNum2[pos] <- inputStringNum[pos]

  # calculate checksum
  wgts <- c(1, 3, 1, 7, 3, 9)
  dgts <- as.numeric(inputStringNum2)
  sumString <- crossprod(dgts[1:6], wgts)
  
  # make sure last digit agrees with mod
  assert_that((10 - (sumString %% 10) %% 10) == as.numeric(inputStringNum2[7]), msg = 'Wrong format')
}
  

#test function
IsValidSEDOL(NULL) 
IsValidSEDOL() 
IsValidSEDOL('12') 
IsValidSEDOL('123456789') 
IsValidSEDOL('1234567') 
IsValidSEDOL('0709954') 
IsValidSEDOL('B0YBKJ7')
IsValidSEDOL ('B0Ybkj7')
IsValidSEDOL('9123458')
IsValidSEDOL('9aBcDe1')



