{
  library(openxlsx)
  library(tidyr)
  library(dplyr)
  library(splitstackshape)
  library(readr)
  #library(tm)
  library(lubridate)
  library(rowr)
  library(shiny)
  library(shinydashboard)
}

options(digits = 7)
options(scipen = 999)

SI <<- numeric()
I <- numeric()
A <<- numeric()
AA <<- numeric()

t.amort <- function(c, i, n){
  
  R <<- c*i/(1-(1+i)^(-n))
  SI[1] <<- c
  I[1] <<- c*i
  
  for (k in 1:n) {
    
    A[k] <<- R - I[k]
    AA[k] <<- sum(A[1:k])
    SI[k+1] <<- SI[k] - A[k]
    I[k+1] <<- SI[k+1]*i
  }
}

t.amort(12000, 0.05, 6)
tabla <- cbind(I, A, AA, SI)
tabla <- tabla[1:6,]