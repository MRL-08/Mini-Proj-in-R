#Mary Rose G.Liguan
#BS in Statistics-3
#CMSC197-3
#FirstMiniProjectInR

##Setting up the working directory

setwd("C:/Users/ASUS/Desktop/specdata")  

#1.PollutantMean

##creating the function pollutantmean that will calculates the mean of a pollutant
      pollutantmean <- function(directory, pollutant, id = 1: 332){

## Get the list of files in the directory
      files <- list.files(path=directory, pattern=".csv") 
      datafiles <- numeric()

## Loop through every file in the directory based on the id parameter
      for(i in id){

## Read CSV file and store in file_data
          datfile <- read.csv(files[i])
          datafiles <- c(datafiles,datfile[[pollutant]])
          }
## mean would calculate the mean of the pollutant in specdata and autoprint
     mean(datafiles, na.rm = TRUE)
     }
##Example Outputs for number 1
pollutantmean("C:/Users/ASUS/Desktop/specdata", "sulfate", 1:10)
[1] 4.064128
pollutantmean("C:/Users/ASUS/Desktop/specdata", "nitrate", 70:72)
[1] 1.706047
pollutantmean("C:/Users/ASUS/Desktop/specdata", "nitrate", 23)
[1] 1.280833



#2Complete

##creating a function complete that reads a directory full of files and reports the number of completely observed cases in each data.
    complete <- function(directory, id = 1:332){
 
## Get the list of files in the directory
    files <- list.files(path=directory, pattern=".csv")
    nobs <- numeric()

## Loop through every file in the directory based on the id parameter  
    for(i in id){
     
## Read CSV file and store in file_data
          datfile <- read.csv(files[i])
          datasum <- sum(complete.cases(datfile))
          nobs <- c(nobs, datasum)
          }

##print data frame
    data.frame(id, nobs)
    }

##Example Outputs for number 2

complete("C:/Users/ASUS/Desktop/specdata", 1)
  id nobs
1  1  117

complete("C:/Users/ASUS/Desktop/specdata", c(2, 4, 8, 10, 12))
  id nobs
1  2 1041
2  4  474
3  8  192
4 10  148
5 12   96

#3.Correlation
##creating the function corr that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations.
    corr <- function(directory, threshold=0){

##Get the list of files in the directory
    files <- list.files(path=directory, pattern=".csv")
    compdir <- complete(directory)

##Number of observations must be greater than threshold to solve the correlation
    ids <- compdir[compdir["nobs"] > threshold, ]$id
    correlation <- numeric()
  
    for(i in ids){
## Read CSV file and store in file_data
        datfile <- read.csv(files[i])
        compdir <- datfile[complete.cases(datfile), ]
        correlation <- c(correlation, cor(compdir$sulfate, compdir$nitrate))
        }
  
     return(correlation)
     }

##Example Outputs for Number 3

cr <- corr("C:/Users/ASUS/Desktop/specdata", 150)
head(cr); summary(cr)
[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 

cr <- corr("C:/Users/ASUS/Desktop/specdata", 400)
head(cr); summary(cr)
[1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.17623 -0.03109  0.10021  0.13969  0.26849  0.7631


#4.Hospital Data

##Setting new desired working directory
    setwd("C:/Users/ASUS/Desktop/rprog_data_ProgHospData")

##Reading in data
    outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    head(outcome)

##Using suppresswarning to get rid of Warnings of NAs
    outcome[, 11] <-suppressWarnings(as.numeric(outcome[, 11]))

##Adjusting plot margins
    par(mar = c(1, 1, 1, 1))

##Plot the histogram with values from outcome[,11]
    hist(outcome[, 11],

##main is attribute for the title of the histogram
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
##xlab is attribute for the label on the x-axis
     xlab = "Deaths",
     
##col is the attribute for the fill color
     col="lightblue"
     )
     