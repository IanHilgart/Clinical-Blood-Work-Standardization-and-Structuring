#This script was written by Ian Hilgart Martiszus, please provide me with acknowledgement for any usage.
#It is designed to standardize healthcare clinical lab values, which often have multiple names
#for the same test. The script also averages values for any any lab tests that occur multiple times
#on the same day.
#Before using the script you'll need to remove special characters from 'Result Value' column.
#If you have any questions, then feel free to email me at hilgart@gmail.com

install.packages("data.table")
library(data.table)
library(readxl)
install.packages("tidyr")
install.packages("dplyr")
library(readr)
library(tidyr)
library(dplyr)
library(reshape2)
library(plyr)
#read in the clinical bloodwork .csv file and remove '.' added to the column headers
Data<- as.data.frame(read.csv("Your raw data file here.csv", check.names = FALSE))
colnames(Data) <- gsub("^.*\\.","",(colnames(Data)))

#Read in the CBC dictionary.
#Rename headers to match between dictionary and raw data
UniqueLabTestsCSV <- as.data.frame(read.csv("Clinical Lab Test Dictionary.csv", check.names = FALSE))
colnames(Data)[colnames(Data) == 'Result Component'] <- 'Raw Data Headers'
###############################

#combine dictionary and raw data
new <- Data
new <- as.data.frame((merge(new, UniqueLabTestsCSV, by = 'Raw Data Headers')))
#omit the lab tests that have no matching definition in the dictionary
na.omit(new)
#select only the necessary columns
new2 <- subset(new, select= c("Patient MRN","Result Date","Standardized Headers","Result Value"), na.rm = TRUE)
#Merge Patient MRN, Result Date and Standardized Headers columns into column named 'IDGroup.' 
#This creates a 'barcode' for identical samples.  
new2$IDGroup<- paste0(new2$'Patient MRN', " ", new2$'Result Date', " ", new2$'Standardized Headers')
#select out IDGroup and Result Value into new dataframe
temp<- as.data.frame(subset(new2, select = c('IDGroup', 'Result Value')))
#Average the samples with identical 'barcodes.'
#This part of code is the solution for patients who have multiple blood draws on the same day.
tempavg <- aggregate(temp["Result Value"], by=temp["IDGroup"], na.rm = TRUE, FUN=mean)
#Remove the old values (non averaged values) from new2 dataframe
NewNoVals<- subset(new2, select = c('Patient MRN', 'Result Date', 'Standardized Headers', 'IDGroup'))
#Combine the dataframe that has averaged lab test values 
#with the dataframe that has the adopted dictionary names
AggDat<- (merge(NewNoVals, tempavg, by = 'IDGroup'))
#remove the barcode column
AggDat<- subset(AggDat, select = -(IDGroup))



#convert from long to wide data

ReformattedData<- reshape(AggDat, 
                       timevar = "Standardized Headers",
                       idvar = c("Patient MRN","Result Date"),
                       direction = "wide")
#Remove '.' which get's added when reformatting data
colnames(ReformattedData) <- gsub("^.*\\.","",(colnames(ReformattedData)))

#Write your data to a .csv file
write.csv(ReformattedData,'Name your finished file here.csv')
