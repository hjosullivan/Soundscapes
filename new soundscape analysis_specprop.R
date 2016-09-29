##################################
## specprop soundscape anaylsis ##
##################################

#use specprop function in seewave
#not indices as such, but these parametres can be used as a set of descriptors for the soundscape
#used by Bormpoudakis 2013 to identify acoustic signatures in a Mediterranean landscape

#data frame with columns for:
#1. file name
#2. hour
#3. site
#4. device number
#5. date
#6. mean - mean frequency 
#7. sd - standard deviation of the mean
#8. sem - standard error of the mean
#9. median - median frequency
#10. mode - mode frequency
#11. Q25 - first quartile
#12. Q75 - third quartile
#13. cent - centroid (one of the most significant parameters)
#14. skewness - a measure of asymmetry
#15. kurtosis - a measure of peakedness
#16. sfm - spectral flatness measure 
#17. sh - spectral entropy
#18. prec - frequency precision of the spectrum

#no. 6 - 18 are values from specprop function returned as a list

#clean environment
rm(list = setdiff(ls(), lsf.str()))


#install.packages("plyr")
#install.packages("tidyr")
#install.packages("seewave")
#install.packages("tuneR")
#install.packages("soundecology")
library(tidyr)
library(plyr)
library(seewave)
library(tuneR)
library(soundecology)

setwd()

#############################################################
## function to convert specprop list values to a dataframe ##
#############################################################

ss_metrics <- function(x){
  #import data
  load_in <- tuneR::readWave(x)
  
  #spec
  firstspec <- seewave::spec(load_in, f = NULL)
  
  #specprop
  spec.prop <- seewave::specprop(firstspec, f = NULL, str = FALSE, plot = FALSE)
  
  temp <- data.frame(unlist(spec.prop), stringsAsFactors = F)
  temp$ColNames <- rownames(temp)
  temp <- tidyr::spread(temp, ColNames, unlist.spec.prop.)
  temp$id <- basename(x)
  return(temp)
}

################################################################
## example using ss_metrics function with a single audio file ##
################################################################

ss_metrics("F:/Sound Scapes/Kabocalli/Rec 1/Herps/SM3OW1_0+1_20160625_180000.wav")
#(approx 15 seconds to compute a single 5 minute audio file)

###############
## all files ##
###############

#all files
all_wavs <- list.files("~/Desktop", pattern = "wav", full.names = T)

ss_metrics <- function(list_of_wavs, device_naming = c("sm3", "other_naming_options")){
  temp <- plyr::ldply(list_of_wavs, get_sound_metric)
  if(device_naming == "SM3"){
    temp <- tidyr::separate(temp, id, c("device", "ignore", "date", "time"), sep = "_", remove = T)
    temp$time <- gsub("\\..*", "", temp$time)
    return(dplyr::select(temp, -ignore))}
}

#list of files
ss_metrics(many_wavs, device_naming = "sm3")

#export results
write.table(t,paste("~/Desktop"), sep = ",", col.names = T, row.names = F)