#attempt to make all previous scripts general

require(soundecology)
require(tuneR)


#import wav files
#change the pathname variable to the location of wav files
#move files for processing into a seperate directory so you do not have to process everything at once.
pathname<- "~/Documents/Guyana soundscapes/Kabocalli/Rec 1/Herps"
setwd(pathname)
FILE <- list.files(pathname, pattern = ".wav", recursive = FALSE, full.names = TRUE)
from <- 1
to <- length(FILE)

#create a blank table for results
#add named but blank columns.
t <- data.frame(file = FILE[seq(from = 1,to)])
t$HOUR <- NA
t$SITE <- NA
t$DATE <- NA
t$ACI <- NA
t$ADI <- NA
t$AEI <- NA
t$H <- NA

#the SM3 filenames do not have contain specific site names

#calculate indices
for(k in from:to){
  print(FILE[k])
  fn <- basename(FILE[k])
  w <- strsplit(fn, "_")
  t$SITE[k] <- "kabbird4"
  t$DATE[k] <- as.character(w[[1]][3])
  t$HOUR[k] <- substr(w[[1]][4],1,4)
  
  wavfile <-readWave(FILE[k])
  wavfile <- wavfile[seq(1,length(wavfile)/3,1)]
  ACI <- acoustic_complexity(wavfile, min_freq = NA, max_freq = NA, j = 5, fft_w = 512)
  ADI <- acoustic_diversity(wavfile, max_freq = 15000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
  AEI <- acoustic_evenness(wavfile, max-freq = 15000, db_threshold = -50, freq_step = 1000)
  H <- H(wavfile, f = f, wl = 512, envt = "hil", msmooth = NULL, ksmooth = NULL)
  M <- M(wavfile, f = f, envt = "hil", plot = FALSE)
  
  
  t$ACI[k] <- ACI
  t$ADI[k] <- ADI
  t$AEI[k] <- AEI
  t$H[k] <- H
  t$M[k] <- M
  
}

#change df to as.character type so data can be exported as a txt file.  
t <- data.frame(lapply(t, as.character), stringsAsFactors = FALSE)

#export results
write.table(t,paste("~/Documents/Guyana soundscapes/Kabocalli/Rec 1/Herps"), sep = ",", col.names = T, row.names = F)

#additional spectrograms etc.

#using spec/spectro/soundscapespec/measure_signals takes a long time for a single 5 minute audio file
#spectro(wavfile, f = f, palette = terrain.colors, osc = TRUE, colwave = "darkgreen")
#measure_signals(wavfile, wl = wl, min_freq = NA, min_time = NA, max_time = NA, plot_range = 50, dBFS_range = 30, sample_size = 1, resultfile = NA, channel = "left")
#spectro(wavfile, f = f, palette = terrain.colors, osc = TRUE, colwave = "blue")

