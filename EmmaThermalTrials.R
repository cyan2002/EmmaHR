library(ggplot2)
library(patchwork)
library(tidyverse)
library(quantmod)
library(dplyr)
library(ggrepel) 

#combining data
{
  setwd("/Users/chanceyan/Documents/R/EmmaHR/EmmaHR/New_CardiacF(x)_converted")
  getwd()
  
  #creates data frame with all fulldata (heartrates of all trials), doesn't include trial 30 currently
  fulldata <- list.files(pattern = "fulldata.csv") 
  heartrates <- do.call(rbind, lapply(fulldata, read.csv))
  
  #loads and assigns ID to each invidividual
  require("readxl")
  my_data <- read_excel("Combined.xlsx")
  my_data$snail_id = paste(my_data$Trial, my_data$Input, sep="_")
  
  #remove first part of file name
  #heartrates$fname<-gsub("input_0_","",as.character(heartrates$fname))
  #heartrates$fname<-gsub("input_1_","",as.character(heartrates$fname))
  #heartrates$fname<-gsub("input_2_","",as.character(heartrates$fname))
  #heartrates$fname<-gsub("input_3_","",as.character(heartrates$fname))
  #heartrates$fname<-gsub("input_4_","",as.character(heartrates$fname))
  #heartrates$fname<-gsub("input_5_","",as.character(heartrates$fname))
  
  #remove end part of the file name
  #heartrates$fname<-gsub("(_202).*","",as.character(heartrates$fname))
  
  #removes all discards
  heartrates=subset(heartrates, confidence !="Discard")
  
  for(i in 1:nrow(heartrates)){
    if(heartrates[i,9] == "Flatline"){
      heartrates[i,6] = 0
    }
  }
  
  
  #remove white space from confidence
  #heartrates$conf<-gsub(" ","",as.character(heartrates$conf))
  
  #assigns ID to each individual in the heartrates data frame
  heartrates$snail_id = paste(heartrates$Input, heartrates$Trial, sep="_")
  
  #error by NAs? -> possible remove columns with NAs
  #heartrates = subset(heartrates, select = -c(freq, start_time, end_time, recording_length, time))
  
  #combine heartrates df and meta df
  #heartrates=merge(heartrates, my_data, by="snail_id", all.x = TRUE) 
  
  #remove all N includes
  #heartrates=subset(heartrates, Include != "N")
  
  #heartrates$ind = NA
  individuallist <-unique(heartrates$snail_id)
  heartratesbackup <- heartrates
}

{
  heartrates <- heartratesbackup
  #removal of problem ones that wouldn't go through code
  heartrates = subset(heartrates, snail_id != "0_Trial12")
  heartrates = subset(heartrates, snail_id != "1_Trial8")
  
  #Runs, but bad data/shape (noted poor signal)
  heartrates = subset(heartrates, snail_id != "3_Trial3")
  heartrates = subset(heartrates, snail_id != "5_Trial2")
  heartrates = subset(heartrates, snail_id != "1_Trial6")
  heartrates = subset(heartrates, snail_id != "4_Trial12")
  heartrates = subset(heartrates, snail_id != "5_Trial12")
  heartrates = subset(heartrates, snail_id != "4_Trial16")

  #Decreasing from the start (perhaps double check or add more points?)
  heartrates = subset(heartrates, snail_id != "0_Trial4")
  heartrates = subset(heartrates, snail_id != "0_Trial14")
  
  individuallist <-unique(heartrates$snail_id)
}

#translates to ABTs
library(weathermetrics) #Install this package if you dont have it already
heartrates$x=1000/celsius.to.kelvin(heartrates$temperature) #creates x axis with temp in 1000/kelvin
heartrates$y=log(heartrates$Heartrate+1) # creates y axis with nat log of hr

#obtain certain species and trials
#heartrates = subset(heartrates, Species=="Littorina obtusata")
#heartrates = subset(heartrates, Type=='W')

individuallist <-unique(heartrates$snail_id)

#use this code to specifically subset different species and trial types
#heartrates = subset(heartrates, Species=="Littorina littorea")
#heartrates = subset(heartrates, Type=='W')
#individuallist <-unique(heartrates$snail_id)

#par(mfrow=c(4,4))

#selgemented function (ABT)
library(segmented)
for(i in individuallist[1:length(individuallist)]) {
  indiv=subset(heartrates,heartrates$snail_id==i)
  out.lm<-lm(Heartrate~temperature, data=indiv)
  print("-----------------------------------------") #I added these "print" lines to create annotations in the console, but they can be removed
  print(i)
  print("
        ")
  os<-selgmented(out.lm, Kmax=3, type="bic", bonferroni=TRUE)
  plot(Heartrate~temperature, data=indiv, main=i)
  plot(os, add=T, lwd=1.6)
  points(os, add=T, lwd=2,col=c("red","blue","green","yellow"),cex=3)
}

#selgemented function (regulatr data)
library(segmented)
for(i in individuallist[1:length(individuallist)]) {
  indiv=subset(heartrates,heartrates$snail_id==i)
  out.lm<-lm(Heartrate~temperature, data=indiv)
  print("-----------------------------------------") #I added these "print" lines to create annotations in the console, but they can be removed
  print(i)
  print("
        ")
  os<-selgmented(out.lm, Kmax=4, type="bic", bonferroni=TRUE)
  plot(Heartrate~temperature, data=indiv, main=i)
  plot(os, add=T, lwd=1.6)
  points(os, add=T, lwd=2,col=c("red","blue","green","yellow"),cex=3)
}

#In the below script, the "segmented' function fits a different number of breakpoints for each individual as specified in an "n.breaks" column on the datasheet
#use this once you have determined the number of breakpoints for each individual, and want to double check you picked the right amount
for(i in individuallist[1:length(individuallist)]) {
  indiv=subset(heartrates,heartrates$snail_id==i)
  out.lm<-lm(Heartrate~temperature, data=indiv)
  os<-segmented(out.lm, npsi=first(1))
  plot(Heartrate~temperature,data=indiv, main=first(indiv$snail_id))
  plot(os, add=T, lwd=1.6)
  points(os, add=T, lwd=2,col=c("red","blue","green","yellow"),cex=3)
}

#collect data from regression lines huge code from here
{
  library(weathermetrics)
  
  heartrates$Individual=heartrates$snail_id
  
  #divide your data into three different files based on the number of breakpoints (specified in the column "n.breaks.arrhenius")
  singleonly=subset(heartrates,Breakpoints=="1")
  tripleonly=subset(heartrates,Breakpoints=="3")
  doubleonly=subset(heartrates,Breakpoints=="2")
  
  #Create seperate individual lists for each file 
  singleonly <-unique(singleonly$Individual)
  tripleonly <-unique(tripleonly$Individual)
  doubleonly <-unique(doubleonly$Individual)
  
  #script for single break individuals
  #---------------
  
  # 3. run loop - subsets the data frame for each sample and adds the results to the lists for lm(), segmented(), 
  # predicted values from segmented lines, and breakpoints contained under $psi of the segmented results 
  
  #create/ clear storage files
  fit <- list()
  fit.seg <- list()
  predicted.seg <- list()
  breakpoints <- data.frame(Psi=as.numeric(),ABT=as.numeric(),SE=as.numeric())
  
  #script to calculate predicted heartrate at breakpoint, and slopes, in single bp individuals
  
  {breakpointhr <- list()
    endslope <- list()
    initialslope <- list()
    breakpointslist  <- list()
    intercepts <- list()
  } #creat lists to hold data
  
  for(i in singleonly[1:length(singleonly)]) {
    fit.seg.A1<-list()
    fit[[i]] <- lm(y~x, data=subset(heartrates,heartrates$Individual==i))
    fit.seg[[i]]<-segmented(fit[[i]], npsi=1, data=heartrates, na.action=na.exclude)
    breakpoints[i,] <- fit.seg[[i]]$psi
    fit.seg.A1<-fit.seg[[i]]
    breakpointhr[[i]]= fit.seg.A1$coefficients[1] + fit.seg.A1$coefficients[2] * fit.seg.A1$psi[2] #gives y1)
    endslope[[i]]=fit.seg.A1$coefficients[2] # initial slope
    initialslope[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]) # initial slope (need to change code for double endpoints)
  } #run script
  
  library (plyr)
  library(weathermetrics)
  
  {
    breakpointhr <- ldply (breakpointhr, data.frame)
    endslope <- ldply (endslope, data.frame)
    initialslope <- ldply (initialslope, data.frame)
    
    breakpoints$Individual <- rownames(breakpoints) 
    breakpoints$Individual=as.factor(breakpoints$Individual)
    colnames(breakpointhr)[1] <- "Individual"
    colnames(endslope)[1] <- "Individual"
    colnames(endslope)[2] <- "endslope"
    colnames(initialslope)[1] <- "Individual"
    colnames(initialslope)[2] <- "initialslope"
    colnames(breakpointhr)[2] <- "finalbphr"
    
    breakpointslist=merge(breakpoints, breakpointhr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, endslope, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, initialslope, by="Individual", all = T) # merge breakpoint data with breakpointhr and slopes
    breakpointslist$ABT=1000/breakpointslist$ABT
    breakpointslist$ABT=kelvin.to.celsius(breakpointslist$ABT) #converts predicted breakpoint back to celsius
    breakpointslist$finalbphr=exp(breakpointslist$finalbphr)
  }
  singlebreakpointlist=breakpointslist
  
  
  #script for double break individuals
  #---------------
  
  # 3. run loop - subsets the data frame for each sample and adds the results to the lists for lm(), segmented(), 
  # predicted values from segmented lines, and breakpoints contained under $psi of the segmented results 
  
  #create/ clear storage files
  
  library(segmented)
  
  {fit <- list()
    fit.seg <- list()
    predicted.seg <- list()
    breakpoints <- data.frame(Psi=as.numeric(),Psi1=as.numeric(),ABT=as.numeric(),ABT1=as.numeric(),SE1=as.numeric(),SE=as.numeric())
    finalbphr <- list()
    breakpoint1hr  <- list()
    slope1 <- list()
    endslope <- list()
    initialslope <- list()
    breakpointslist  <- list()
    intercepts <- list()
  } #creat lists to hold data
  
  for(i in doubleonly[1:length(doubleonly)]) {
    fit.seg.A1<-list()
    fit[[i]] <- lm(y~x, data=subset(heartrates,heartrates$Individual==i))
    fit.seg[[i]]<-segmented(fit[[i]], npsi=2, data=heartrates, na.action=na.exclude)
    breakpoints[i,] <- fit.seg[[i]]$psi
    fit.seg.A1<-fit.seg[[i]]
    finalbphr[[i]]= fit.seg.A1$coefficients[1] + fit.seg.A1$coefficients[2] * fit.seg.A1$psi[3] #gives finalbphr)
    endslope[[i]]=fit.seg.A1$coefficients[2] # endslope (fixed)
    slope1[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]) #slope1 (fixed)
    initialslope[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]+fit.seg.A1$coefficients[4]) # initial slope
    breakpoint1hr[[i]]= fit.seg.A1$coefficients[1] -(fit.seg.A1$coefficients[3])*fit.seg.A1$psi[3] + (fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3])*fit.seg.A1$psi[4] #gives y2
    
  } 
  
  #run script
  
  
  library (plyr)
  
  {finalbphr <- ldply (finalbphr, data.frame)
    breakpoint1hr <- ldply (breakpoint1hr, data.frame)
    slope1 <- ldply (slope1, data.frame)
    endslope <- ldply (endslope, data.frame)
    initialslope <- ldply (initialslope, data.frame)
    breakpoints$Individual <- rownames(breakpoints) 
    breakpoints$Individual=as.factor(breakpoints$Individual)
    colnames(finalbphr)[1] <- "Individual"
    colnames(slope1)[1] <- "Individual"
    colnames(slope1)[2] <- "slope1"
    colnames(endslope)[1] <- "Individual"
    colnames(endslope)[2] <- "endslope"
    colnames(initialslope)[1] <- "Individual"
    colnames(initialslope)[2] <- "initialslope"
    colnames(finalbphr)[1] <- "Individual"
    colnames(finalbphr)[2] <- "finalbphr"
    colnames(breakpoint1hr)[1] <- "Individual"
    colnames(breakpoint1hr)[2] <- "breakpoint1hr"
    
    breakpointslist=merge(breakpoints, finalbphr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, breakpoint1hr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, slope1, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, endslope, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, initialslope, by="Individual", all = T) # merge breakpoint data with breakpointhr and slopes
    breakpointslist$ABT=1000/breakpointslist$ABT
    breakpointslist$ABT=kelvin.to.celsius(breakpointslist$ABT)
    breakpointslist$ABT1=1000/breakpointslist$ABT1
    breakpointslist$ABT1=kelvin.to.celsius(breakpointslist$ABT1)
    breakpointslist$breakpoint1hr=exp(breakpointslist$breakpoint1hr)
    breakpointslist$finalbphr=exp(breakpointslist$finalbphr)
  }
  
  doublebreakpointlist=breakpointslist
  
  #script for triple break individuals
  #---------------
  
  # 3. run loop - subsets the data frame for each sample and adds the results to the lists for lm(), segmented(), 
  # predicted values from segmented lines, and breakpoints contained under $psi of the segmented results 
  
  #create/ clear storage files
  
  library(segmented)
  
  {fit <- list()
    fit.seg <- list()
    predicted.seg <- list()
    breakpoints <- data.frame(Psi=as.numeric(),Psi1=as.numeric(),Psi2=as.numeric(),ABT=as.numeric(),ABT1=as.numeric(),ABT2=as.numeric(),SE=as.numeric(),SE1=as.numeric(),SE2=as.numeric())
    finalbphr <- list()
    breakpoint1hr  <- list()
    breakpoint2hr <- list()
    slope1 <- list()
    slope2 <- list()
    endslope <- list()
    initialslope <- list()
    breakpointslist  <- list()
    intercepts <- list()
  } #creat lists to hold data
  
  for(i in tripleonly[1:length(tripleonly)]) {
    fit.seg.A1<-list()
    fit[[i]] <- lm(y~x, data=subset(heartrates,heartrates$Individual==i))
    fit.seg[[i]]<-segmented(fit[[i]], npsi=3, data=heartrates, na.action=na.exclude)
    breakpoints[i,] <- fit.seg[[i]]$psi
    fit.seg.A1<-fit.seg[[i]]
    finalbphr[[i]]= fit.seg.A1$coefficients[1] + fit.seg.A1$coefficients[2]* fit.seg.A1$psi [4] #gives finalbphr)
    endslope[[i]]=fit.seg.A1$coefficients[2] # endslope (fixed)
    slope1[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]) #slope1 (fixed)
    slope2[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]+fit.seg.A1$coefficients[4]) # slope2
    initialslope[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]+fit.seg.A1$coefficients[4]+fit.seg.A1$coefficients[5]) # initialslope
    breakpoint1hr[[i]]= fit.seg.A1$coefficients[1] -(fit.seg.A1$coefficients[3])*fit.seg.A1$psi[4] + (fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3])*fit.seg.A1$psi[5] # breakpoint 1
    breakpoint2hr[[i]]= fit.seg.A1$coefficients[1] -(fit.seg.A1$coefficients[3])*fit.seg.A1$psi[4] -(fit.seg.A1$coefficients[4])*fit.seg.A1$psi[5] + (fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]+fit.seg.A1$coefficients[4])*fit.seg.A1$psi[6] #breakpoint 2
  } #run script
  
  library (plyr)
  library(weathermetrics)
  {finalbphr <- ldply (finalbphr, data.frame)
    breakpoint1hr <- ldply (breakpoint1hr, data.frame)
    breakpoint2hr <- ldply (breakpoint2hr, data.frame)
    slope1 <- ldply (slope1, data.frame)
    slope2 <- ldply (slope2, data.frame) 
    endslope <- ldply (endslope, data.frame)
    initialslope <- ldply (initialslope, data.frame)
    breakpoints$Individual <- rownames(breakpoints) 
    breakpoints$Individual=as.factor(breakpoints$Individual)
    colnames(finalbphr)[1] <- "Individual"
    colnames(slope1)[1] <- "Individual"
    colnames(slope1)[2] <- "slope1"
    colnames(slope2)[1] <- "Individual"
    colnames(slope2)[2] <- "slope2"
    colnames(endslope)[1] <- "Individual"
    colnames(endslope)[2] <- "endslope"
    colnames(initialslope)[1] <- "Individual"
    colnames(initialslope)[2] <- "initialslope"
    colnames(finalbphr)[1] <- "Individual"
    colnames(finalbphr)[2] <- "finalbphr"
    colnames(breakpoint1hr)[1] <- "Individual"
    colnames(breakpoint1hr)[2] <- "breakpoint1hr"
    colnames(breakpoint2hr)[1] <- "Individual"
    colnames(breakpoint2hr)[2] <- "breakpoint2hr"
    breakpointslist=merge(breakpoints, finalbphr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, breakpoint1hr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, breakpoint2hr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, slope1, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, slope2, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, endslope, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, initialslope, by="Individual", all = T) # merge breakpoint data with breakpointhr and slopes
    breakpointslist$ABT=1000/breakpointslist$ABT
    breakpointslist$ABT=kelvin.to.celsius(breakpointslist$ABT)
    breakpointslist$ABT1=1000/breakpointslist$ABT1
    breakpointslist$ABT1=kelvin.to.celsius(breakpointslist$ABT1)
    breakpointslist$ABT2=1000/breakpointslist$ABT2
    breakpointslist$ABT2=kelvin.to.celsius(breakpointslist$ABT2)
    breakpointslist$breakpoint1hr=exp(breakpointslist$breakpoint1hr)
    breakpointslist$breakpoint2hr=exp(breakpointslist$breakpoint2hr)
    breakpointslist$finalbphr=exp(breakpointslist$finalbphr)
  }
  
  triplebreakpointlist=breakpointslist
  
  
  #format into single breakpoint list 
  
  #(check that the correct ABTs from the double and triple breakpoint individuals are placed into the same column as ABTs for the single breakpoint individuals)
  {
    singlebreakpointlist$breakpoints="single"
    doublebreakpointlist$breakpoints="double"
    triplebreakpointlist$breakpoints="triple"
    allbreakpointlist=merge(singlebreakpointlist,doublebreakpointlist, all = T)
    allbreakpointlist=merge(allbreakpointlist,triplebreakpointlist, all = T)
    #allbreakpointlist contains all the info :)
  }
  
}
#may need to divide individuals up by species and trial then analyze heart rate and breaking points
allbreakpointlist2 = allbreakpointlist #backup heartrate data

#heartrates2 has all meta data backed up
#heartrates3 has just flatline rows of all individuals
heartrates = merge(heartrates, allbreakpointlist)
heartrates2 = heartrates
#use the following code below when grapging Flatline graphs
heartrates = subset(heartrates, conf == 'Flatline')
heartrates3 = heartrates