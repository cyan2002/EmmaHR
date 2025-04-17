library(ggplot2)
library(patchwork)
library(tidyverse)
library(quantmod)
library(dplyr)
library(ggrepel) 
library(segmented)
library(AICcmodavg)
library(emmeans)
library(multcomp)
library(multcompView)

#combining data
{
  setwd("/Users/chanceyan/Documents/R/EmmaHR/EmmaHR/New_CardiacF(x)_converted")
  getwd()
  
  #creates data frame with all fulldata (heartrates of all trials), doesn't include trial 30 currently
  fulldata <- list.files(pattern = "fulldata.csv") 
  heartrates <- do.call(rbind, lapply(fulldata, read.csv))
  
  #loads and assigns ID to each invidividual
  require("readxl")
  setwd("/Users/chanceyan/Documents/R/EmmaHR/EmmaHR")
  my_data <- read_excel("Combined.xlsx")

  #removes all discards
  heartrates=subset(heartrates, confidence !="Discard")
  
  for(i in 1:nrow(heartrates)){
    if(heartrates[i,9] == "Flatline"){
      heartrates[i,6] = 0
    }
  }
  
  #assigns ID to each individual in the heartrates data frame
  heartrates$snail_id = paste(heartrates$Input, heartrates$Trial, sep="_")
  
  #combine heartrates df and meta df
  heartrates=merge(heartrates, my_data, by="snail_id", all.x = TRUE) 
  
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
individuallist <-unique(heartrates$snail_id)

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
  
  heartrates$snail_id=heartrates$snail_id
  
  #divide your data into three different files based on the number of breakpoints (specified in the column "n.breaks.arrhenius")
  singleonly=subset(heartrates,Breakpoints=="1")
  
  #Create seperate individual lists for each file 
  singleonly <-unique(singleonly$snail_id)
  
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
    fit[[i]] <- lm(y~x, data=subset(heartrates,heartrates$snail_id==i))
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
    
    breakpoints$snail_id <- rownames(breakpoints) 
    breakpoints$snail_id=as.factor(breakpoints$snail_id)
    colnames(breakpointhr)[1] <- "snail_id"
    colnames(endslope)[1] <- "snail_id"
    colnames(endslope)[2] <- "endslope"
    colnames(initialslope)[1] <- "snail_id"
    colnames(initialslope)[2] <- "initialslope"
    colnames(breakpointhr)[2] <- "finalbphr"
    
    breakpointslist=merge(breakpoints, breakpointhr, by="snail_id", all = T)
    breakpointslist=merge(breakpointslist, endslope, by="snail_id", all = T)
    breakpointslist=merge(breakpointslist, initialslope, by="snail_id", all = T) # merge breakpoint data with breakpointhr and slopes
    breakpointslist$ABT=1000/breakpointslist$ABT
    breakpointslist$ABT=kelvin.to.celsius(breakpointslist$ABT) #converts predicted breakpoint back to celsius
    breakpointslist$finalbphr=exp(breakpointslist$finalbphr)
  }
  singlebreakpointlist=breakpointslist
  
}

#heartrates2 has all meta data backed up
#heartrates3 has just flatline rows of all individuals
heartrates = merge(heartrates, singlebreakpointlist,by="snail_id")
heartratesbackup <- heartrates
#use the following code below when grapging Flatline graphs
flatlines = subset(heartrates, confidence == 'Flatline' | confidence == "Zero")

ggplot(data = heartrates, aes(x = population, y = ABT, color = Treatment)) + geom_boxplot() + facet_wrap(~group) + ggtitle("ABT differences across group and treatments")
ggplot(data = flatlines, aes(x = population, y = temperature, color = Treatment)) + geom_boxplot() + facet_wrap(~group)+ ggtitle("FLT differences across group and treatments")

table(flatlines$population, flatlines$group, flatlines$Treatment)

#Sorting datetime to date in a new column. Some rows didn't have a time which messed up the clean code, 
#so I had to parse through each one to see which ones needed to be corrected
flatlines$day <- as.POSIXct(flatlines$date, format = "%Y-%m-%d %H:%M:%S")
flatlines$day <- as.Date(flatlines$day)
for(i in 1:nrow(flatlines)){
  if(is.na(flatlines$day[i])){
    flatlines$day[i] <- as.POSIXct(flatlines$date, format = "%Y-%m-%d %H:%M")
    flatlines$day <- as.Date(flatlines$day)
  }
}

#need to add size data to the file. (There is none...?)

#checking means
tapply(flatlines$ABT, flatlines$population, FUN = mean)
tapply(flatlines$ABT, flatlines$group, FUN = mean)
tapply(flatlines$ABT, flatlines$Treatment, FUN = mean)

#beginning to model
#-------------------------------------------------------------------------------------------------
library(MuMIn)
modFull <- glm(ABT ~ population*Treatment*group + (1/day), data = flatlines, family = "Gamma"(link = "identity"), na.action = na.fail)
dredge(modFull)

#model that is good/chosen/picked
bestMod <- glm(ABT ~ population*Treatment + (1/day), data = flatlines, family = "gaussian"(link = "identity"))
summary(bestMod)
plot(bestMod)
hist(bestMod$residuals)

#Trying 2nd best model - with group seems like a much better fit!
bestMod <- glm(ABT ~ group + population*Treatment + (1/day), data = flatlines, family = "Gamma"(link = "log"))
summary(bestMod)
plot(bestMod)
hist(bestMod$residuals)

#Trying another best model - with group seems like a much better fit!
bestMod <- glm(ABT ~ population + Treatment + (1/day), data = flatlines, family = "Gamma"(link = "log"))
summary(bestMod)
plot(bestMod)
hist(bestMod$residuals)

#seems like the second model, interaction with population and treatment additive of group seems like a good fit!

model_means=emmeans(bestMod, list(pairwise ~ population*Treatment + group + (1/day)), adjust = "tukey")
model_means_cld <- cld(object = model_means,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)
data_summ$Cld=model_means_cld$Letters
ggplot(flatlines) +
  aes(x = Treatment, y = temperature, color = population) +
  geom_boxplot()+
  geom_text(data = model_means_cld, aes(label=.group, x=Treatment, color = population, y=emmean+38.5),position = position_dodge(width=1) )+
  theme_classic() + facet_wrap(~group) 
#+ ylim(37.5,42.5)
