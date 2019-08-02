##################################### INFO #####################################
#Modelli probabilistici per le decisioni
#Progetto: bike sharing  prediction system
#Team:
# - Mattia Pennati, 793375
# - Luca Virgilio, 794866

#set working directory [CHANGE IT BEFORE RUN]
setwd(paste0("C:/Users/luca/Desktop/bikeSharingProject (1)/",
             "bikeSharingProject/"))
################################################################################


################################## LIBRARIES ###################################
#bnlearn package for bayesian networks
if (!require("bnlearn"))
  install.packages("bnlearn", dependencies = T)
library("bnlearn")
#graph package
if (!require("graph"))
  install.packages("graph", dependencies = T)
library("graph")
#graph plotting package
if (!require("Rgraphviz"))
  install.packages("Rgraphviz", dependencies = T)
library("Rgraphviz")
#exact inference on bnlearn package  
if (!require("gRain"))
  install.packages("gRain", dependencies = T)
library("gRain")
#ROC curves package for performance analysis 
if (!require("ROCR"))
  install.packages("ROCR", dependencies = T)
library("ROCR")
#alternative plot package
if (!require("ggplot2"))
  install.packages("ggplot2", dependencies = T)
library("ggplot2")
#correlation packages
if (!require("corrplot"))
  install.packages("corrplot", dependencies = T)
library("corrplot")
if (!require("ggcorrplot"))
  install.packages("ggcorrplot", dependencies = T)
library("ggcorrplot")
if (!require("PerformanceAnalytics"))
  install.packages("PerformanceAnalytics", dependencies = T)
library("PerformanceAnalytics")
if (!require("psych"))
  install.packages("psych", dependencies = T)
library("psych")
if (!require("GGally"))
  install.packages("GGally", dependencies = T)
library("GGally")
if (!require("corrr"))
  install.packages("corrr", dependencies = T)
library("corrr")
################################################################################



################################# IMPORT DATA ##################################
#import day dataset
dayDataset = read.csv('Datasets/day.csv', sep = ",")
#import hour dataset
hourDataset = read.csv('Datasets/hour.csv', sep = ",")
################################################################################



############################### PREPROCESS DATA ################################
#remove "useless" columns
hourDataset[c(1,2,15,16)] = list(NULL)

#-------------------------- PLOTS PRE DISCRETIZATION ---------------------------
#analyze the dataset
#summary(dayDataset)
#summary(dayDataset$registered)
#plot(...)

#hr vs cnt
boxplot(hourDataset$cnt ~ hourDataset$hr, col = "coral2", 
        xlab = "hour (hr)", ylab = "# used bikes (cnt)", 
        main="hr/cnt distribution")
grid(NULL, NULL, lty = 5)

#boxplot for each discrete variable
par(mfrow=c(2,3))
boxplot(hourDataset$cnt ~ hourDataset$season, col = "gold1", 
        xlab = "season", ylab = "# used bikes (cnt)", 
        main="season/cnt distribution")
grid(NULL, NULL, lty = 5)
boxplot(hourDataset$cnt ~ hourDataset$mnth, col = "goldenrod2", 
        xlab = "month (mnth)", ylab = "# used bikes (cnt)", 
        main="mnth/cnt distribution")
grid(NULL, NULL, lty = 5)
boxplot(hourDataset$cnt ~ hourDataset$weathersit, col = "darkorange", 
        xlab = "weathersit", ylab = "# used bikes (cnt)", 
        main="weathersit/cnt distribution")
grid(NULL, NULL, lty = 5)
boxplot(hourDataset$cnt ~ hourDataset$holiday, col = "darkorange3", 
        xlab = "holiday", ylab = "# used bikes (cnt)", 
        main="holiday/cnt distribution")
grid(NULL, NULL, lty = 5)
boxplot(hourDataset$cnt ~ hourDataset$workingday, col = "firebrick1", 
        xlab = "workingday", ylab = "# used bikes (cnt)", 
        main="workingday/cnt distribution")
grid(NULL, NULL, lty = 5)
boxplot(hourDataset$cnt ~ hourDataset$weekday, col = "firebrick3", 
        xlab = "weekday", ylab = "# used bikes (cnt)", 
        main="weekday/cnt distribution")
grid(NULL, NULL, lty = 5)

#distribution plot for each continue variable
par(mfrow=c(2,2))
plot(hourDataset$temp, hourDataset$cnt, main = "temp/cnt distribution", 
     xlab = "temperature (temp)", ylab = "# used bikes (cnt)", 
     col = "skyblue")
grid(NULL, NULL, lty = 2)
plot(hourDataset$atemp, hourDataset$cnt, main = "atemp/cnt distribution", 
     xlab = "real feel temperature (atemp)", 
     ylab = "# used bikes (cnt)", col = "steelblue1")
grid(NULL, NULL, lty = 2)
plot(hourDataset$hum, hourDataset$cnt, main = "hum/cnt distribution", 
     xlab = "humidity (hum)", ylab = "# used bikes (cnt)", 
     col = "steelblue3")
grid(NULL, NULL, lty = 2)
plot(hourDataset$windspeed, hourDataset$cnt, 
     main = "windspeed/cnt distribution", 
     xlab = "windspeed", ylab = "# used bikes (cnt)", col = "steelblue4")
grid(NULL, NULL, lty = 2)

#restore par
par(mfrow=c(1,1))
#------------------------------------------------------------------------------


#------------------------------- DISCRETIZATION --------------------------------
#discretize all dataset (only for not discrete variables)
#factorization: 
#hourDataset[] = lapply(hourDataset, factor)
#discretization: 
#hourDataset  = discretize(hourDataset, breaks = 2, method = "interval")
#check difference with your own implementation: seems WORSE, check what change
# modifing some parameters

#OBS: discretization is useful and important. Discretize some features more
# then others.

#plot for cnt variables (remember: 1300 total bikes year 0 and 1500 year 1)
#plot()
#discretization of target variable 'cnt'
#4 interval (1,2,3,4) = 
#version 1:
# year 0: (very low 0-100, low 100-300, medium 300-600, high >600)
# year 1: (very low 0-120, low 120-350, medium 350-700, high >700)
#version 2:
# year 0: (very low 0-68, low 68-210, medium 210-450, high >450)
# year 1: (very low 0-120, low 120-350, medium 350-700, high >700)
#version 3:
# year 0: (very low 0-80, low 80-250, medium 250-500, high >500)
# year 1: (very low 0-120, low 120-350, medium 350-700, high >700)
temp = hourDataset$cnt
for(i in 1:nrow(hourDataset)){
  if(hourDataset$yr[i] == 0){
    if(temp[i]<=80){
      temp[i] = 1
    } else if(temp[i]<=250) {
      temp[i] = 2
    } else if(temp[i]<=500) { 
      temp[i] = 3
    } else {
      temp[i] = 4
    }
  } else {
    if(temp[i]<=120){
      temp[i] = 1
    } else if(temp[i]<=350) {
      temp[i] = 2
    } else if(temp[i]<=700) {
      temp[i] = 3
    } else {
      temp[i] = 4
    }
  }
}
hourDataset$cnt = temp

#discretization of target variable 'hr'
#plot(hourDataset$hr, hourDataset$cnt, 
#     col = c("red", "orange", "yellow", "green" ))
#4 interval (1,2,3,4) = (0-2, 3-6, 7-8, 9-10, 11-16, 17-19, 20-21, 21-23)
#temp = hourDataset$hr
#temp = sapply(temp, function(x){
#  if(x<=2){
#    x = 1
#  } else if(x<=6) {
#    x = 2
#  } else if(x<=8) {
#    x = 3
#  } else if(x<=10) {
#    x = 4
#  } else if(x<=16) {
#    x = 5
#  } else if(x<=19) {
#    x = 6
#  } else if(x<=21) {
#    x = 7
#  } else {
#    x = 8
#  }})
#hourDataset$hr = temp
#OBS: results get worse

#discretization temperature (temp: -8° -- 38°)
temp = hourDataset$temp
max_val = max(temp)
min_val = min(temp)
interval = (max_val - min_val)/10 #10 intervals (near to 5° each one)
temp = sapply(temp, function(x){
  if(x<interval){
    x = 1
  } else if(x<2*interval) {
    x = 2
  } else if(x<3*interval) {
    x = 3
  } else if(x<4*interval) {
    x = 4
  } else if(x<5*interval) {
    x = 5
  } else if(x<6*interval) {
    x = 6
  } else if(x<7*interval) {
    x = 7
  } else if(x<8*interval) {
    x = 8
  } else if(x<9*interval) {
    x = 9
  } else {
    x = 10
  }})
hourDataset$temp = temp

#discretization feeling temperature (atemp: -15° -- 50°)
temp = hourDataset$atemp
max_val = max(temp)
min_val = min(temp)
interval = (max_val - min_val)/13 #13 intervals (5° each one)
temp = sapply(temp, function(x){
  if(x<interval){
    x = 1
  } else if(x<2*interval) {
    x = 2
  } else if(x<3*interval) {
    x = 3
  } else if(x<4*interval) {
    x = 4
  } else if(x<5*interval) {
    x = 5
  } else if(x<6*interval) {
    x = 6
  } else if(x<7*interval) {
    x = 7
  } else if(x<8*interval) {
    x = 8
  } else if(x<9*interval) {
    x = 9
  } else if(x<10*interval) {
    x = 10
  } else if(x<11*interval) {
    x = 11
  } else if(x<12*interval) {
    x = 12
  } else {
    x = 13
  }
})
hourDataset$atemp = temp

#discretization of humidity (hum)
temp = hourDataset$hum
max_val = max(temp)
min_val = min(temp)
interval = (max_val - min_val)/10 #10 intervals
temp = sapply(temp, function(x){
  if(x<2*interval) {
    #interval 1 and 2 are considered the same (hum < 10% or < 20% --> 
    # very low humidity) --> only 100/150 istances
    x = 1
  } else if(x<3*interval) {
    x = 2 
  } else if(x<4*interval) {
    x = 3
  } else if(x<5*interval) {
    x = 4
  } else if(x<6*interval) {
    x = 5
  } else if(x<7*interval) {
    x = 6
  } else if(x<8*interval) {
    x = 7
  } else if(x<9*interval) {
    x = 8
  } else {
    x = 9
  }})
hourDataset$hum = temp

#discretization of wind speed (windspeed)
temp = hourDataset$windspeed
max_val = max(temp)
min_val = min(temp)
interval = (max_val - min_val)/9 #9 intervals
temp = sapply(temp, function(x){
  if(x<interval){
    x = 1
  } else if(x<2*interval) {
    x = 2
  } else if(x<3*interval) {
    x = 3
  } else if(x<4*interval) {
    x = 4
  } else if(x<5*interval) {
    x = 5
  } else if(x<6*interval) {
    x = 6
  } else if(x<7*interval) {
    x = 7
  }  else {
    x = 8 #union of 8 and 9 cause together has only 15 events
  }})
hourDataset$windspeed = temp
#-------------------------------------------------------------------------------


#-------------------------------- CORRELATION ----------------------------------
cordataset = hourDataset[, -2]
#check correlation
res = cor(cordataset)
#corrplot 2: with corr. values and no diagonal
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
png(filename = "Correlation/corr.png", width = 1024)
corrplot(res, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)
dev.off()
#corrplot 3: with X on the value too much low
png(filename = "Correlation/gg_corr.png", width = 1024)
ggcorrplot(cor(res), p.mat = cor_pmat(cordataset), 
           hc.order=TRUE, type='lower')
dev.off()
#heatmap
col= colorRampPalette(c("blue", "white", "red"))(20)
png(filename = "Correlation/heatmap_corr.png", width = 1024)
heatmap(x = res, col = col, symm = T)
dev.off()
#correlation chart
png(filename = "Correlation/chart_corr.png", width = 1024)
chart.Correlation(cordataset, histogram=TRUE, pch=19, col = "blue")
dev.off()
#psyck correlation chart
png(filename = "Correlation/psyck_corr.png", width = 1024)
pairs.panels(cordataset, scale=TRUE)
dev.off()
#ggpairs correlation chart 
png(filename = "Correlation/ggpairs_corr.png", width = 1024)
ggpairs(cordataset, aes(col = as.factor(cnt)))
dev.off()
#-------------------------------------------------------------------------------


#-------------------------- PLOTS POST DISCRETIZATION --------------------------
#distribution plot for each continue-discretized variable
par(mfrow=c(2,2))
boxplot(hourDataset$temp ~ hourDataset$cnt, col = "skyblue", 
        ylab = "temperature (temp)", xlab = "# used bikes (cnt)", 
        main="temp/cnt distribution")
grid(NULL, NULL, lty = 5)
boxplot(hourDataset$atemp ~ hourDataset$cnt, main = "atemp/cnt distribution", 
     ylab = "real feel temperature (atemp)", 
     xlab = "# used bikes (cnt)", col = "steelblue1")
grid(NULL, NULL, lty = 5)
boxplot(hourDataset$hum ~ hourDataset$cnt, main = "hum/cnt distribution", 
     ylab = "humidity (hum)", xlab = "# used bikes (cnt)", 
     col = "steelblue3")
grid(NULL, NULL, lty = 5)
boxplot(hourDataset$windspeed ~ hourDataset$cnt, main = "windspeed VS cnt", 
     ylab = "windspeed", xlab = "# used bikes (cnt)", col = "steelblue4")
grid(NULL, NULL, lty = 5)
par(mfrow=c(1,1))#restore par
#------------------------------------------------------------------------------


#------------------------ TRAIN AND TEST SETS CREATION -------------------------
#factorize all the variables
hourDataset[] = lapply(hourDataset, factor)

#create Train and test sets
# 0) full dataset as trainset ONLY to infer the structure of the net
trainset = hourDataset
trainset$yr = NULL
# 1) as reported in the paper: year 0 --> train, year = 1 --> test
# 50% - 50%
trainset1 = hourDataset[hourDataset$yr == 0,]
trainset1$yr = NULL
testset1 = hourDataset[hourDataset$yr == 1,]
testset1$yr = NULL
#remove year
hourDataset$yr = NULL
# 2) 55% - 45%
indexes = sample(1:nrow(hourDataset), replace = F)
hourDataset$index = indexes
trainset2 = hourDataset[sample(nrow(hourDataset), (0.55 * nrow(hourDataset))),]
testset2 = subset(hourDataset, !(hourDataset$index %in% trainset2$index))
# 3) 60% - 40%
trainset3 = hourDataset[sample(nrow(hourDataset), (0.6 * nrow(hourDataset))),]
testset3 = subset(hourDataset, !(hourDataset$index %in% trainset3$index))
# 4) 65% - 35%
trainset4 = hourDataset[sample(nrow(hourDataset), (0.65 * nrow(hourDataset))),]
testset4 = subset(hourDataset, !(hourDataset$index %in% trainset4$index))
# 5) 70% - 30%
trainset5 = hourDataset[sample(nrow(hourDataset), (0.7 * nrow(hourDataset))),]
testset5 = subset(hourDataset, !(hourDataset$index %in% trainset5$index))
# 6) 75% - 25%
trainset6 = hourDataset[sample(nrow(hourDataset), (0.75 * nrow(hourDataset))),]
testset6 = subset(hourDataset, !(hourDataset$index %in% trainset6$index))
#remove index column
trainset2$index = NULL
testset2$index = NULL
trainset3$index = NULL
testset3$index = NULL
trainset4$index = NULL
testset4$index = NULL
trainset5$index = NULL
testset5$index = NULL
trainset6$index = NULL
testset6$index = NULL
#-------------------------------------------------------------------------------

################################################################################



######################### BAYESIAN NETWORK STRUCTURE ###########################
#infer/manually choose net structure

#3 methods' families:
#1) Constraint-Based V
# 1.1) PC --> d-separation
# 1.2) GS = Grow Shrink --> markov blanket
# 1.3) IAMB --> 2 phases (forward and backward)
# 1.4) Fast-IAMB
# 1.5) Inter-IAMB
#2) Score-Based X
# 2.1) HC --> hill climbing
# 2.2) tabu --> tabu search (modified hill climbing)
# 2.x) HC starting from 1) models
# 2.y) tabu starting from 1) models
#3) Hybrid V
# 3.1) MMHC --> Max-Min hill climbing
# 3.2) rsmax2 --> restricted maximization

#OBS: using of bootstrap with boot.strength function with threshold = 0.5
#--> method that repeat different iteration to assess edges and their direction
#--> then find the averaged network

#----------------------------- 1)Constraint-based ------------------------------
#1.1) PC 
boot = boot.strength(trainset, R = 500, algorithm = "pc.stable")
bnet_pc = averaged.network(boot, threshold = 0.5)
#1.2) GS
boot = boot.strength(trainset, R = 500, algorithm = "gs")
bnet_gs = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "gs",
                      algorithm.args = list(optimized = T))
bnet_gs_opt = averaged.network(boot, threshold = 0.5)
#1.3) IAMB
boot = boot.strength(trainset, R = 500, algorithm = "iamb")
bnet_iamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "iamb",
                     algorithm.args = list(optimized = T))
bnet_iamb_opt = averaged.network(boot, threshold = 0.5)
#1.4) Fast-IAMB
boot = boot.strength(trainset, R = 500, algorithm = "fast.iamb")
bnet_fiamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "fast.iamb",
                     algorithm.args = list(optimized = T))
bnet_fiamb_opt = averaged.network(boot, threshold = 0.5)
#1.5) Inter-IAMB
boot = boot.strength(trainset, R = 500, algorithm = "inter.iamb")
bnet_iiamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "inter.iamb",
                     algorithm.args = list(optimized = T))
bnet_iiamb_opt = averaged.network(boot, threshold = 0.5)
#-------------------------------------------------------------------------------


#-------------------------------- Score-based ----------------------------------
#2.1) HC
boot = boot.strength(trainset, R = 500, algorithm = "hc")
bnet_hc = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(optimized = T))
bnet_hc_opt = averaged.network(boot, threshold = 0.5)
#2.2) tabu
boot = boot.strength(trainset, R = 500, algorithm = "tabu")
bnet_tabu = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(optimized = T))
bnet_tabu_opt = averaged.network(boot, threshold = 0.5)
#OBS: is possibile using a parameter 'start' to use a DAG as start model for 
# the algorithm)
#BUT: start dag has to be directed
#directs undirected edge
#PC:
bnet_pc = set.arc(bnet_pc, "season", "mnth")
bnet_pc = set.arc(bnet_pc, "temp", "atemp")
bnet_pc = set.arc(bnet_pc, "holiday", "workingday")
#GS
bnet_gs = set.arc(bnet_gs, "hr", "cnt")
#GS optimized
bnet_gs_opt = set.arc(bnet_gs_opt, "holiday", "workingday")
#IAMB
bnet_iamb = set.arc(bnet_iamb, "weathersit", "hum")
bnet_iamb = set.arc(bnet_iamb, "weekday", "workingday")
#IAMB optimized
bnet_iamb_opt = set.arc(bnet_iamb_opt, "weekday", "workingday")
bnet_iamb_opt = set.arc(bnet_iamb_opt, "holiday", "workingday")
#fast-IAMB
bnet_fiamb = set.arc(bnet_fiamb, "weekday", "workingday")
#fast-IAMB optimized
bnet_fiamb_opt = set.arc(bnet_fiamb_opt, "holiday", "workingday")
#inter-IAMB
bnet_iiamb = set.arc(bnet_iiamb, "weathersit", "hum")
bnet_iiamb = set.arc(bnet_iiamb, "weekday", "workingday")
#inter-IAMB optimized --> yet good
#2.3) HC on pc
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_pc))
bnet_hc_pc = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_pc, optimized = T))
bnet_hc_pc_opt = averaged.network(boot, threshold = 0.5)
#2.4) HC on gs
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_gs))
bnet_hc_gs = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_gs_opt, optimized = T))
bnet_hc_gs_opt = averaged.network(boot, threshold = 0.5)
#2.5) HC on IAMB
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_iamb))
bnet_hc_iamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_iamb, optimized = T))
bnet_hc_iamb_opt = averaged.network(boot, threshold = 0.5)
#2.6) HC on fast-IAMB
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_fiamb))
bnet_hc_fiamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_fiamb, optimized = T))
bnet_hc_fiamb_opt = averaged.network(boot, threshold = 0.5)
#2.7) HC on inter-IAMB
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_iiamb))
bnet_hc_iiamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "hc",
                     algorithm.args = list(start = bnet_iiamb, optimized = T))
bnet_hc_iiamb_opt = averaged.network(boot, threshold = 0.5)
#2.8) tabu on pc
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_pc))
bnet_tabu_pc = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_pc, optimized = T))
bnet_tabu_pc_opt = averaged.network(boot, threshold = 0.5)
#2.9) tabu on gs
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_gs))
bnet_tabu_gs = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_gs_opt, optimized = T))
bnet_tabu_gs_opt = averaged.network(boot, threshold = 0.5)
#2.10) tabu on IAMB
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_iamb))
bnet_tabu_iamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_iamb, optimized = T))
bnet_tabu_iamb_opt = averaged.network(boot, threshold = 0.5)
#2.11) tabu on fast-IAMB
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_fiamb))
bnet_tabu_fiamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_fiamb, optimized = T))
bnet_tabu_fiamb_opt = averaged.network(boot, threshold = 0.5)
#2.12) tabu on inter-IAMB
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_iiamb))
bnet_tabu_iiamb = averaged.network(boot, threshold = 0.5)
boot = boot.strength(trainset, R = 500, algorithm = "tabu",
                     algorithm.args = list(start = bnet_iiamb, optimized = T))
bnet_tabu_iiamb_opt = averaged.network(boot, threshold = 0.5)
#-------------------------------------------------------------------------------


#---------------------------------- 3)Hybrid -----------------------------------
#3.1) MMHC (rsmax2 with restrict = mmpc and maximize = hc)
boot = boot.strength(trainset, R = 500, algorithm = "mmhc")
bnet_mmhc = averaged.network(boot, threshold = 0.5)
#3.2) rsmax2
boot = boot.strength(trainset, R = 500, algorithm = "rsmax2")
bnet_rsmax2 = averaged.network(boot, threshold = 0.5)
#OBS: different models
#-------------------------------------------------------------------------------


#-------------------------------- FINAL MODEL ----------------------------------
#Find a criterion to test each model, choose the best and use domain knowledge
# other models results and correlation to modify it to achieve the final model

#bnet_iiamb_opt looks good
bnet = bnet_iiamb_opt
#OBS: is only an EXAMPLE and every iteration of precedent algorithm could
# return different model --> next line change every iteration
#At the iteration used to get the final model the net only need to:
# - connect workingday and atemp to cnt (domain knowledge + correlation)
#(NOT mnth and season cause they will be parents of temp/atemp)
bnet = set.arc(bnet, "workingday", "cnt")
bnet = set.arc(bnet, "atemp", "cnt")
# - remove workingday-->mnth edge and weekday-->holiday
bnet = drop.arc(bnet, "workingday", "mnth")
bnet = drop.arc(bnet, "weekday", "holiday")
# - reverse hr-->cnt as edge direction (domain knowledge)
bnet = reverse.arc(bnet, "cnt", "hr")
# - reverse atemp-->hum as edge direction (domain knowledge)
bnet = reverse.arc(bnet, "atemp", "hum")
# - reverse hum-->hr as edge direction (domain knowledge)
bnet = reverse.arc(bnet, "hum", "hr")
# - set holiday-->workingday as edge direction ()
bnet = reverse.arc(bnet, "workingday", "holiday")
# - reverse temp-->mnth as edge direction (domain knowledge) ans mnth-->season
bnet = reverse.arc(bnet, "temp", "mnth")
bnet = reverse.arc(bnet, "mnth", "season")
# - set temp-->atemp as edge direction (domain knowledge + correlation)
bnet = set.arc(bnet, "temp", "atemp")
# - set windspeed-->atemp (correlation+domain knowledge)
bnet = set.arc(bnet, "windspeed", "atemp")
#OBS: undirected edge A -- B represented as 2 edges ("A","B") and ("B","A"),
# directed edge A --> B represented as ("A","B")

#try some of the score based builded starting from constraints based
#the structure of bnet_tabu_iamb_opt is pretty recurrent as net in 
# the score-based algorithm which start from a precedent model and not only.
# It also have a lot of direct arcs and need a low number of change.
# Also hc net on inter-iamb is a lot recurrent, but has less directed edges
# --> preferred bnet_tabu_iamb_opt
bnet1 = bnet_tabu_iamb_opt 
bnet1 = reverse.arc(bnet1, "cnt", "hr") #to be reverted
bnet1 = set.arc(bnet1, "atemp", "cnt") #unique new edge
bnet1 = set.arc(bnet1, "hum", "atemp") #indirected
#bnet = bnet1

#old model:
bnet2 = bnet_iiamb
#set mnt-->temp, mnth-->hum, hr-->cnt, workingday-->cnt, atemp-->cnt, 
# hum-->atemp, temp-->atemp, windspeed-->atemp, holiday-->workingday
# and then remove weekday-->holiday
# (domain knowledge+others net result+correlation)
bnet2 = set.arc(bnet2, "mnth", "temp") #reverse
bnet2 = set.arc(bnet2, "mnth", "hum") #new edge
bnet2 = set.arc(bnet2, "hr", "cnt") #reverse
bnet2 = set.arc(bnet2, "workingday", "cnt") #new edge
bnet2 = set.arc(bnet2, "atemp", "cnt") #new edge
bnet2 = set.arc(bnet2, "hum", "atemp") #new edge
bnet2 = set.arc(bnet2, "temp", "atemp") #new edge
bnet2 = set.arc(bnet2, "windspeed", "atemp") #new edge
bnet2 = set.arc(bnet2, "holiday", "workingday") #reverse
bnet2 = drop.arc(bnet2, "weekday", "holiday") #drop (meaningless)

#windspeed is useless (see plots)
bnet$nodes$windspeed = NULL
model.string = modelstring(bnet)
model.string = gsub(pattern = ":windspeed", replacement = "", model.string)
bnet = model2network(model.string)
hourDataset$windspeed = NULL
trainset$windspeed = NULL
trainset1$windspeed = NULL
testset1$windspeed = NULL
trainset2$windspeed = NULL
testset2$windspeed = NULL
trainset3$windspeed = NULL
testset3$windspeed = NULL
trainset4$windspeed = NULL
testset4$windspeed = NULL
trainset5$windspeed = NULL
testset5$windspeed = NULL
trainset6$windspeed = NULL
testset6$windspeed = NULL
#-------------------------------------------------------------------------------

################################################################################



######################## BAYESIAN NETWORK VISUALIZATION ########################
#layouts: "dot", "neato", "twopi", "circo", "fdp"

#Constraints-based nets
graphviz.plot(bnet_pc, layout = "dot", main = "PC net")
graphviz.plot(bnet_gs, layout = "dot", main = "GS net")
graphviz.plot(bnet_gs_opt, layout = "dot", main = "GS net optimized")
graphviz.plot(bnet_iamb, layout = "dot", main = "IAMB net")
graphviz.plot(bnet_iamb_opt, layout = "dot", main = "IAMB net optimized")
graphviz.plot(bnet_fiamb, layout = "dot", main = "Fast-IAMB net")
graphviz.plot(bnet_fiamb_opt, layout = "dot", 
              main = "Fast-IAMB net optimized")
graphviz.plot(bnet_iiamb, layout = "dot", main = "Inter-IAMB net")
graphviz.plot(bnet_iiamb_opt, layout = "dot", 
              main = "Inter-IAMB net optimized")

#Score-based nets
graphviz.plot(bnet_hc, layout = "fdp", main = "HC net")
graphviz.plot(bnet_hc_opt, layout = "fdp", main = "HC net optimized")
graphviz.plot(bnet_tabu, layout = "fdp", main = "tabu net")
graphviz.plot(bnet_tabu_opt, layout = "fdp", main = "tabu net optimized")

#Score-based nets starting from constraints-based nets
graphviz.plot(bnet_hc_pc, layout = "fdp", main = "HC net on pc")
graphviz.plot(bnet_hc_pc_opt, layout = "fdp", main = "HC net optimized on GS")
graphviz.plot(bnet_hc_gs, layout = "fdp", main = "HC net on gs")
graphviz.plot(bnet_hc_gs_opt, layout = "fdp", main = "HC net optimized on GS")
graphviz.plot(bnet_hc_iamb, layout = "fdp", main = "HC net on iamb")
graphviz.plot(bnet_hc_iamb_opt, layout = "fdp", 
              main = "HC net optimized on IAMB")
graphviz.plot(bnet_hc_fiamb, layout = "fdp", main = "HC net on fast-IAMB")
graphviz.plot(bnet_hc_fiamb_opt, layout = "fdp", 
              main = "HC net optimized on fast-IAMB")
graphviz.plot(bnet_hc_iiamb, layout = "fdp", main = "HC net on inter-IAMB")
graphviz.plot(bnet_hc_iiamb_opt, layout = "fdp", 
              main = "HC net optimized on inter-IAMB")
graphviz.plot(bnet_tabu_pc, layout = "fdp", main = "tabu net on PC")
graphviz.plot(bnet_tabu_pc_opt, layout = "fdp", 
              main = "tabu net optimized on PC")
graphviz.plot(bnet_tabu_gs, layout = "fdp", main = "tabu net on GS")
graphviz.plot(bnet_tabu_gs_opt, layout = "fdp", 
              main = "tabu net optimized on GS")
graphviz.plot(bnet_tabu_iamb, layout = "fdp", main = "tabu net on IAMB")
graphviz.plot(bnet_tabu_iamb_opt, layout = "fdp", 
              main = "tabu net optimized on IAMB")
graphviz.plot(bnet_tabu_fiamb, layout = "fdp", main = "tabu net on fast-IAMB")
graphviz.plot(bnet_tabu_fiamb_opt, layout = "fdp", 
              main = "tabu net optimized on fast-IAMB")
graphviz.plot(bnet_tabu_iiamb, layout = "fdp", main = "tabu net on inter-IAMB")
graphviz.plot(bnet_tabu_iiamb_opt, layout = "fdp",
              main = "tabu net optimized on inter-IAMB")


#Hybrid nets
graphviz.plot(bnet_mmhc, layout = "dot", main = "MMHC net")
graphviz.plot(bnet_rsmax2, layout = "dot", main = "rsmax2 net")

#Final net
graphviz.plot(bnet, layout = "dot", main = "Final net", 
              highlight = list(nodes = "cnt", col = "green", fill = "green"))

################################################################################



######################### BAYESIAN NETWORK PARAMETERS ##########################
#infer CPTs --> 2 ways:
# 1) mle = maximum likelihood estimation
# 2) bayes = posterior bayesian estimation

#1) mle
bnet_cpts_mle = bn.fit(x = bnet, data = trainset, method = "mle")
#2) bayes
bnet_cpts_bayes = bn.fit(x = bnet, data = trainset, method = "bayes")
#OBS: they are fitted object, contains CPTs and the graph model, if you plot 
# them the plot will be the graph
#OBS: bayes method avoid NaN probabilities result when querying

################################################################################



######################## NAIVE-BAYES NETWORK STRUCTURE #########################
#infer/manually choose net structure
bnet_naive = naive.bayes(x = trainset, training = "cnt")

################################################################################



###################### NAIVE-BAYES NETWORK VISUALIZATION #######################
#plot naive bayes net structure (layouts: dot, neato, twopi, circo, fdp)
graphviz.plot(bnet_naive, layout = "dot", main = "Naive-Bayes net",
              highlight = list(nodes = "cnt", col = "green", fill = "green"))

################################################################################



######################## NAIVE BAYES NETWORK PARAMETERS ########################
#infer CPTs

#1) MLE - maximum likelihood estimation
bnet_naive_cpts_mle = bn.fit(x = bnet_naive, data = trainset, method = "mle")
#2) posterior bayesian estimation
bnet_naive_cpts_bayes = bn.fit(x = bnet_naive, data = trainset, method = "bayes")

################################################################################



############################ EXAMPLE OF INFERENCE ##############################
#Inference in the net
#1) exact --> junction tree
#2) approximate --> logic sampling OR likelihood weighting
# 2.1) get probability distribution for a single variable
# 2.2) get probability for a specific event

#1) junction tree
#create junction tree
bnet_jtree = compile(as.grain(bnet_cpts_mle))
#add evidence
jtree_evidence = setEvidence(bnet_jtree, nodes = "hr", states = "7")
#make query
result = querygrain(jtree_evidence, nodes = "cnt")
probs_result = result$cnt
arg_result = names(which.max(result$cnt))

#2) logic sampling
# 2.1) get probability distribution for target variable
#logic sampling
#(nodes could be a list)
ls_query = cpdist(bnet_cpts_mle, nodes = "cnt",
                  evidence = (hr == "1") & (temp == "2"), method = "ls")
ls_result_prob = prop.table(table(ls_query))
#likelihood weighting
lw_query = cpdist(bnet_cpts_mle, nodes = "cnt", 
                  evidence = list(hr = "1", temp = "2"), method = "lw")
lw_result_prob = prop.table(table(lw_query))
# 2.2) get single probability for an event
#logic sampling
ls_result_prob2 = cpquery(bnet_cpts_mle, event = (cnt == "4"), 
                          evidence = (hr == "1") & (temp == "2"), method = "ls")
#likelihood weighting (if result = NA --> number too much little)
lw_result_prob2 = cpquery(bnet_cpts_mle, event = (cnt == "1"),
                          evidence = list(hr = "1", temp = "2"), method = "lw")
#OBS: cpquery with LW is useless, a disaster, never compute a right result

################################################################################



######################## PREDICTION OF TARGET VARIABLE #########################

#---------------------------------- FUNCTIONS ----------------------------------

#function bnet_prediction:
# input = a bayesian network, a training set and a test set 
# output = prediction for the target variable on the test set
bnet_prediction = function(net, train, test) {
  predictions = c()
  #1)compute CPTs
  #cpts = bn.fit(x = net, data = train, method = "mle")
  cpts = bn.fit(x = net, data = train, method = "bayes")
  #2)inference and prediction
  #inference engine
  jtree = compile(as.grain(cpts))
  #prediction on test set
  evidence_names = colnames(test[,1:(ncol(test)-1)]) #removing 'cnt' (target)
  for(i in 1:nrow(test)){ #for each sample
    evidence_values = c()
    for(j in 1:length(evidence_names)){#for each feature
      evidence_values = append(evidence_values, as.character(test[i,j]))
    }
    #add evidence to the engine
    ev_jtree = setEvidence(jtree, nodes = evidence_names, 
                           states = evidence_values)
    #predict "cnt"
    cnt = names(which.max(querygrain(ev_jtree, nodes = "cnt")$cnt))
    if(is.null(cnt)){#not occurring for the moment
      cnt = "1" #most prior probable
      print(paste0("error: ", i)) 
    }
    predictions = append(predictions, cnt)
  }
  #return the prediction for the test set
  return(predictions)
}

#function to calculate accuracy of a confusion matrix
calc_accuracy = function(cm) {
  return(sum(diag(cm))/sum(cm))
}

#function to calculate precision of a confusion matrix
calc_precision = function(cm) {
  i = 1
  p = 0
  tot = nrow(cm)
  for(i in 1:tot) {
    p = p + cm[i,i]/sum(cm[,i])
  }
  return(p/tot)
}

#function to calculate recall of a confusion matrix
calc_recall = function(cm) {
  i = 1
  p = 0
  tot = nrow(cm)
  for(i in 1:tot) {
    p = p + cm[i,i]/sum(cm[i,])
  }
  return(p/tot)
}

#function to calculate f-measure of a confusion matrix
calc_fmeasure = function(precision, recall) {
  return((2 * (precision * recall)) / (precision + recall))
}

#-------------------------------------------------------------------------------

#Test bnet and bnet_naive with different train and testset

#------------------------------ BAYESIAN NETWORK -------------------------------

#testset1
bnet_test1 = bnet_prediction(bnet, trainset1, testset1)
bnet_test1_cm = table(testset1$cnt, bnet_test1)
bnet_test1_accuracy = calc_accuracy(bnet_test1_cm)
bnet_test1_precision = calc_precision(bnet_test1_cm)
bnet_test1_recall = calc_recall(bnet_test1_cm)
bnet_test1_fmeasure = calc_fmeasure(bnet_test1_precision, bnet_test1_recall)
#testset2
bnet_test2 = bnet_prediction(bnet, trainset2, testset2)
bnet_test2_cm = table(testset2$cnt, bnet_test2)
bnet_test2_accuracy = calc_accuracy(bnet_test2_cm)
bnet_test2_precision = calc_precision(bnet_test2_cm)
bnet_test2_recall = calc_recall(bnet_test2_cm)
bnet_test2_fmeasure = calc_fmeasure(bnet_test2_precision, bnet_test2_recall)
#testset3
bnet_test3 = bnet_prediction(bnet, trainset3, testset3)
bnet_test3_cm = table(testset3$cnt, bnet_test3)
bnet_test3_accuracy = calc_accuracy(bnet_test3_cm)
bnet_test3_precision = calc_precision(bnet_test3_cm)
bnet_test3_recall = calc_recall(bnet_test3_cm)
bnet_test3_fmeasure = calc_fmeasure(bnet_test3_precision, bnet_test3_recall)
#testset4
bnet_test4 = bnet_prediction(bnet, trainset4, testset4)
bnet_test4_cm = table(testset4$cnt, bnet_test4)
bnet_test4_accuracy = calc_accuracy(bnet_test4_cm)
bnet_test4_precision = calc_precision(bnet_test4_cm)
bnet_test4_recall = calc_recall(bnet_test4_cm)
bnet_test4_fmeasure = calc_fmeasure(bnet_test4_precision, bnet_test4_recall)
#testset5
bnet_test5 = bnet_prediction(bnet, trainset5, testset5)
bnet_test5_cm = table(testset5$cnt, bnet_test5)
bnet_test5_accuracy = calc_accuracy(bnet_test5_cm)
bnet_test5_precision = calc_precision(bnet_test5_cm)
bnet_test5_recall = calc_recall(bnet_test5_cm)
bnet_test5_fmeasure = calc_fmeasure(bnet_test5_precision, bnet_test5_recall)
#testset6
bnet_test6 = bnet_prediction(bnet, trainset6, testset6)
bnet_test6_cm = table(testset6$cnt, bnet_test6)
bnet_test6_accuracy = calc_accuracy(bnet_test6_cm)
bnet_test6_precision = calc_precision(bnet_test6_cm)
bnet_test6_recall = calc_recall(bnet_test6_cm)
bnet_test6_fmeasure = calc_fmeasure(bnet_test6_precision, bnet_test6_recall)
#all tests:
bnet_accs = c(bnet_test1_accuracy, bnet_test2_accuracy, bnet_test3_accuracy, 
              bnet_test4_accuracy, bnet_test5_accuracy, bnet_test6_accuracy)

#-------------------------------------------------------------------------------


#--------------------------- NAIVE BAYESIAN NETWORK ----------------------------

#testset1
bnet_naive_test1 = bnet_prediction(bnet_naive, trainset1, testset1)
bnet_naive_test1_cm = table(testset1$cnt, bnet_naive_test1)
bnet_naive_test1_accuracy = calc_accuracy(bnet_naive_test1_cm)
bnet_naive_test1_precision = calc_precision(bnet_naive_test1_cm)
bnet_naive_test1_recall = calc_recall(bnet_naive_test1_cm)
bnet_naive_test1_fmeasure = calc_fmeasure(bnet_naive_test1_precision,
                                          bnet_naive_test1_recall)
#testset2
bnet_naive_test2 = bnet_prediction(bnet_naive, trainset2, testset2)
bnet_naive_test2_cm = table(testset2$cnt, bnet_naive_test2)
bnet_naive_test2_accuracy = calc_accuracy(bnet_naive_test2_cm)
bnet_naive_test2_precision = calc_precision(bnet_naive_test2_cm)
bnet_naive_test2_recall = calc_recall(bnet_naive_test2_cm)
bnet_naive_test2_fmeasure = calc_fmeasure(bnet_naive_test2_precision,
                                          bnet_naive_test2_recall)
#testset3
bnet_naive_test3 = bnet_prediction(bnet_naive, trainset3, testset3)
bnet_naive_test3_cm = table(testset3$cnt, bnet_naive_test3)
bnet_naive_test3_accuracy = calc_accuracy(bnet_naive_test3_cm)
bnet_naive_test3_precision = calc_precision(bnet_naive_test3_cm)
bnet_naive_test3_recall = calc_recall(bnet_naive_test3_cm)
bnet_naive_test3_fmeasure = calc_fmeasure(bnet_naive_test3_precision, 
                                          bnet_naive_test3_recall)
#testset4
bnet_naive_test4 = bnet_prediction(bnet_naive, trainset4, testset4)
bnet_naive_test4_cm = table(testset4$cnt, bnet_naive_test4)
bnet_naive_test4_accuracy = calc_accuracy(bnet_naive_test4_cm)
bnet_naive_test4_precision = calc_precision(bnet_naive_test4_cm)
bnet_naive_test4_recall = calc_recall(bnet_naive_test4_cm)
bnet_naive_test4_fmeasure = calc_fmeasure(bnet_naive_test4_precision, 
                                          bnet_naive_test4_recall)
#testset5
bnet_naive_test5 = bnet_prediction(bnet_naive, trainset5, testset5)
bnet_naive_test5_cm = table(testset5$cnt, bnet_naive_test5)
bnet_naive_test5_accuracy = calc_accuracy(bnet_naive_test5_cm)
bnet_naive_test5_precision = calc_precision(bnet_naive_test5_cm)
bnet_naive_test5_recall = calc_recall(bnet_naive_test5_cm)
bnet_naive_test5_fmeasure = calc_fmeasure(bnet_naive_test5_precision, 
                                          bnet_naive_test5_recall)
#testset6
bnet_naive_test6 = bnet_prediction(bnet_naive, trainset6, testset6)
bnet_naive_test6_cm = table(testset6$cnt, bnet_naive_test6)
bnet_naive_test6_accuracy = calc_accuracy(bnet_naive_test6_cm)
bnet_naive_test6_precision = calc_precision(bnet_naive_test6_cm)
bnet_naive_test6_recall = calc_recall(bnet_naive_test6_cm)
bnet_naive_test6_fmeasure = calc_fmeasure(bnet_naive_test6_precision, 
                                          bnet_naive_test6_recall)
#all tests
bnet_naive_accs = c(bnet_naive_test1_accuracy, bnet_naive_test2_accuracy, 
                    bnet_naive_test3_accuracy, bnet_naive_test4_accuracy, 
                    bnet_naive_test5_accuracy, bnet_naive_test6_accuracy)

#-------------------------------------------------------------------------------

################################################################################



#################### VALIDATION AND PERFORMANCE EVALUATION #####################


#--------------------- performance at growing training set ---------------------

#plot bnet vs bnet_naive performance at growing training set size
plot(bnet_accs, col = "red", type = "b", ylim = c(0.65, 0.9), lwd = 1.5,
     main = "Bayesian network VS Naive Bayes performance", xaxt = "n",
     xlab = "Training set size (% of dataset size)", ylab = "Accuracy")
axis(1, at=1:6, labels = c("50", "55", "60", "65", "70", "75"))
lines(bnet_naive_accs, col = "blue", type = "b")
grid(NULL, NULL, lty = 5)
legend("bottom", legend = c("bnet", "bnet_naive"), col = c("red", "blue"),
       lwd = 1.5, pch = 1, bty = "n")

#-------------------------------------------------------------------------------


#-------------------------- 5-fold cross validation --------------------------- 
#Validate and compute performance with 5-fold cross validation on both the 
# models (bnet and bnet_naive)
#OBS: k=5 and not k=10 to keep the size of the test set significant. 
#     --> avoid testset without some class (cnt=4 for example)

set.seed(42)
#Randomly shuffle the data
data = trainset[sample(nrow(trainset)),]
#Create 5 equally size folds
folds = cut(seq(1, nrow(trainset)), breaks=5, labels=FALSE)
#Create accuracies array
accuracies_bn = rep(0, 5)
precisions_bn = rep(0, 5)
recalls_bn = rep(0, 5)
fmeasures_bn = rep(0, 5)
accuracies_nbn = rep(0, 5)
precisions_nbn = rep(0, 5)
recalls_nbn = rep(0, 5)
fmeasures_nbn = rep(0, 5)

#Perform 5 fold cross validation
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes = which(folds == i, arr.ind = TRUE)
  testData = trainset[testIndexes, ]
  trainData = trainset[-testIndexes, ]
  #train the model and predict the target variable 'cnt'
  prediction_bn = bnet_prediction(net = bnet, 
                                  train = trainData, test = testData)
  prediction_nbn = bnet_prediction(net = bnet_naive, 
                                   train = trainData, test = testData)
  #check accuracy, precision, recall and fmeasure
  #bn
  cm_bn = table(testData$cnt, prediction_bn)
  if(ncol(cm_bn)<4){
    print(paste0("iterazione: ", i, ", bnet"))
    print(cm_bn)
    cm_temp = matrix(0,4,4)
    for(i in 1:ncol(cm_bn)){
      cm_temp[,i] = cm_bn[,i] 
    }
    cm_bn = cm_temp
    print(cm_bn)
  }
  accuracy_bn = calc_accuracy(cm_bn)
  precision_bn = calc_precision(cm_bn)
  recall_bn = calc_recall(cm_bn)
  fmeasure_bn = calc_fmeasure(precision_bn, recall_bn)
  #nbn
  cm_nbn = table(testData$cnt, prediction_nbn)
  if(ncol(cm_nbn)<4){
    print(paste0("iterazione: ", i, ", bnet"))
    print(cm_bn)
    cm_temp = matrix(0,4,4)
    for(i in 1:ncol(cm_nbn)){
      cm_temp[,i] = cm_nbn[,i] 
    }
    cm_nbn = cm_temp
    print(cm_bn)
  }
  accuracy_nbn = calc_accuracy(cm_nbn)
  precision_nbn = calc_precision(cm_nbn)
  recall_nbn = calc_recall(cm_nbn)
  fmeasure_nbn = calc_fmeasure(precision_nbn, recall_nbn)
  #add accuracy, precision, recall and fmeasure to the relative list
  accuracies_bn[i] = accuracy_bn
  precisions_bn[i] = precision_bn
  recalls_bn[i] = recall_bn
  fmeasures_bn[i] = fmeasure_bn
  accuracies_nbn[i] = accuracy_nbn
  precisions_nbn[i] = precision_nbn
  recalls_nbn[i] = recall_nbn
  fmeasures_nbn[i] = fmeasure_nbn
}

#mean of accuracies, precisions, recalls and fmeasures
bnet_mean_accuracy = mean(accuracies_bn)
bnet_mean_precision = mean(precisions_bn)
bnet_mean_recalls = mean(recalls_bn)
bnet_mean_fmeasure = mean(fmeasures_bn)
bnet_naive_mean_accuracy = mean(accuracies_nbn)
bnet_naive_mean_precision = mean(precisions_nbn)
bnet_naive_mean_recalls = mean(recalls_nbn)
bnet_naive_mean_fmeasure = mean(fmeasures_nbn)

cat_vect = rep("bnet", 4)
cat_vect = append(cat_vect, rep("bnet_naive", 4))
par_vect = c("accuracy", "precision", "recall", "fmeasure", 
             "accuracy", "precision", "recall", "fmeasure")
val_vect = c(bnet_mean_accuracy, bnet_mean_precision, 
             bnet_mean_recalls, bnet_mean_fmeasure,
             bnet_naive_mean_accuracy, bnet_naive_mean_precision, 
             bnet_naive_mean_recalls, bnet_naive_mean_fmeasure)
plot_df = data.frame(cat_vect, par_vect, val_vect)

ggplot(plot_df, aes(factor(par_vect, levels=c("accuracy", "precision", 
                                              "recall", "fmeasure")), 
                    val_vect, fill = cat_vect)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + 
  labs(fill = "Model:") + 
  labs(x = "performance parameter") + 
  labs(y = "value") +
  labs(title = "Performance evaluation after 5-fold cross validation")

#-------------------------------------------------------------------------------


#--------------------------------- ROC curves ----------------------------------
#ROC curves for both the models

#OBS: Use one vs all approach to do ROC curve on a multiclass classifier.
#Consider one class like 1 and other class like 0, do it for all class and
# compare the results

#ROC curve for bnet
#plot all the single curve together to compare them
#array for auc means
aucs = c()
#empty plot
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate', xlab='False Positive Rate', 
     main = "ROC for bnet (trainset: 70%, testset: 30%)", 
     sub = "1 class vs all the others classes", bty='n')
for(category in 1:4) {
  #calcolate prediction as 1 vs all, so hold 1 class and set it as 1 and 
  # the others to 0:
  testset.cnt = as.numeric(testset5$cnt)
  tot = length(bnet_test5)
  temp_pred = rep(1, tot)
  i=1
  for(i in 1:tot) {
    #set to 0 other class in prediction and hold as 1 only prediction = category
    if(bnet_test5[i] != category) {
      temp_pred[i] = 0
    }
    #set to 0 other class in testset and hold as 1 only cnt = category
    if(testset.cnt[i] != category) {
      testset.cnt[i] = 0
    }
  }
  pred = ROCR::prediction(temp_pred, testset.cnt)
  #calcolate roc curve
  tpr_rocr = performance(pred, measure = "tpr","fpr")
  roc.x = unlist(tpr_rocr@x.values)
  roc.y = unlist(tpr_rocr@y.values)
  lines(roc.y ~ roc.x, col=category+1, lwd=2)
  #calcolate auc
  rocr = performance(pred, measure = "auc", x.measure = "cutoff")
  rocr = unlist(slot(rocr, "y.values"))
  aucs[category] = rocr
}
auc = mean(aucs) #means of every auc
lines(x=c(0,1), c(0,1))
grid(NULL, NULL, lty = 5)
legend("bottomright", col = c(2, 3, 4, 5), pch = 1, lwd = 1, title = "#bikes: ",
       legend = c(paste0("very low -"," AUC:", 
                         as.character(round(aucs[1], digits = 2))), 
                  paste0("low -"," AUC:", 
                         as.character(round(aucs[2], digits = 2))),
                  paste0("medium -"," AUC:", 
                         as.character(round(aucs[3], digits = 2))),
                  paste0("high -"," AUC:", 
                         as.character(round(aucs[4], digits = 2)))))
legend(x = 0.25, y = 0.15, bty = "n", 
       legend = paste0("AUC mean: ", as.character(round(auc, digits = 2))))

#ROC curve for bnet_naive
#plot all the single curve together to compare them
#array for auc means
aucs = c()
#empty plot
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate', xlab='False Positive Rate', 
     main = "ROC for bnet_naive (trainset: 70%, testset: 30%)", 
     sub = "1 class vs all the others classes", bty='n')
for(category in 1:4) {
  #calcolate prediction as 1 vs all, so hold 1 class and set it as 1 and 
  # the others to 0:
  testset.cnt = as.numeric(testset5$cnt)
  tot = length(bnet_naive_test5)
  temp_pred = rep(1, tot)
  i=1
  for(i in 1:tot) {
    #set to 0 other class in prediction and hold as 1 only prediction = category
    if(bnet_naive_test5[i] != category) {
      temp_pred[i] = 0
    }
    #set to 0 other class in testset and hold as 1 only cnt = category
    if(testset.cnt[i] != category) {
      testset.cnt[i] = 0
    }
  }
  pred = ROCR::prediction(temp_pred, testset.cnt)
  #calcolate roc curve
  tpr_rocr = performance(pred, measure = "tpr","fpr")
  roc.x = unlist(tpr_rocr@x.values)
  roc.y = unlist(tpr_rocr@y.values)
  lines(roc.y ~ roc.x, col=category+1, lwd=2)
  #calcolate auc
  rocr = performance(pred, measure = "auc", x.measure = "cutoff")
  rocr = unlist(slot(rocr, "y.values"))
  aucs[category] = rocr
}
auc = mean(aucs) #means of every auc
lines(x=c(0,1), c(0,1))
grid(NULL, NULL, lty = 5)
legend("bottomright", col = c(2, 3, 4, 5), pch = 1, lwd = 1, title = "#bikes: ",
       legend = c(paste0("very low -"," AUC:", 
                         as.character(round(aucs[1], digits = 2))), 
                  paste0("low -"," AUC:", 
                         as.character(round(aucs[2], digits = 2))),
                  paste0("medium -"," AUC:", 
                         as.character(round(aucs[3], digits = 2))),
                  paste0("high -"," AUC:", 
                         as.character(round(aucs[4], digits = 2)))))
legend(x = 0.25, y = 0.15, bty = "n", 
       legend = paste0("AUC mean: ", as.character(round(auc, digits = 2))))

#-------------------------------------------------------------------------------

################################################################################



################################ EXPORT MODELS #################################

#Export the final models (bnet and naive bnet)
save(list = c("bnet", "bnet_cpts_bayes", "bnet_naive", "bnet_naive_cpts_bayes"),
     file = "Models/bnet.RData")

################################################################################