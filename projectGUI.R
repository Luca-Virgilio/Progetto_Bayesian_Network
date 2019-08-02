################################## LIBRARIES ##################################
#package for gWidgets library 
if (!require("gWidgets2"))
  install.packages("gWidgets2", dependencies = T)
library("gWidgets2")
if (!require("gWidgets2RGtk2"))
  install.packages("gWidgets2RGtk2", dependencies = T)
library("gWidgets2RGtk2")
options(guiToolkit="RGtk2")
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
#png images package
if (!require("png"))
  install.packages("png", dependencies = T)
library("png")
################################################################################



################################## FUNCTIONS ###################################
#set_evidences_and_inference --> exact inference
# input: jtree
# other variables: variables readed from interface --> evidences
# output: results of querying cnt
# procedure:
#  1) adding evidences to the query tree
#  2) infer --> query cnt given the evidences
#  3) plot the probability distribution resulted from the query
set_evidences_and_inference = function(jtree, textbox, net_type) {
  
#----------------------- 1) set evidences for inference ------------------------
  if(svalue(month_value_combobox) != ""){ #month
    month = svalue(month_value_combobox)
    if(month == "January") {
      month = 1
    } else if(month == "February") {
      month = 2
    } else if(month == "March") {
      month = 3
    } else if(month == "April") {
      month = 4
    } else if(month == "May") {
      month = 5
    } else if(month == "June") {
      month = 6
    } else if(month == "July") {
      month = 7
    } else if(month == "August") {
      month = 8
    } else if(month == "September") {
      month = 9
    } else if(month == "October") {
      month = 10
    } else if(month == "November") {
      month = 11
    } else {
      month = 12
    }
    #add month = val as evidence
    jtree = setEvidence(jtree, nodes = "mnth", states = as.character(month))
  }
  if((svalue(season_value_combobox) != "") & 
     (svalue(month_value_combobox) != "")){ #accord season and month
    if(((svalue(month_value_combobox) == "January") |
       (svalue(month_value_combobox) == "February")) &
       (svalue(season_value_combobox) != "Winter")) {
      svalue(season_value_combobox) = "Winter"
      gmessage("Season changed to agree with month" , 
               title = "Season change info", icon = "info")
    } else if(((svalue(month_value_combobox) == "April") |
              (svalue(month_value_combobox) == "May")) &
      (svalue(season_value_combobox) != "Spring")) {
      svalue(season_value_combobox) = "Spring"
      gmessage("Season changed to agree with month" , 
               title = "Season change info", icon = "info")
    } else if(((svalue(month_value_combobox) == "July") |
              (svalue(month_value_combobox) == "August")) &
              (svalue(season_value_combobox) != "Summer")) {
      svalue(season_value_combobox) = "Summer"
      gmessage("Season changed to agree with month" , 
               title = "Season change info", icon = "info")
    } else if(((svalue(month_value_combobox) == "October") |
              (svalue(month_value_combobox) == "November")) &
              (svalue(season_value_combobox) != "Autumn")) {
      svalue(season_value_combobox) = "Autumn"
      gmessage("Season changed to agree with month" , 
               title = "Season change info", icon = "info")
    } else if(svalue(month_value_combobox) == "September"){
      if((svalue(season_value_combobox) != "Summer") | 
         (svalue(season_value_combobox) != "Autumn")){
        svalue(season_value_combobox) = "Summer"
        gmessage("Season changed to agree with month" , 
                 title = "Season change info", icon = "info")
      }
    } else if(svalue(month_value_combobox) == "December"){
      if((svalue(season_value_combobox) != "Winter") & 
         (svalue(season_value_combobox) != "Autumn")){
        svalue(season_value_combobox) = "Autumn"
        gmessage("Season changed to agree with month" , 
                 title = "Season change info", icon = "info")
      } 
    } else if(svalue(month_value_combobox) == "September"){
      if((svalue(season_value_combobox) != "Summer") & 
         (svalue(season_value_combobox) != "Autumn")){
        svalue(season_value_combobox) = "Summer"
        gmessage("Season changed to agree with month" , 
                 title = "Season change info", icon = "info")
      }
    } else if(svalue(month_value_combobox) == "March"){
      if((svalue(season_value_combobox) != "Winter") & 
         (svalue(season_value_combobox) != "Spring")){
        svalue(season_value_combobox) = "Winter"
        gmessage("Season changed to agree with month" , 
                 title = "Season change info", icon = "info")
      }
    } else if(svalue(month_value_combobox) == "June"){
      if((svalue(season_value_combobox) != "Summer") & 
         (svalue(season_value_combobox) != "Spring")){
        svalue(season_value_combobox) = "Spring"
        gmessage("Season changed to agree with month" , 
                 title = "Season change info", icon = "info")
      }
    }
    if(svalue(season_value_combobox) == "Winter"){
      season = 1
    } else if(svalue(season_value_combobox) == "Spring"){
      season = 2
    } else if(svalue(season_value_combobox) == "Summer"){
      season = 3
    } else {
      season = 4
    }
    #add season = val as evidence
    jtree = setEvidence(jtree, nodes = "season", 
                        states = as.character(season))
  } else if(svalue(season_value_combobox) != ""){ #only season
    season = svalue(season_value_combobox)
    if(svalue(season_value_combobox) == "Winter"){
      season = 1
    } else if(svalue(season_value_combobox) == "Spring"){
      season = 2
    } else if(svalue(season_value_combobox) == "Summer"){
      season = 3
    } else {
      season = 4
    }
    #add season = val as evidence
    jtree = setEvidence(jtree, nodes = "season", 
                        states = as.character(season))
  }
  if(svalue(days_value_combobox) != ""){ #weekday
    if(svalue(days_value_combobox) == "Monday"){
      weekday = 1
      workingday = 1
    } else if(svalue(days_value_combobox) == "Tuesday"){
      weekday = 2
      workingday = 1
    } else if(svalue(days_value_combobox) == "Wednesday"){
      weekday = 3
      workingday = 1
    } else if(svalue(days_value_combobox) == "Thursday"){
      weekday = 4
      workingday = 1
    } else if(svalue(days_value_combobox) == "Friday"){
      weekday = 5
      workingday = 1
    } else if(svalue(days_value_combobox) == "Saturday"){
      weekday = 6
      workingday = 0
    } else{
      weekday = 0
      workingday = 0
    }
    #add weekday = val as evidence
    jtree = setEvidence(jtree, nodes = "weekday", 
                        states = as.character(weekday))
    #add workingday = val as evidence
    jtree = setEvidence(jtree, nodes = "workingday", 
                        states = as.character(workingday))
  }
  if(svalue(hol_value_radiobutton) != "Unknown"){
    if(svalue(hol_value_radiobutton) == "Yes"){
      holiday = 1
      workingday = 0
      #change workingday = val as evidence
      jtree = setEvidence(jtree, nodes = "workingday", 
                          states = as.character(workingday))
    } else {
      holiday = 0
    }
    #add holiday = val as evidence
    jtree = setEvidence(jtree, nodes = "holiday", 
                        states = as.character(holiday))
  }
  if(svalue(hour_value_textbox) != ""){
    hour = svalue(hour_value_textbox)
    hour = gsub(",", "\\.", hour)
    hour = gsub(":", "\\.", hour)
    if(is.na(as.numeric(hour))){
      gmessage(paste0("Hour must be a number (23 or 23.10 or 23,10 or 23:10),",
                      "setted as 12.00") , 
               title = "Hour error info", icon = "info")
      hour = 12
      svalue(hour_value_textbox) = 20
    } else {
      hour = as.numeric(hour)
      hour = floor(hour) #flor of hour to the nearest one
      if(hour == 24){
        hour = 0
      } else if(hour > 23) {
        gmessage("Hour can't be more greater then 23! Setted as 00.00" , 
                 title = "Hour error info", icon = "info")
        hour = 0
        svalue(hour_value_textbox) = 0
      } else if(hour < 0) {
        gmessage("Hour can't be a negative value! Setted as 00.00" , 
                 title = "Hour error info", icon = "info")
        hour = 0
        svalue(hour_value_textbox) = 0
      } else {
        #add hour = val as evidence
        jtree = setEvidence(jtree, nodes = "hr", states = as.character(hour))
      }
    }      
  }
  if(svalue(temp_value_textbox) != ""){
    temp = svalue(temp_value_textbox)
    temp = gsub(",", "\\.", temp)
    if(is.na(as.numeric(temp))){
      gmessage("Temp must be a number (30 or 30.10 or 30,10. Setted as 20" , 
               title = "Temp error info", icon = "info")
      temp = 20
      svalue(temp_value_textbox) = 20
    } else {
      temp = as.numeric(temp)
      temp = round(temp, digits = 0) #round to the nearest integer
      #normalization
      if(temp > 39){
        temp = 39
        svalue(temp_value_textbox) = 39
        gmessage("Temp over max registered temp. Changed to max (39°C)." , 
                 title = "Temp info", icon = "info")
      } else if(temp < -8){
        temp = -8
        svalue(temp_value_textbox) = -8
        gmessage("Temp less then min registered temp. Changed to min (-8°C)." , 
                 title = "Temp info", icon = "info")
      }
      temp = (temp-(-8))/(39-(-8))
      interval = 1/9
      if(temp<interval){
        temp = 1
      } else if(temp<2*interval) {
        temp = 2
      } else if(temp<3*interval) {
        temp = 3
      } else if(temp<4*interval) {
        temp = 4
      } else if(temp<5*interval) {
        temp = 5
      } else if(temp<6*interval) {
        temp = 6
      } else if(temp<7*interval) {
        temp = 7
      } else if(temp<8*interval) {
        temp = 8
      } else if(temp<9*interval) {
        temp = 9
      } else {
        temp = 10
      }
      #add temp = val as evidence
      jtree = setEvidence(jtree, nodes = "temp", states = as.character(temp))
    }
  }
  if(svalue(atemp_value_textbox) != ""){
    atemp = svalue(atemp_value_textbox)
    atemp = gsub(",", "\\.", atemp)
    if(is.na(as.numeric(atemp))){
      gmessage(paste0("RealFeel Temp must be a number (eg: 33 or 33.10 or ",
                      "33,10). Setted as 22."), 
               title = "Temp error info", icon = "error")
      atemp = 22
      svalue(atemp_value_textbox) = 22
    } else {
      atemp = as.numeric(atemp)
      atemp = round(atemp, digits = 0) #round to the nearest integer
      #normalization
      if(atemp > 50){
        atemp = 50
        gmessage("Temp over max registered temp. Changed to max." , 
                 title = "Temp info", icon = "info")
        svalue(atemp_value_textbox) = 50
      } else if(atemp < -16){
        atemp = -16
        gmessage("Temp less then min registered temp. Changed to min." , 
                 title = "Temp info", icon = "info")
        svalue(atemp_value_textbox) = -16
      }
      atemp = (atemp-(-16))/(50-(-16))
      interval = 1/12
      if(atemp<interval){
        atemp = 1
      } else if(atemp<2*interval) {
        atemp = 2
      } else if(atemp<3*interval) {
        atemp = 3
      } else if(atemp<4*interval) {
        atemp = 4
      } else if(atemp<5*interval) {
        atemp = 5
      } else if(atemp<6*interval) {
        atemp = 6
      } else if(atemp<7*interval) {
        atemp = 7
      } else if(atemp<8*interval) {
        atemp = 8
      } else if(atemp<9*interval) {
        atemp = 9
      } else if(atemp<10*interval) {
        atemp = 10
      } else if(atemp<11*interval) {
        atemp = 11
      } else if(atemp<12*interval) {
        atemp = 12
      } else {
        atemp = 13
      }
      #add atemp = val as evidence
      jtree = setEvidence(jtree, nodes = "atemp", 
                          states = as.character(atemp))
    }      
  }
  if(svalue(weath_value_combobox) != ""){
    if(svalue(weath_value_combobox) == "Clear or partly cloudy"){
      weathersit = 1
    } else if(svalue(weath_value_combobox) == "Cloudy or mist"){
      weathersit = 2
    } else if(svalue(weath_value_combobox) == "Light rain/snow") {
      weathersit = 3
    } else {
      weathersit = 4
    }
    #add weathersit = val as evidence
    jtree = setEvidence(jtree, nodes = "weathersit", 
                        states = as.character(weathersit))
  }
  if(svalue(hum_value_combobox) != ""){
    if(svalue(hum_value_combobox) == "less then 20%)"){
      hum = 1
    } else if(svalue(hum_value_combobox) == "from 20% to 29%"){
      hum = 2
    } else if(svalue(hum_value_combobox) == "from 30% to 39%"){
      hum = 3
    } else if(svalue(hum_value_combobox) == "from 40 to 49%"){
      hum = 4
    } else if(svalue(hum_value_combobox) == "from 50% to 59%"){
      hum = 5
    } else if(svalue(hum_value_combobox) == "from 60% to 69%"){
      hum = 6
    } else if(svalue(hum_value_combobox) == "from 70 to 79%"){
      hum = 7
    } else if(svalue(hum_value_combobox) == "from 80 to 89%"){
      hum = 8
    } else { #Very high (>90%)
      hum = 9
    }
    #add hum = val as evidence
    jtree = setEvidence(jtree, nodes = "hum", states = as.character(hum))
  }
  
#----------- 2) inference P(cnt | evidences) through exact inference -----------
  result = querygrain(jtree, nodes = "cnt")$cnt #only a single probability
  arg_result = names(which.max(result))
  if(arg_result == "1"){
    svalue(textbox) = paste0("1 - Very low (", 
                             as.character(round(max(result)*100, digits = 0)), 
                             "%)")
  } else if(arg_result == "2"){
    svalue(textbox) = paste0("2 - Low (", 
                             as.character(round(max(result)*100, digits = 0)),
                             "%)")
  } else if(arg_result == "3"){
    svalue(textbox) = paste0("3 - Medium (", 
                             as.character(round(max(result)*100, digits = 0)),
                             "%)")
  } else {
    svalue(textbox) = paste0("4 - High (",
                             as.character(round(max(result)*100, digits = 0)),
                             "%)")
  }
  
#---------------------- 3) plot the distribution for cnt -----------------------
  barplot(result, col = c("red", "orange", "yellow", "green"), 
          main = paste0("Bike usage", "\n", "estimation through ", net_type), 
          xlab = "Bikes usage", ylab = "Probability",
          names.arg=c("very low", "low", "medium", "high"), ylim=c(0,1))
  grid(NA, NULL, lty = 2) #x = NA, y = NULL --> only horizontal
}
################################################################################



################################ IMPORT MODELS #################################
#set working directory
setwd("C:/Users/luca/Desktop/bikeSharingProject (1)/bikeSharingProject/")

#import the workspace with:
# - models (bnet and bnet_naive)
load("Models/bnet2_and_nb.RData")
################################################################################



################################## INTERFACE ###################################
#Top container
win = gwindow("BikeSharingApp", visible = F)
#gtkWindowSetResizable(win, FALSE)
win$set_icon("Images/bike.png")
#Paned group
paned = gpanedgroup(container = win)

#--------------------- VARIABLES AND MODEL CHANGE FRAMES -----------------------
#Variables and model choice group --> left side group
var_group = ggroup(container = paned, horizontal = F)

#frame for change model
frame0 = gframe("Model:", cont = var_group, horizontal = F)
#Horizontal change model group
model_group = ggroup(container = frame0)
#label for variable 'season'
model_label = glabel(
  "Choose the models to use: ",
  container = model_group
)
#combobox for variable 'season'
models = c("Bayesian model 1 and Naive Bayes", 
          "Bayesian model 2 and Naive Bayes",
          "Bayesian model 3 and Naive Bayes")
model_value_combobox = gcombobox(models, selected = 1, expand = T,
                                 container = model_group)

#frame for information about date
frame1 = gframe("Date info:" , cont = var_group, horizontal = F)

#Horizontal season group
season_group = ggroup(container = frame1)
#label for variable 'season'
season_label = glabel(
  "Season: ",
  container = season_group
)
#textbox for variable 'season'
seasons = c("", "Spring", "Summer", "Autumn", "Winter")
season_value_combobox = gcombobox(seasons, selected = 1, expand = T,
                                 container = season_group)

#Horizontal month group
month_group = ggroup(container = frame1)
#label for variable 'mnth'
month_label = glabel(
  "Month: ",
  container = month_group
)
#textbox for variable 'mnth'
months = c("", "January", "February", "March", "April", "May", "June", "July", 
         "August", "September", "October", "November", "December")
month_value_combobox = gcombobox(months, selected = 1, expand = T,
                                  container = month_group)

#Horizontal days group
days_group = ggroup(container = frame1)
#label for variable 'weekday'
days_label = glabel(
  "Week day: ",
  container = days_group
)
#textbox for variable 'weekday'
days = c("", "Monday", "Tuesday", "Wednesday", 
         "Thursday", "Friday", "Saturday", "Sunday")
days_value_combobox = gcombobox(days, selected = 1, expand = T,
                                container = days_group)

#Horizontal hour group
hour_group = ggroup(container = frame1)
#label for variable 'hour'
hour_label = glabel(
  "Hour (0:00-23:59): ",
  container = hour_group
)
#textbox for variable 'temp'
hour_value_textbox = gedit("", container = hour_group, expand=TRUE)

#Horizontal holiday group
hol_group = ggroup(container = frame1)
#label for variable 'holiday'
hol_label = glabel(
  "Is a holiday day? ",
  container = hol_group
)
#radiobutton for variabl 'holiday' 
hol_value_radiobutton = gradio(c("Yes", "No", "Unknown"), horizontal = T, selected = 3, 
                               container = hol_group)

frame2 = gframe("Weather info:" , cont = var_group, horizontal = F)

#Horizontal 'temp' group
temp_group = ggroup(container = frame2)
#label for variable 'temp'
temp_label = glabel(
  "Temperature(°C): ",
  container = temp_group
)
#textbox for variable 'temp'
temp_value_textbox = gedit("", container = temp_group, expand=TRUE)

#Horizontal 'atemp' group
atemp_group = ggroup(container = frame2)
#label for variable 'atemp'
atemp_label = glabel(
  "RealFeel Temperature(°C): ",
  container = atemp_group
)
#textbox for variable 'atemp'
atemp_value_textbox = gedit("", container = atemp_group, expand=T)

#Horizontal weathersit group
weath_group = ggroup(container = frame2)
#label for variable 'weathersit'
days_label = glabel(
  "Weather: ",
  container = weath_group
)
#textbox for variable 'weekday'
weaths = c("", 
           "Clear or partly cloudy", 
           "Cloudy or mist", 
           "Light rain/snow", 
           "Heavy rain/snow, thunderstorm or ice pallets")
weath_value_combobox = gcombobox(weaths, selected = 1, expand = T,
                                container = weath_group)

#Horizontal humidity group
hum_group = ggroup(container = frame2)
#label for variable 'hum'
days_label = glabel(
  "Humidity: ",
  container = hum_group
)
#textbox for variable 'weekday'
hums = c("",
         "less then 20%",
         "from 20% to 29%",
         "from 30% to 39%",
         "from 40% to 49%", 
         "from 50% to 59%", 
         "from 60% to 69%",
         "from 70% to 79%",
         "from 80% to 89%",
         "greater then 90%")
hum_value_combobox = gcombobox(hums, selected = 1, expand = T,
                                 container = hum_group)
#-------------------------------------------------------------------------------


#------------------------------- INFERENCE FRAME -------------------------------
#Frame for inference
frame3 = gframe("Inference:" , cont = var_group, horizontal = F)
#Estimate cnt with bn horizontal group
infer_bn_group = ggroup(container = frame3, horizontal = T, expand = T)
#Infer cnt
btn_infer_bn = gbutton(
  text      = "Infer used bikes with Bayesian Network",
  container = infer_bn_group,
  expand    = TRUE,
  handler   = function(h, ...)
  {
    if(svalue(model_value_combobox) == "Bayesian model 1 and Naive Bayes"){
      load("Models/bnet1_and_nb.RData")
    } else if(svalue(model_value_combobox) == "Bayesian model 2 and Naive Bayes"){
      load("Models/bnet2_and_nb.RData")
    } else {
      load("Models/bnet3_and_nb.RData")
    }
    #create the query enginer
    bn_jtree = compile(as.grain(bnet_cpts_bayes))
    #call the function for setting evidences and make exact inference
    set_evidences_and_inference(bn_jtree, cnt_bn_value_textbox, 
                               net_type = "Bayesian Network")
    #set inference results for naive bayes to null
    svalue(cnt_nbn_value_textbox) = ""
  }
)
size(btn_infer_bn) = c(290, 20)
#cnt textbox output
cnt_bn_value_textbox = gedit("", container = infer_bn_group, expand=T)
cnt_bn_value_textbox$set_editable(FALSE)

#Estimate cnt with bn horizontal group
infer_nbn_group = ggroup(container = frame3, horizontal = T, expand = T)
#Infer cnt
btn_infer_nbn = gbutton(
  text      = "Infer used bikes with Naive Bayes",
  container = infer_nbn_group,
  expand    = TRUE,
  handler   = function(h, ...)
  {
    if(svalue(model_value_combobox) == "Bayesian model 1 and Naive Bayes"){
      load("Models/bnet1_and_nb.RData")
    } else if(svalue(model_value_combobox) == "Bayesian model 2 and Naive Bayes"){
      load("Models/bnet2_and_nb.RData")
    } else {
      load("Models/bnet3_and_nb.RData")
    }
    #create the query enginer
    nbn_jtree = compile(as.grain(bnet_naive_cpts_bayes))
    #call the function for setting evidences and make exact inference
    set_evidences_and_inference(nbn_jtree, cnt_nbn_value_textbox, 
                                net_type = "Naive Bayes") 
    #set inference results for bayes net to null
    svalue(cnt_bn_value_textbox) = ""
  }
)
size(btn_infer_nbn) = c(290, 20)
#cnt textbox output
cnt_nbn_value_textbox = gedit("", container = infer_nbn_group, expand=T)
cnt_nbn_value_textbox$set_editable(FALSE)
#-------------------------------------------------------------------------------


#--------------------------------- PLOT FRAME ----------------------------------
#Plots group --> right side group
plots_group = ggroup(container = paned, horizontal = F)

#plots frame
frame4 = gframe("Plots:" , container = plots_group, horizontal = F)

#Plot nets horizontal group
plot_net_group = ggroup(container = frame4)
#plot bn button
btn_plot_bnet = gbutton(
  text      = "Plot Bayesian Network",
  container = plot_net_group,
  expand    = TRUE,
  handler   = function(h, ...)
  {
    if(svalue(model_value_combobox) == "Bayesian model 1 and Naive Bayes"){
      load("Models/bnet1_and_nb.RData")
    } else if(svalue(model_value_combobox) == "Bayesian model 2 and Naive Bayes"){
      load("Models/bnet2_and_nb.RData")
    } else {
      load("Models/bnet3_and_nb.RData")
    }
    #bayesian network
    graphviz.plot(bnet, layout = "dot", 
                  highlight=list(nodes = "cnt", col = "green", fill = "green"))
  }
)
#plot naive bn button
btn_plot_bnet_naive = gbutton(
  text      = "Plot Naive Bayes Network",
  container = plot_net_group,
  expand    = TRUE,
  handler   = function(h, ...)
  {
    if(svalue(model_value_combobox) == "Bayesian model 1 and Naive Bayes"){
      load("Models/bnet1_and_nb.RData")
    } else if(svalue(model_value_combobox) == "Bayesian model 2 and Naive Bayes"){
      load("Models/bnet2_and_nb.RData")
    } else {
      load("Models/bnet3_and_nb.RData")
    }
    #Naive Bayes network
    graphviz.plot(bnet_naive, layout = "neato", 
                  highlight=list(nodes = "cnt", col = "green", fill = "green"))
  }
)

#graphic space
ggraphics(container = frame4, width = 550, height = 474)

#set plot agasin in R window
#options(device = "RStudioGD")
#-------------------------------------------------------------------------------

#display GUI
visible(win) = TRUE
#initial image
pp = readPNG("Images/bike.png")
plot.new() 
rasterImage(pp,0,0,1,1)

################################################################################