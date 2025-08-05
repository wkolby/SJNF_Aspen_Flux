setwd("/Users/wksmith/Documents/GitHub/SJNF_Aspen_Flux")
github_dir <- "/Users/wksmith/Documents/GitHub/SJNF_Aspen_Flux/"
library(reshape2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)

#####################################TEST#######################################
master <- read.csv2(paste(github_dir,'Data/Level0/Data_Sheets/Datasheet_Dendro_07222025.csv',sep=''),sep=',',header=T)
dataset <- data.frame()

for(i in 1:length(master$Dir)){
  file <- read.csv2(paste(github_dir,'/Data/Level0/',master$Dir[i],sep=''),sep=';',header=F)
  temp_data<-file$V7
  data<-as.numeric(temp_data)-as.numeric(temp_data[2848])
  datetime<-format(as.POSIXct(file$V2,format="%Y.%m.%d %H:%M"))
  date<-format(as.POSIXct(file$V2,format="%Y.%m.%d %H:%M"),"%D")
  year<-format(as.POSIXct(file$V2,format="%Y.%m.%d %H:%M"), "%Y")
  month<-format(as.POSIXct(file$V2,format="%Y.%m.%d %H:%M"), "%m")
  week<-format(as.POSIXct(file$V2,format="%Y.%m.%d %H:%M"), "%W")
  day<-format(as.POSIXct(file$V2,format="%Y.%m.%d %H:%M"), "%d")
  hour<-format(as.POSIXct(file$V2,format="%Y.%m.%d %H:%M"), "%H")
  t<-format(as.POSIXct(file$V2,format="%Y.%m.%d %H:%M"), "%H:%M")
  type=rep(master$Site[i],length(data))
  id=rep(master$ID[i],length(data))
  
  ds=cbind(datetime,date,year,month,week,day,hour,t,type,id,data)
  dataset <- rbind(dataset,ds)
}

#Name the columns
colnames(dataset) <- c('datetime','date','year','month','week','day','hour','time',"type","id","data")
dataset=transform(dataset,id = as.numeric(id))
dataset=transform(dataset,data = as.numeric(data))
dataset=transform(dataset,datetime = as.POSIXct(datetime))
######################
dataset<-subset(dataset, date >= "06/15/25")
dataset_mean_hourly <- dataset %>% group_by(date,type,id,week,hour) %>% 
  summarise(datetime=first(datetime),d_90th = quantile(data,na.rm=T,probs=0.9),d_mean = mean(data,na.rm=T),d_sd = sd(data,na.rm=T))

ggplot(dataset_mean_hourly, aes(x=datetime,y=d_mean,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_discrete(name='Type',labels=c('Happy','Sad')) +
  scale_y_continuous("Increment (uM)") +
  scale_x_datetime("") +
  facet_wrap(~id)+
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/Figures/SJNF_ASPEN_Hourly_by ID_07222025.png',sep=''),dpi=300,width=180,height=120,units='mm')


dataset_mean_hourly<-subset(dataset_mean_hourly, id != 64 & id != 69)
ggplot(dataset_mean_hourly, aes(x=datetime,y=d_mean,group=id,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_discrete(name='Type',labels=c('Happy','Sad')) +
  scale_y_continuous("Increment (uM)") +
  scale_x_datetime("") +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/Figures/SJNF_ASPEN_Hourly_07222025.png',sep=''),dpi=300,width=180,height=120,units='mm')

