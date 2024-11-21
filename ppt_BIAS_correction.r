library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyselect)
library(forcats)
library(viridis)
library(qmap)
library(data.table)
## Input section to be defined by the user
input_location="E:\\Masters_FC\\01_fclimate_projection\\01_input\\02_extract_bias_corr_analysis"
training_folder="E:\\Masters_FC\\01_fclimate_projection\\03_output\\02_extract_bias_corr_analysis" ## Output folder from the extraction code

#gcm=c("access","earth") #new GCM can be added ,"miroc","mpi","mri"
var='ppt'
GCM_dir=file.path(input_location,var,"GCM")
gcm <- basename(list.dirs(GCM_dir, recursive = FALSE, full.names = FALSE))
### Function
model.assess <- function(Sim, Obs) { 
  # Nash-Sutcliffe Efficiency (NSE)
  NSE <- 1 - sum((Sim - Obs)^2) / sum((Obs - mean(Obs))^2)
  NSE <- round(NSE, 2)
  
  # Percent Bias (PBIAS)
  PBIAS <- (sum(Obs) - sum(Sim))*100 / sum(Obs) 
  PBIAS <- round(PBIAS, 2)
  
  # R-squared (R²)
  R2=round((cor(Sim, Obs, method="pearson"))^2,2)
  R2 <- round(R2, 2)
  
  return(rbind(NSE,PBIAS,R2))
}
extractYearMonth <- function(data, dateColumn) {
  # Extract year and month as strings
  data$year <- substr(data[[dateColumn]], 1, 4)
  data$month <- substr(data[[dateColumn]], 6, 7)
  data$day<- substr(data[[dateColumn]], 9, 10)
  
  # Convert 'Year' and 'Month' to numeric if needed
  data$year <- as.numeric(data$year)
  data$month <- as.numeric(data$month)
  data$day <- as.numeric(data$day)
  return(data)
}
output_df=function(df){
  df1=do.call(rbind, df)
  setDT(df1) 
  df2 <- df1[order(Date)]
  df3=rapply(object=df2,f=round,classes="numeric",how="replace",digits=1)
  return(df3)
}
gather_df=function(df,st_name,gcm,type,var){
  df1=df %>% group_by(year,month) %>% summarise_at(st_name,sum)
  df2=df1 %>% group_by(month) %>% summarise_at(st_name,mean)
  df3=df2 %>% gather(station,P,all_of(st_name))
  df3$GCM=gcm
  df3$type=type
  return(df3)
}
obs_location=file.path(input_location,var,"\\hist_obs.xlsx")
obs=read_excel(obs_location)
# Create a folder to save the JPEG files if it doesn't exist
if (!file.exists(file.path(training_folder,var,"bias_corrected_graph"))) {
  dir.create(file.path(training_folder,var,"bias_corrected_graph"))
}
graph_location=file.path(training_folder,var,"bias_corrected_graph")
m2=vector("list",length(gcm))
r1=NULL

for (k in 1:length(gcm)){
  loc=file.path(training_folder,var,"GCM",gcm[k])
  mod <- read_excel(paste0(loc,"\\historical_raw.xlsx"))
  # Assuming your data frame is named 'mod'
  mod <- extractYearMonth(mod, "Date")
  modssp245 <- read_excel(paste0(loc,"\\ssp245_raw.xlsx"))
  # Assuming your data frame is named 'mod'
  modssp245 <- extractYearMonth(modssp245, "Date")
  modssp585 <- read_excel(paste0(loc,"\\ssp585_raw.xlsx"))
  modssp585 <- extractYearMonth(modssp585, "Date")
  obs$Date=as.Date(obs$Date,format="%Y-%m-%d")
  obs1=obs%>%mutate(year=as.numeric(format(Date,"%Y")),month=as.numeric(format(Date,"%m")))
  l=vector("list",12)    #creating empty list
  lm=vector("list",12) 
  lmssp245=vector("list",12) 
  lmssp585=vector("list",12) 
  lm_date=vector("list",12)
  lmssp245_date=vector("list",12)
  lmssp585_date=vector("list",12)
  for(i in 1:12){
    x1=obs1%>%filter(month==i)
    x2=mod%>%filter(month==i)
    x3=modssp245%>%filter(month==i)
    x4=modssp585%>%filter(month==i)
    l[[i]]=dplyr::select(x1,-c(Date,year,month))
    lm[[i]]=dplyr::select(x2,-c(Date,year,month,day))
    lmssp245[[i]]=dplyr::select(x3,-c(Date,year,month,day))
    lmssp585[[i]]=dplyr::select(x4,-c(Date,year,month,day))
    lm_date[[i]]=dplyr::select(x2,c(Date,year,month,day))
    lmssp245_date[[i]]=dplyr::select(x3,c(Date,year,month,day))
    lmssp585_date[[i]]=dplyr::select(x4,c(Date,year,month,day))
    
  }
  #View(lm[[2]])
  st_name=colnames(lm[[1]])
  lm_bias=vector("list",12)
  ssp245_bias=vector("list",12)
  ssp585_bias=vector("list",12)
  for(i in 1:length(lm)){      #bias correction method to be applied
    qm.fit <- fitQmapRQUANT(l[[i]],lm[[i]],
                            qstep=0.01,nboot=10,wet.day=TRUE)
    lm_bias[[i]] <- doQmapRQUANT(lm[[i]],qm.fit,type="linear")
    ssp245_bias[[i]] <- doQmapRQUANT(lmssp245[[i]],qm.fit,type="linear")
    ssp585_bias[[i]] <- doQmapRQUANT(lmssp585[[i]],qm.fit,type="linear")
    
  }
  
  for (i in 1:12) {
    lm_bias[[i]] <- data.frame(lm_date[[i]], lm_bias[[i]])
    ssp245_bias[[i]] <- data.frame(lmssp245_date[[i]], ssp245_bias[[i]])
    ssp585_bias[[i]] <- data.frame(lmssp585_date[[i]], ssp585_bias[[i]])
    
  }
  historical=output_df(lm_bias)
  ssp245=output_df(ssp245_bias)
  ssp585=output_df(ssp585_bias)
  write_xlsx(historical,paste0(loc,"\\historical_bias_corrected.xlsx"))
  write_xlsx(ssp245,paste0(loc,"\\ssp245_bias_corrected.xlsx"))
  write_xlsx(ssp585,paste0(loc,"\\ssp585_bias_corrected.xlsx"))
  mod_month=gather_df(mod,st_name,gcm[k],'Raw')
  obs_month=gather_df(obs1,st_name,gcm[k],'Observed')
  modc_month=gather_df(historical,st_name,gcm[k],'Corrected')
  m2[[k]]=rbind(obs_month,mod_month,modc_month)
  r1=rbind(r1,m2[[k]])
  for(j in 1:length(st_name)){
    z1=m2[[k]] %>% spread(type,P)
    z2=filter(z1,station==st_name[j])
    pre_cor=model.assess(z2$Raw,z2$Observed)
    post_cor=model.assess(z2$Corrected,z2$Observed)
    stat_daily=data.frame(cbind(pre_cor,post_cor))
    stat_daily$station=st_name[j]
    colnames(stat_daily)=c("Pre_Correction","Post_Correction","Station")
    stat_daily <- data.frame(Statistical_indicator = row.names(stat_daily),stat_daily)
    stat_daily$Pre_Correction=as.numeric(stat_daily$Pre_Correction)
    stat_daily$Post_Correction=as.numeric(stat_daily$Post_Correction)
    write_xlsx(stat_daily,paste0(loc,"\\",st_name[j],"_stat_indicator.xlsx"))
  }
}
split_values <- split(unique(r1$station), rep(1:length(unique(r1$station)), each=1
                                              ,length.out = length(unique(r1$station))))
z=substr(month.abb, 1, 1)
for (k in 1:length(split_values)){
  var1=split_values[[k]]
  df11=filter(r1,station %in% var1) 
  print(ggplot(df11,aes(x=month,y=P,color=type,size=type))+geom_line()
        +scale_x_discrete(limits=month.abb)
        +facet_grid(rows=vars(GCM)  #fct_recode(GCM, "ACCESS-CM2" = "access","EC-EARTH3"="earth")
                    ,cols = vars(station),scales="free_y")
        +labs(title="Bias Correction Result For Precipitation (mm)")+scale_size_manual(values=c(3,1,1))
        +theme_bw()+theme(plot.title = element_text(face="bold",hjust = 0.5,size = 20) 
                          ,legend.title = element_blank(),legend.position = c(0.2,0.75)
                          ,axis.title = element_text(face = "bold",size = 15)
                          ,axis.text = element_text(face="bold",size=10)
                          ,legend.text = element_text(face="bold",size=15)
                          ,axis.title.x = element_blank(),axis.title.y = element_blank()
                          ,legend.direction = "horizontal",strip.text = element_text(face="bold",size=10))
        +scale_color_viridis(discrete = TRUE,option = "plasma")) 
  jpeg_filename <- paste0(graph_location,"\\",var, "_",var1, ".jpg")
  ggsave(jpeg_filename, plot = last_plot(), device = "jpeg", width = 10, height = 6)
}

