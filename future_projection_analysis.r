library(ftExtra)
library(flextable)
library(officer)
library(writexl)
library(readxl)
library(ggplot2)
library(ggdist)
library(tidyr)
library(dplyr)
library(lubridate)
library(forcats)
library(webshot)
## Input section to be defined by the user
input_location="E:\\Masters_FC\\01_fclimate_projection\\01_input\\02_extract_bias_corr_analysis"
training_folder="E:\\Masters_FC\\01_fclimate_projection\\03_output\\02_extract_bias_corr_analysis" ## Output folder from the extraction code
#gcm=c("access","earth")
var=c("ppt","tmax","tmin")
var1=c("P(mm)","Tmax(°C)","Tmin(°C)") ### Notation to be used for graph
var2=c("PPT","Tmax","Tmin")
scen=c("ssp245","ssp585")
scen1=c("SSP245","SSP585")

### Code Starts
#m2=vector("list",length(gcm))
final=NULL
annual_graph_folder=vector("list",length(var))
seasonal_graph_folder=vector("list",length(var))
table_output_folder=vector("list",length(var))
stn_file=vector("list",length(var))
stn1=vector("list",length(var))
i=1
for (i in 1:length(var)){
  datafile=file.path(training_folder,var[i],'GCM')
  #for Seasonal Graph Folder
  if (!file.exists(file.path(training_folder,var[i],"seasonal_graph"))) {
    dir.create(file.path(training_folder,var[i],"seasonal_graph"))
  }
  seasonal_graph_folder[[i]]=file.path(training_folder,var[i],"seasonal_graph")
  #for Annual Graph Folder
  if (!file.exists(file.path(training_folder,var[i],"Annual_graph"))) {
    dir.create(file.path(training_folder,var[i],"Annual_graph"))
  }
  annual_graph_folder[[i]]=file.path(training_folder,var[i],"Annual_graph")
  #for Output table Folder
  if (!file.exists(file.path(training_folder,var[i],"Table_output"))) {
    dir.create(file.path(training_folder,var[i],"Table_output"))
  }
  table_output_folder[[i]]=file.path(training_folder,var[i],"Table_output")
  stn_file[[i]]=read.csv(file.path(input_location,var[i],'Met_Station_list.csv'))
  stn1[[i]]=stn_file[[i]]$INDEX
  x2=NULL
  GCM_dir=file.path(input_location,var[i],"GCM")
  gcm <- basename(list.dirs(GCM_dir, recursive = FALSE, full.names = FALSE))
  for (j in 1:length(gcm)){
    x1=NULL
    for (k in 1:length(scen)){
      datafile2=paste0(datafile,'\\',gcm[j],"\\",scen[k],"_bias_corrected.xlsx")
      x=read_excel(datafile2)
      x=x[,c(stn1[[i]],"Date","year","month","day")]
      x$GCM=rep(gcm[j],nrow(x))
      if (j==1){
        obs=read_excel(paste0(input_location,"\\",var[i],"\\hist_obs.xlsx"))
        obs$Date=as.Date(obs$Date,format = "%Y-%m-%d")
        obs1=obs%>%mutate(year=as.numeric(format(Date,"%Y")),month=as.numeric(format(Date,"%m")),day=mday(Date))
        obs2=obs1[,c(stn1[[i]],"Date","year","month","day")]
        obs2$GCM=rep("historical",nrow(obs2))
        x=rbind(obs2,x)
      }
      x$plot1=rep(scen1[k],nrow(x))
      x$type=rep(var1[i],nrow(x))
      x1=rbind(x1,x)
    }
    
    assign(paste0(var[i],"_",gcm[j]),x1)
    x2=rbind(x2,x1)
  }
  assign(paste0("long","_",var[i]),x2)
  if(i==1){
    x3=x2 %>% group_by(year,plot1,type,GCM) %>% summarise_at(stn1[[i]],sum)
    assign(paste0("plot","_",var[i]),x3)
    final=rbind(final,x3)
  }else{
    x3=x2 %>% group_by(year,plot1,type,GCM) %>% summarise_at(stn1[[i]],mean)
    assign(paste0("plot","_",var[i]),x3)
    final=rbind(final,x3)
  }
}

p1=vector("list",length(var))
for(i in 1:length(p1)){
  p1[[i]]=filter(final,type==var1[i])
  p1[[i]]=dplyr::select(data.frame(p1[[i]]),c("year","plot1","type","GCM",stn1[[i]]))
  p1[[i]]=p1[[i]] %>% gather(station,value,all_of(stn1[[i]]))
  for (j in 1:length(stn1[[i]])){
    v1 = filter(p1[[i]],station==stn1[[i]][j])
    print(ggplot(v1,aes(x=year,y=value))+stat_lineribbon(.width = c(0.95,0.8,0.5))+scale_fill_brewer()
          +labs(y=var1[i])+ facet_grid(rows = vars(plot1),scales="free_y")+labs(x="Year",title = stn1[[i]][j])
          + scale_x_continuous(breaks = c(1981,2010,2016,2036,2045,2065,2071,2100))
          +geom_vline(xintercept = 1981,color="red",lty=2,lwd=1.2)+geom_vline(xintercept = 2010
                                                                              ,color="red",lty=2,lwd=1.2)
          +geom_vline(xintercept = 2016,color="orange",lty=2,lwd=1.2)+geom_vline(xintercept = 2036
                                                                                 ,color="green",lty=2,lwd=1.2)
          +geom_vline(xintercept = 2045,color="orange",lty=2,lwd=1.2)+geom_vline(xintercept = 2065
                                                                                 ,color="green",lty=2,lwd=1.2)
          +geom_vline(xintercept = 2071,color="yellow",lty=2,lwd=1.2)+geom_vline(xintercept = 2100
                                                                                 ,color="yellow",lty=2,lwd=1.2)
          +theme_bw()
          +theme(legend.position = c(0.2,0.4),legend.title = element_blank(),legend.direction = "horizontal"
                 ,plot.title = element_text(color = "black",face = "bold",hjust = 0.5)
                 ,legend.text = element_text(face="bold",size = 16)
                 ,axis.title = element_text(face = "bold",size= 18)
                 ,strip.text = element_text(face="bold",size=18),axis.text=element_text(face="bold",size=16)
                 ,axis.text.x = element_text(angle = 90)))
    jpeg_filename <- paste0(annual_graph_folder[[i]],"\\plot_", var2[i],"_",stn1[[i]][j], ".jpg")
    ggsave(jpeg_filename, plot = last_plot(), device = "jpeg", width = 10, height = 6)
  }
  
}
pstn=stn1[[1]]
tmaxstn=stn1[[2]]
tminstn=stn1[[3]]
### For Precipitation Seasonal analysis
ppt <- read_pptx()
DJF=long_ppt[long_ppt$month %in% c(12,1,2),]
DJF$season="DJF"
MAM=long_ppt[long_ppt$month %in% c(3:5),]
MAM$season="MAM"
JJAS=long_ppt[long_ppt$month %in% c(6:9),]
JJAS$season="JJAS"
ON=long_ppt[long_ppt$month %in% c(10,11),]
ON$season="ON"
Annual=long_ppt
Annual$season="Annual"
p_season=rbind(Annual,DJF,MAM,JJAS,ON)
p_season1=p_season %>% group_by(year,season,plot1,type,GCM) %>% summarise_at(all_of(pstn),sum)
p_season1_long=p_season1 %>% gather(station,p,all_of(pstn))
p_hist=p_season1_long[p_season1_long$GCM=="historical",]
names(p_hist)[names(p_hist) == 'p'] <- 'p_obs'
p_hist=dplyr::select(p_hist,-c(GCM))
p_hist=dplyr::select(data.frame(p_hist),-c(type))
p_hist_avg=p_hist %>% group_by(season,plot1,station) %>% summarise_at("p_obs",mean)
p_fut=p_season1_long[!p_season1_long$GCM=="historical",]
p_fut_ensemble=p_fut %>% group_by(year,season,plot1,type,station) %>% summarise_at("p",mean)
p_fut_ensemble$GCM="ENSEMBLE"
p_fut_ensemble=p_fut_ensemble[,c(1:4,7,5:6)]
p_fut=rbind(p_fut,p_fut_ensemble)
p_merged=merge(p_fut, p_hist_avg, by.x=c("season","plot1","station"), by.y=c("season","plot1","station"),all.x = T)
p_merged1=mutate(p_merged,change=round((((p-p_obs)*100)/(p_obs)),2))
p_merged2=filter(p_merged1,year>=2016)
NF=filter(p_merged2,year>=2016 & year<=2045)
NF$period="NF"
MF=filter(p_merged2,year>=2036 & year<=2065)
MF$period="MF"
FF=filter(p_merged2,year>=2071 & year<=2100)
FF$period="FF"
p_merged3=rbind(NF,MF,FF)
for(i in 1:length(pstn)){
  p3=filter(p_merged3,station==pstn[i])
  g2=ggplot(p_merged3,aes(x=factor(season,levels = c("DJF","MAM","JJAS","ON","Annual")),y=change
                          ,fill=factor(GCM)))+ #fct_recode(GCM, "ACCESS-CM2" = "access","EC-EARTH3"="earth","ENSEMBLE"="Ensemble"),levels = c("ACCESS-CM2","EC-EARTH3","ENSEMBLE")
    geom_boxplot(outlier.shape = NA,position = position_dodge(0.9))+
    labs(x="Season",y="Change in P[%]",fill="GCMs",title = pstn[i])+theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,face="bold",size = 18)
          ,axis.title = element_text(face = "bold",size = 12)
          ,axis.text.x=element_text(face = "bold",size = 10)
          ,legend.position = "bottom",legend.direction = "horizontal"
          ,axis.title.x = element_blank(),axis.text=element_text(face="bold",size = 10)
          ,strip.text = element_text(face="bold",size=10)
          ,legend.text =element_text(face="bold",size=10),legend.title =element_text(face="bold",size=12))+
    facet_grid(rows=vars(plot1),cols = vars(factor(period,levels = c("NF","MF","FF"))))+
    scale_y_continuous(limits = quantile(p_merged3$change,c(0.1,0.9)))
  g1=g2+stat_summary(fun =mean,geom="point",aes(group=factor(GCM)),position = position_dodge(0.9)
                     ,col="black",shape=4)
  print(g1)
  jpeg_filename <- paste0(seasonal_graph_folder[[1]],"\\seasonal_PPT_",pstn[i], ".jpg")
  ggsave(jpeg_filename, plot = last_plot(), device = "jpeg", width = 10, height = 6)
  u=unique(p3$GCM)
  u2=u
  #u2=case_when(u=="access"~ "ACCESS-CM2",u=="earth"~ "EC-EARTH3",u=="Ensemble"~ "ENSEMBLE")
  for (j in 1:length(u)){
    p31=filter(p3,GCM==u[j])
    p4=p31%>% unite(period1,plot1,period,sep="-")
    p5=p4 %>% group_by(season,period1) %>%  
      summarize(mean = sprintf("%.0f",mean(change)),range = paste(sprintf("%.0f",min(change))
                                                                  ,"-", sprintf("%.0f",max(change))))
    p6=p5 %>% gather(parameter,value,c(mean,range))
    p7=p6 %>% spread(period1,value) 
    p8=p3%>% group_by(season) %>%  summarize(Baseline=round(mean(p_obs),0))
    p9=merge(p7, p8, by.x=c("season"), by.y=c("season"),all.x = T)
    colnames(p9)[which(names(p9) == "Baseline")] <- "Baseline (mm)"
    colnames(p9)[which(names(p9) == "parameter")] <- "Parameter"
    colnames(p9)[which(names(p9) == "season")] <- "Season"
    p9=p9[c(1,2,3,4,7,8,5,6,9,10),c(1,9,2,5,4,3,8,7,6)]
    ft <- p9 %>% as_flextable
    set_flextable_defaults(font.size = 15, theme_fun = theme_vanilla, padding = 6,font.family = "Times New Roman")
    def_par <- fp_par(text.align = "center", padding = 5)
    ft1=ft %>% span_header(sep = "-")  %>% theme_vanilla()%>% style(pr_p = def_par,  part = "all") %>% 
      set_caption(caption = paste0(pstn[i],"_",u2[j],"_PPT")) %>% autofit()%>% 
      merge_v(j = "Season",target = c("Season","Baseline (mm)")) %>% add_header_lines(paste0(pstn[i],"_",u2[j]
                                                                                             ,"_PPT"))%>% 
      add_footer_lines("Change is reported in %")                                                                                    
    print(ft1)
    ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
    
    t_s <- flextable_dim(ft1)$widths
    s_s <- slide_size(ppt)$width
    left <- (s_s/2) - (t_s/2) 
    
    ppt <- ph_with(ppt, value = ft1, location = ph_location(left = left, top = 1)) 
    print(ppt, target = paste0(table_output_folder[[1]],"\\","ppt_seasonal.pptx"))
  }
}
#For maximum temperature
ppt <- read_pptx()
DJF=long_tmax[long_tmax$month %in% c(12,1,2),]
DJF$season="DJF"
MAM=long_tmax[long_tmax$month %in% c(3:5),]
MAM$season="MAM"
JJAS=long_tmax[long_tmax$month %in% c(6:9),]
JJAS$season="JJAS"
ON=long_tmax[long_tmax$month %in% c(10,11),]
ON$season="ON"
Annual=long_tmax
Annual$season="Annual"
p_season=rbind(Annual,DJF,MAM,JJAS,ON)
p_season1=p_season %>% group_by(year,season,plot1,type,GCM) %>% summarise_at(all_of(tmaxstn),mean)
p_season1_long=p_season1 %>% gather(station,p,all_of(tmaxstn))
p_hist=p_season1_long[p_season1_long$GCM=="historical",]
names(p_hist)[names(p_hist) == 'p'] <- 'p_obs'
p_hist=dplyr::select(p_hist,-c(GCM))
p_hist=dplyr::select(data.frame(p_hist),-c(type))
p_hist_avg=p_hist %>% group_by(season,plot1,station) %>% summarise_at("p_obs",mean)
p_fut=p_season1_long[!p_season1_long$GCM=="historical",]
p_fut_ensemble=p_fut %>% group_by(year,season,plot1,type,station) %>% summarise_at("p",mean)
p_fut_ensemble$GCM="Ensemble"
p_fut_ensemble=p_fut_ensemble[,c(1:4,7,5:6)]
p_fut=rbind(p_fut,p_fut_ensemble)
p_merged=merge(p_fut, p_hist_avg, by.x=c("season","plot1","station"), by.y=c("season","plot1","station"),all.x = T)
p_merged1=mutate(p_merged,change=round((p-p_obs),2))
p_merged2=filter(p_merged1,year>=2016)
NF=filter(p_merged2,year>=2016 & year<=2045)
NF$period="NF"
MF=filter(p_merged2,year>=2036 & year<=2065)
MF$period="MF"
FF=filter(p_merged2,year>=2071 & year<=2100)
FF$period="FF"
p_merged3=rbind(NF,MF,FF)
for(i in 1:length(tmaxstn)){
  p3=filter(p_merged3,station==tmaxstn[i])
  g3=ggplot(p_merged3,aes(x=factor(season,levels = c("DJF","MAM","JJAS","ON","Annual"))
                          ,y=change,fill=factor(GCM))) + #fct_recode(GCM, "ACCESS-CM2" = "access","EC-EARTH3"="earth","ENSEMBLE"="Ensemble"),levels = c("ACCESS-CM2","EC-EARTH3","ENSEMBLE")
    geom_boxplot(outlier.shape = NA,position = position_dodge(0.9))+
    labs(x="Season",y="Change in Tmax[°C]",fill="GCMs",title=tmaxstn[i])+theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,face="bold",size = 18),axis.title = element_text(face = "bold"
                                                                                                 ,size = 12)
          ,axis.text.x=element_text(face = "bold",size = 11),legend.position = "bottom"
          ,legend.direction = "horizontal"
          ,axis.title.x = element_blank(),axis.text=element_text(face="bold",size = 11)
          ,strip.text = element_text(face="bold",size=12),legend.text =element_text(face="bold",size=12)
          ,legend.title =element_text(face="bold",size=11) )+
    facet_grid(rows=vars(plot1),cols = vars(factor(period,levels = c("NF","MF","FF")))
               ,space = "free",scales = "free")+
    scale_y_continuous(limits = quantile(p_merged3$change,c(0.1,0.95)))
  g4=g3+stat_summary(fun =mean,geom="point",aes(group=factor(GCM)),position = position_dodge(0.9)
                     ,col="black",shape=4)
  print(g4)
  jpeg_filename <- paste0(seasonal_graph_folder[[2]],"\\seasonal_Tmax_",tmaxstn[i], ".jpg")
  ggsave(jpeg_filename, plot = last_plot(), device = "jpeg", width = 10, height = 6)
  u=unique(p3$GCM)
  u2=u
  for (j in 1:length(u)){
    p31=filter(p3,GCM==u[j])
    p4=p31%>% unite(period1,plot1,period,sep="-")
    p5=p4 %>% group_by(season,period1) %>%  summarize(mean = sprintf("%.1f",mean(change))
                                                      ,range = paste(sprintf("%.1f",min(change)) 
                                                                     ,"-", sprintf("%.1f",max(change))))
    p6=p5 %>% gather(parameter,value,c(mean,range))
    p7=p6 %>% spread(period1,value) 
    p8=p3%>% group_by(season) %>%  summarize(Baseline=round(mean(p_obs),1))
    p9=merge(p7, p8, by.x=c("season"), by.y=c("season"),all.x = T)
    colnames(p9)[which(names(p9) == "Baseline")] <- "Baseline (°C)"
    colnames(p9)[which(names(p9) == "parameter")] <- "Parameter"
    colnames(p9)[which(names(p9) == "season")] <- "Season"
    p9=p9[c(1,2,3,4,7,8,5,6,9,10),c(1,9,2,5,4,3,8,7,6)]
    ft <- p9 %>% as_flextable
    set_flextable_defaults(font.size = 15, theme_fun = theme_vanilla, padding = 6,font.family = "Times New Roman")
    def_par <- fp_par(text.align = "center", padding = 5)
    ft1=ft %>% span_header(sep = "-")  %>% theme_vanilla() %>% style(pr_p = def_par,  part = "all") %>% 
      set_caption(caption = paste0(tmaxstn[i],"_",u2[j],"_Tmax")) %>% autofit() %>% 
      merge_v(j = "Season",target = c("Season","Baseline (°C)")) %>% add_header_lines(paste0(tmaxstn[i]
                                                                                             ,"_",u2[j],"_Tmax"))%>% 
      add_footer_lines("Change is reported in°C")                                                                                 
    print(ft1)
    ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
    
    t_s <- flextable_dim(ft1)$widths
    s_s <- slide_size(ppt)$width
    left <- (s_s/2) - (t_s/2) 
    
    ppt <- ph_with(ppt, value = ft1, location = ph_location(left = left, top = 1)) 
    print(ppt, target = paste0(table_output_folder[[2]],"\\","tmax_seasonal.pptx"))
  }
}
#For minimum temperature
ppt <- read_pptx()
DJF=long_tmin[long_tmin$month %in% c(12,1,2),]
DJF$season="DJF"
sapply(DJF,class)
MAM=long_tmin[long_tmin$month %in% c(3:5),]
MAM$season="MAM"
JJAS=long_tmin[long_tmin$month %in% c(6:9),]
JJAS$season="JJAS"
ON=long_tmin[long_tmin$month %in% c(10,11),]
ON$season="ON"
Annual=long_tmin
Annual$season="Annual"
p_season=rbind(Annual,DJF,MAM,JJAS,ON)
p_season1=p_season %>% group_by(year,season,plot1,type,GCM) %>% summarise_at(all_of(tminstn),mean)
p_season1_long=p_season1 %>% gather(station,p,all_of(tminstn))
p_hist=p_season1_long[p_season1_long$GCM=="historical",]
names(p_hist)[names(p_hist) == 'p'] <- 'p_obs'
p_hist=dplyr::select(p_hist,-c(GCM))
p_hist=dplyr::select(data.frame(p_hist),-c(type))
p_hist_avg=p_hist %>% group_by(season,plot1,station) %>% summarise_at("p_obs",mean)
p_fut=p_season1_long[!p_season1_long$GCM=="historical",]
p_fut_ensemble=p_fut %>% group_by(year,season,plot1,type,station) %>% summarise_at("p",mean)
p_fut_ensemble$GCM="Ensemble"
p_fut_ensemble=p_fut_ensemble[,c(1:4,7,5:6)]
p_fut=rbind(p_fut,p_fut_ensemble)
p_merged=merge(p_fut, p_hist_avg, by.x=c("season","plot1","station"), by.y=c("season","plot1","station"),all.x = T)
p_merged1=mutate(p_merged,change=round((p-p_obs),2))
p_merged2=filter(p_merged1,year>=2016)
NF=filter(p_merged2,year>=2016 & year<=2045)
NF$period="NF"
MF=filter(p_merged2,year>=2036 & year<=2065)
MF$period="MF"
FF=filter(p_merged2,year>=2071 & year<=2100)
FF$period="FF"
p_merged3=rbind(NF,MF,FF)
for(i in 1:length(tminstn)){
  p3=filter(p_merged3,station==tminstn[i])
  g2=ggplot(p3,aes(x=factor(season,levels = c("DJF","MAM","JJAS","ON","Annual"))
                   ,y=change,fill=factor(GCM)))+ #fct_recode(GCM, "ACCESS-CM2" = "access","EC-EARTH3"="earth","ENSEMBLE"="Ensemble"),levels = c("ACCESS-CM2","EC-EARTH3","ENSEMBLE")
    geom_boxplot(outlier.shape = NA,position = position_dodge(0.9))+
    labs(x="Season",y="Change in Tmin[°C]",fill="GCMs",title = tminstn[i])+theme_bw() +
    theme(plot.title = element_text(hjust = 0.5,face="bold",size=18),axis.title = element_text(face = "bold"
                                                                                               ,size = 15)
          ,axis.text.x=element_text(face = "bold",size = 14),legend.position = "bottom"
          ,legend.direction = "horizontal"
          ,axis.title.x = element_blank(),axis.text=element_text(face="bold",size = 14)
          ,strip.text = element_text(face="bold",size=16),legend.text =element_text(face="bold",size=16)
          ,legend.title =element_text(face="bold",size=16))+
    facet_grid(rows=vars(factor(period,levels = c("NF","MF","FF"))),cols = vars(plot1),space = "free"
               ,scales = "free") 
  g1=g2+stat_summary(fun =mean,geom="point",aes(group=factor(GCM)),position = position_dodge(0.9),col="black"
                     ,shape=4)
  print(g1)
  jpeg_filename <- paste0(seasonal_graph_folder[[3]],"\\seasonal_Tmin_",tminstn[i], ".jpg")
  ggsave(jpeg_filename, plot = last_plot(), device = "jpeg", width = 10, height = 6)
  u=unique(p3$GCM)
  u2=u
  for (j in 1:length(u)){
    p31=filter(p3,GCM==u[j])
    p4=p31%>% unite(period1,plot1,period,sep="-")
    p5=p4 %>% group_by(season,period1) %>%  summarize(mean = sprintf("%.1f",mean(change)),
                                                      range = paste(sprintf("%.1f",min(change)) ,"-", 
                                                                    sprintf("%.1f",max(change))))
    p6=p5 %>% gather(parameter,value,c(mean,range))
    p7=p6 %>% spread(period1,value) 
    p8=p3%>% group_by(season) %>%  summarize(Baseline=round(mean(p_obs),1))
    p9=merge(p7, p8, by.x=c("season"), by.y=c("season"),all.x = T)
    colnames(p9)[which(names(p9) == "Baseline")] <- "Baseline (°C)"
    colnames(p9)[which(names(p9) == "parameter")] <- "Parameter"
    colnames(p9)[which(names(p9) == "season")] <- "Season"
    p9=p9[c(1,2,3,4,7,8,5,6,9,10),c(1,9,2,5,4,3,8,7,6)]
    ft <- p9 %>% as_flextable
    set_flextable_defaults(font.size = 15, theme_fun = theme_vanilla, padding = 6,font.family = "Times New Roman")
    def_par <- fp_par(text.align = "center", padding = 5)
    ft1=ft %>% span_header(sep = "-") %>% theme_vanilla() %>% style(pr_p = def_par,  part = "all") %>% 
      set_caption(caption = paste0(tminstn[i],"_",u2[j],"_Tmin")) %>% autofit() %>% 
      merge_v(j = "Season",target = c("Season","Baseline (°C)")) %>% 
      add_header_lines(paste0(tminstn[i],"_",u2[j],"_Tmin")) %>% 
      add_footer_lines("Change is reported in °C")                                                                                                                                                                                     
    print(ft1)
    ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
    t_s <- flextable_dim(ft1)$widths
    s_s <- slide_size(ppt)$width
    left <- (s_s/2) - (t_s/2) 
    ppt <- ph_with(ppt, value = ft1, location = ph_location_type(type = "body")) 
    print(ppt, target = paste0(table_output_folder[[3]],"\\","tmin_seasonal.pptx"))
  }
}





