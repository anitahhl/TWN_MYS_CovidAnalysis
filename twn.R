rm(list = ls())

library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
options(scipen = 999)

setwd("/Users/anitahuang/scu_financial_program")
dir()


COVID_19<-read.csv("twn.csv")
COVID_19_vac<-read.csv("twn_vac.csv")
names(COVID_19)

COVID_19_part<-COVID_19[,c("日期","總確診數","新增確診數","七天移動平均新增確診數","總死亡數","新增死亡數","傳染率","總人口數")]
COVID_19_vac_part<-COVID_19_vac[,c("date","people_vaccinated_per_hundred","people_fully_vaccinated_per_hundred","total_boosters_per_hundred")]

names(COVID_19_part)<-c("date","total_vaccination","daily_vaccination",
                        "daily_vaccination_avg","die","daily_die","infection","population")

COVID_19_part<-COVID_19_part[-1,]#刪除第一欄
COVID_19_vac_part<-COVID_19_vac_part[-1,]#刪除第一欄
# View(COVID_19_part)#再檢查欄位
# View(COVID_19_vac_part)#再檢查欄位
str(COVID_19_part)
str(COVID_19_vac_part)
COVID_19_part<-merge(COVID_19_part,COVID_19_vac_part,all=TRUE) #聯集合併
# View(COVID_19_part)
str(COVID_19_part)

#將格式改成日期格式及數字格式
COVID_19_part$date<-as.character(COVID_19_part$date)
COVID_19_part$date<-strptime(COVID_19_part$date,
                             "%Y-%m-%d",
                             tz=Sys.timezone())
COVID_19_part$date<-as.POSIXct(COVID_19_part$date)
COVID_19_part[4]
COVID_19_part$total_vaccination<-as.character(COVID_19_part$total_vaccination)#
COVID_19_part$daily_vaccination<-as.character(COVID_19_part$daily_vaccination)#
COVID_19_part$daily_vaccination_avg<-as.character(COVID_19_part$daily_vaccination_avg)#
COVID_19_part$die<-as.character(COVID_19_part$die)
COVID_19_part$daily_die<-as.character(COVID_19_part$daily_die)#
COVID_19_part$infection<-as.character(COVID_19_part$infection)
COVID_19_part$population<-as.character(COVID_19_part$population)
COVID_19_part$people_vaccinated_per_hundred<-as.character(COVID_19_part$people_vaccinated_per_hundred)
COVID_19_part$people_fully_vaccinated_per_hundred<-as.character(COVID_19_part$people_fully_vaccinated_per_hundred)
COVID_19_part$total_boosters_per_hundred<-as.character(COVID_19_part$total_boosters_per_hundred)

COVID_19_part$total_vaccination<-as.numeric(COVID_19_part$total_vaccination)#
COVID_19_part$daily_vaccination<-as.numeric(COVID_19_part$daily_vaccination)#
COVID_19_part$daily_vaccination_avg<-as.numeric(COVID_19_part$daily_vaccination_avg)#
COVID_19_part$daily_die<-as.numeric(COVID_19_part$daily_die)#
COVID_19_part$die<-as.numeric(COVID_19_part$die)
COVID_19_part$infection<-as.numeric(COVID_19_part$infection)
COVID_19_part$population<-as.numeric(COVID_19_part$population)
COVID_19_part$people_vaccinated_per_hundred<-as.numeric(COVID_19_part$people_vaccinated_per_hundred)
COVID_19_part$people_fully_vaccinated_per_hundred<-as.numeric(COVID_19_part$people_fully_vaccinated_per_hundred)
COVID_19_part$total_boosters_per_hundred<-as.numeric(COVID_19_part$total_boosters_per_hundred)
str(COVID_19_part)#檢查格式正確與否
# View(COVID_19_part)
COVID_19_part_2022<-subset(COVID_19_part,date<="2022-06-07 CST"
                           &date>="2022-01-01 CST")
# View(COVID_19_part_2022)#最新的日期的資料

##劃圖累計
dev.new()
ggplot()+
  geom_line(data=COVID_19_part_2022,#data.frame的資料
            aes(x=date,y=daily_vaccination_avg,
                colour = "daily_vaccination_avg"),size=1)+#折線
  geom_line(data=COVID_19_part_2022,#data.frame的資料
            aes(x=date,y=infection*40000,
                colour = "infection_rate"),size=1)+
  scale_y_continuous(name = "daily_vaccination_avg", sec.axis = sec_axis(~./40000 ,name = "infection"))+
  theme_bw()+
  annotate("text" #標籤
           ,COVID_19_part_2022$date[which.max(COVID_19_part_2022$daily_vaccination_avg)]
           ,y=COVID_19_part_2022$daily_vaccination_avg[which.max(COVID_19_part_2022$daily_vaccination_avg)]
           ,label=paste(COVID_19_part_2022$date[which.max(COVID_19_part_2022$daily_vaccination_avg)]
                        ,"當日確診數最高峰")
           ,family="微軟正黑體",colour="red",size=3,vjust=-0.5)+ 
  annotate("text" #標籤
           ,COVID_19_part_2022$date[which.max(COVID_19_part_2022$infection)]
           ,y=COVID_19_part_2022$daily_vaccination_avg[which.max(COVID_19_part_2022$infection)]
           ,label=paste(COVID_19_part_2022$date[which.max(COVID_19_part_2022$infection)]
                        ,"傳染率最高" 
                        ,COVID_19_part_2022$daily_vaccination_avg[which.max(COVID_19_part_2022$infection)]
                        ,"第一劑"
                        ,COVID_19_part_2022$people_vaccinated_per_hundred[which.max(COVID_19_part_2022$infection)]
                        ,"%"
                        ,"第二劑"
                        ,COVID_19_part_2022$people_fully_vaccinated_per_hundred[which.max(COVID_19_part_2022$infection)]
                        ,"%"
                        ,"第三劑"
                        ,COVID_19_part_2022$total_boosters_per_hundred[which.max(COVID_19_part_2022$infection)]
                        ,"%"
                        ,"\n累計確診占總人口"
                        ,round(COVID_19_part_2022$total_vaccination[which.max(COVID_19_part_2022$infection)]/
                                 COVID_19_part_2022$population[which.max(COVID_19_part_2022$infection)],4)*100
                        ,"% 、"
                        ,"死亡率"
                        ,round(COVID_19_part_2022$daily_die[which.max(COVID_19_part_2022$infection)]/
                                 COVID_19_part_2022$daily_vaccination[which.max(COVID_19_part_2022$infection)]*100,7)
                        ,"% 、"
                        ,"傳染率"
                        ,COVID_19_part_2022$infection[which.max(COVID_19_part_2022$infection)])
           ,family="微軟正黑體",colour="black",size=3,vjust=-17)+
  annotate("rect" #區間
           ,xmin=as.POSIXct(as_date(COVID_19_part_2022$date[which.max(COVID_19_part_2022$infection)])+22)
           ,xmax=as.POSIXct(as_date(COVID_19_part_2022$date[which.max(COVID_19_part_2022$infection)])+33)
           ,ymin=0
           ,ymax=85000
           ,alpha=0.1,fill="blue")+
  ggtitle(paste("臺灣 2022 COVID-19 傳染率最高發生在"
                ,COVID_19_part_2022$date[which.max(COVID_19_part_2022$infection)]
                ,"推測經過28天到"
                ,as_date(COVID_19_part_2022$date[which.max(COVID_19_part_2022$infection)])+28
                ,"前後5日區間達當日確診數最高峰  Source:衛部疾管署，美國衛生研究院 Updated on 2022-06-09"))+
  theme(text = element_text(family = "微軟正黑體"))+
  theme(plot.title = element_text(face="italic"
                                  ,colour = "red",hjust = 0.2,size = 10))#標題