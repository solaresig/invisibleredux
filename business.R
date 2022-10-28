#### Paper for the BHC####
#Author: Israel G Solares#
#contact:isgarcia at colmex.mx#
#Licence: CC by 4#
#language: English#

#### I. defining, extracting and cleaning####
####I.0.1 installing packages####
install.packages("tidyverse")
install.packages("rworldmap")
install.packages("rworldmapxtra")
####I.0. 2 defining working directory####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#### I.0.3 loading packages####
library(countrycode)
library(dplyr)
library(gganimate)
library(ggmap)
library(gtable)
library(grid)
library(gridExtra)
library(refinr)
library(rworldmapxtra)
library(rworldmap)
library(scales)
library(tidyverse)
library(tmap)
library(tmaptools)
library(vctrs)
library(viridis)
library(viafr)
library(plotly)
library(plyr)
####I. 0. 4. bases for maps####
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "black"),  
      panel.grid.minor = element_line(color = "black"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
}
base<- getMap()
base<- ggplot() + geom_polygon(data = base, aes(x=long, y = lat, group = group),  fill = "white", color="black")
base2<- getMap()
base2<- ggplot() + geom_polygon(data = base2, aes(x=long, y = lat, group = group),  fill = "black", color="black")

####I.0.4 defining functions####
cargar<-function(x){
  y<-read.csv(x,na.strings=c(""," ","NA"))
  y$X<-NULL
  return(y)
}
coords2country <- function(df){  
  ll<-subset(df, select=c("lon", "lat"))
  ll[is.na(ll)]<-FALSE
  countriesSP <- getMap(resolution='high')
  pointsSP <- SpatialPoints(ll, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP, use="complete.obs")
  indices$ADMIN<-tolower(indices$ADMIN)
  indices$ADMIN
}
buscar<-function(x){
  register_google(key = ""
  y<-geocode(as.character(x$loc))
  y<-as.data.frame(y)
  return(y)
}
buscar2<-function(x){
  y<-subset(x[which(!is.na(x$loc)),], select=c("loc"))
  y<-unique(y$loc)
  y<-as.data.frame(y)
  colnames(y)<-"loc"
  z<-buscar(y)
  y$lon<-z$lon
  y$lat<-z$lat
  return(y)
}
rfin<-function(x){
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
}
sname<-function(x){
  y<-ifelse(!is.na(x$lastname), 
            paste(x$lastname, ", ", substr(x$firstname, 1, 1), " " ,
                  ifelse(!is.na(substr(x$middlename, 1, 1)), substr(x$middlename, 1, 1),""), sep=""), 
            NA)
  y<-tolower(trimws(y))
  return(y)
}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
trimdigi<-function(x){
  y<-str_replace_all(x,  "C,", "0,")
  x<-str_replace_all(y,  "[(],", "0,")
  y<-str_replace_all(x,  "C(?=\\d)", "0")
  x<-str_replace_all(y,  "[(]0", "00")
  y<-str_replace_all(x,  "OO", "00")
  x<-str_replace_all(y,  "OC", "00")
  y<-str_replace_all(x,  "CO", "00")
  x<-str_replace_all(y,  "Q(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)Q", "0")
  x<-str_replace_all(y,  "O(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)O", "0")
  x<-str_replace_all(y,  "o(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)o", "0")
  x<-str_replace_all(y,  "u(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)u", "0")
  x<-str_replace_all(y,  "U(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)U", "0")
  x<-str_replace_all(y,  "D(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)D", "0")
  x<-str_replace_all(y,  "M(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)M", "0")
  x<-str_replace_all(y,  "G(?=\\d)", "0")
  y<-str_replace_all(x,  "E(?=\\d)", "0")
  x<-str_replace_all(y,  "(?<=\\d)G", "0")
  y<-str_replace_all(x,  "(?<=\\d)E", "0")
  x<-str_replace_all(y,  "I(?=\\d)", "1")
  y<-str_replace_all(x,  "X(?=\\d)", "0")
  x<-str_replace_all(y,  "(?<=\\d)I", "1")
  y<-str_replace_all(x,  "(?<=\\d)X", "0")
  x<-str_replace_all(y,  "T(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)T", "1")
  x<-str_replace_all(y,  "[|](?=\\d)", "1")
  y<-str_replace_all(x,  "(?<=\\d)[|]", "1")
  x<-str_replace_all(y,  "EO", "00")
  y<-str_replace_all(x,  "t(?=\\d)", "4")
  x<-str_replace_all(y,  "(?<=\\d)t", "4")
  y<-str_replace_all(x,  "L(?=\\d)", "1")
  x<-str_replace_all(y,  "(?<=\\d)L", "1")
  y<-str_replace_all(x,  "l(?=\\d)", "1")
  x<-trimws(str_replace_all(y,  "(?<=\\d)l", "1"))
  return(x)
}
viafbuscar<-function(x){
  y<-map(viaf_search(x, maximumResults=5), function(z) pluck(z$text[z$name_type=="Corporate Names"],1))
  z<-map(y,function(y) pluck(y$a, 1))
  z[sapply(z, is.null)] <- NA
  y<-unlist(z)
  return(y)}

#Z is 1.96 at 95% confidence level, we suppose a p=0.5, for a sample with replacement
muestra<-function(df){
  n<-ceiling((1.96^2)*((0.5*(1-0.5)/(0.05^2)))/(1+(0.5*(1-0.5)/((0.05^2)*nrow(df)))))
  y<-df[sample(nrow(df), n, replace=T), ]
  return(y)
}
errores<-function(df, org){
  mistakes0<-paste(c("resident", "miscellaneous(\\s+)(\\w+)", "(\\b)rector(\\b)"), collapse="|")
  org<-str_replace_all(org, mistakes0, "")
  df$firstname<-str_replace_all(df$firstname, mistakes0, "")
  df$middlename<-str_replace_all(df$middlename, mistakes0, "")
  df$lastname<-str_replace_all(df$lastname, mistakes0, "")
  df$lastname<-df$lastname%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
  df$firstname<-df$firstname%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
  df$middlename<-df$middlename%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
  df$firstname[which(df$firstname=="")]<-NA
  df$lastname[which(df$lastname=="")]<-NA
  df$middlename[which(df$middlename=="")]<-NA
  df$firstname<-ifelse(is.na(df$firstname)&!is.na(df$middlename), df$middlename, df$firstname)
  df$middlename<-ifelse((df$firstname==df$middlename)|(df$lastname==df$middlename), NA, df$middlename)
  df<-df[which(!is.na(df$firstname)&!is.na(df$lastname)),]
  df$simpname<-sname(df)
  mistakes<-c("ltd", "milling","r[e|c]gistered", "smelters", "company", "(\\b)box(\\b)","foundation","developmen", 
              "school", "aime", "prospectin", "(\\b)and(\\b)", "incorpo", "limite", "explora", "(\\b)mine(\\b)",
              "(\\b)mines(\\b)", "produc", "metal", "compan[y|i]", "valley", "africa", "o[f|l][f|l]ice", "(\\b)co(\\b)", 
              "chair", "manage", "reader(\\b)", "mited(\\b)", "(\\b)each(\\b)", "altern", "registr", "fully", 
              "local", "mineral", "share", "paid(\\b)", "trustee", "phone", "synd(\\b)", "district", "except")%>%paste(collapse = "|")
  df<-df[which(!is.na(df$simpname)&!str_detect(df$lastname, mistakes)&!str_detect(df$firstname, mistakes)&!str_detect(df$middlename, mistakes)),]
}

####profesionales####
officers<-read.csv("officers.csv")
officers<-errores(officers, officers$company)
officers2<-officers[which((officers$role=="engineer")|(officers$role=="metallurgist")|(officers$role=="chemist")),]
mengi<-read.csv("imm6.csv")
mengi$id<-paste("imm", rownames(mengi), sep="")
mengi$role<-mengi$prof
mengi$lastname<-str_extract(mengi$simpname, ".*(?=[,])")
mengi$firstname<-str_extract(mengi$simpname, "(?<=[,](\\s))(\\w)")
mengi$middlename<-str_extract(mengi$simpname, "(?<=[,](\\s)(\\w)(\\s))(\\w)")
mengi<-errores(mengi, mengi$org)
mengi<-mengi[order(mengi$org),]
profis<-read.csv("combo5_2.csv")
profis<-subset(profis, profis$type!="corpis", select=c("id", "year","type", "simpname", "lastname","firstname", "middlename", "position", "edu","source", "loc", "lon", "lat"))
profis<-profis[which(!is.na(profis$simpname)&(profis$year<1924)&!is.na(profis$year)), ]
profis<-profis[which(!is.na(str_extract(profis$edu, "eng|geol|assay|draw|chem|top|gen|sci"))|profis$type=="professional"),]
ama<-read.csv("ama.csv")
ama$id<-paste("ama", rownames(ama), sep="")
ama$simpname<-sname(ama)
mengi$edu<-mengi$prof
ama$edu<-"engineer"
officers2$edu<-officers2$role
officers2<-distinct(officers2, simpname, .keep_all=T)
officers2$source<-"myb"
profesionales<-rbind(subset(mengi, select=c("simpname", "edu", "source", "id")), 
                     subset(profis, select=c("simpname", "edu", "source", "id")), 
                     subset(ama, select=c("simpname", "edu", "source", "id")), 
                     subset(officers2, select=c("simpname", "edu", "source", "id")))
profesionales<-distinct(profesionales, simpname, .keep_all = T)
write.csv(profesionales, "profesionales11.csv", row.names=F)
write.csv(officers, "officers11.csv", row.names = F)
####MYB####
####Test####
together<-read.csv("together.csv")
toge2<-subset(together, !is.na(together$simpname))
profesionales<-read.csv("profesionales11.csv")
matchf<-left_join(
  toge2, 
  profesionales, by="simpname", match="all")
matcht<-matchf[which(!is.na(matchf$simpname)&!is.na(matchf$source)),]
testmat<-muestra(matcht)
testmat<-testmat[order(testmat$id.x, testmat$id.y),]
write.csv(testmat, "testmat.csv", row.names=F)
####Analisis of deciles####
together<-read.csv("together.csv")
together$cat<-str_extract(together$id, "[:alpha:]{1,}")
toge2<-subset(together, !is.na(together$simpname))
profesionales<-read.csv("profesionales11.csv")
profesionales$id<-NULL
matchf<-left_join(
  toge2, 
  profesionales, by="simpname", match="all")
match2<-matchf[which(!is.na(matchf$simpname)),]
match2$n<-1
match2$engis<-ifelse(!is.na(match2$source), 1, 0)
match2$capital<-as.numeric(match2$capital)
match2<-match2[which(!is.na(match2$capital)),]
match3<-match2%>%
  group_by(company, year)%>%
  nest()
####table cpaital per company####
#toge3<-read.csv("crp3bruto.csv")
#toge3$X<-NULL
#toge3$cat<-str_replace_all(toge3$id, "[^[:alpha:]]", "")
#toge3<-subset(toge3, !is.na(toge3$cat))
toge3<-subset(together, select=c("year", "company", "capital", "cat"))%>%distinct()
tablecies<- ddply(toge3, 
                 c("year", "cat"), 
                 summarise, 
                 Nocompanies=length(year),
                 Capital=sum(capital, na.rm = TRUE), 
                 capitalpc=mean(capital, na.rm=T))
tablecies$cat<-toupper(tablecies$cat)
tablecies<-subset(tablecies, tablecies$year<1924)
tablecies$Nocompanies<-ifelse(tablecies$cat=="AMM",tablecies$Nocompanies/2, tablecies$Nocompanies)
tablecies %>% 
  ggplot(aes(x = year, y = Nocompanies))+
  geom_point(aes(size=capitalpc),alpha=0.5)+
  geom_line(aes(linetype=cat))+
  theme(axis.text.x = element_text(size=7, angle=90))+
  scale_size(name="Capital per company") +
  scale_linetype(name="Catalogue")+
  scale_x_continuous(breaks = round(seq(min(tablecies$year), max(tablecies$year), by = 1),1))+
  scale_y_continuous(name="MYB",sec.axis=sec_axis(trans=~.*2, name="AMM"))+
  ggtitle("Number of companies registered at the Catalogues, 1887-1923")
toge3$period<-cut(as.numeric(as.character(toge3$year)), breaks=c(1888, 1903, 1908, 1912, 1915, 1923))
tablecies2<- ddply(toge3, 
                  c("period", "cat"), 
                  summarise, 
                  Nocompanies=length(year),
                  Capital=sum(capital, na.rm = TRUE), 
                  capitalpc=mean(capital, na.rm=T))
write.csv(tablecies, "tableciestemp.csv")
####Table myb professions####
matching<-matchf
matching$n<-1
matching$engis<-ifelse(!is.na(matching$source), 1, 0)
role2<-read.csv("roles2.csv")
matching<-left_join(matching, role2)
matching$cat<-str_extract(matching$id, "[:alpha:]{1,}")
write.csv(matching, "matching.csv")
matching$role<-NULL
matching$role2[which(is.na(matching$role2))]<-"Other"
myb<-subset(together, together$cat=="myb")
#officers<-read.csv("officers11.csv")
#officers$id<-paste("myb", officers$id, sep="")
#myb<-left_join(myb, subset(officers, select=c("id","role", "simpname")), by="id", match="all")
mybtemp<-myb[which(!is.na(myb$simpname)),]
mybtemp<-left_join(mybtemp, role2)
mybtemp$n<-1
profesionales<-read.csv("profesionales11.csv")
profesionales$id<-NULL
matchmybtemp<-left_join(
  mybtemp, 
  profesionales, by="simpname", match="all")
matchmybtemp$engis<-ifelse(!is.na(matchmybtemp$source), 1, 0)
proffs<-merge(
  left_join(
    ddply(matchmybtemp, 
          c("role2"), summarize, NMY=sum(n)), 
    ddply(matchmybtemp[which(matchmybtemp$engis==1),], 
          c("role2"), summarize, nmy=sum(n))), 
  left_join(
    ddply(matching[which(matching$cat=="amm"),], 
          c("role2"), summarize, NAM=sum(n)), 
    ddply(matching[which(matching$cat=="amm"&matching$engis==1),], 
          c("role2"), summarize, nam=sum(n))), all=T)
write.csv(proffs, "proffstemp.csv")

####Table MYB####
matchmyb<-match2[which(match2$cat=="myb"),]%>%
  group_by(company, year)%>%
  nest()
tmyb<-matchmyb%>%
  mutate(engis = map_dbl(.x=matchmyb$data, .f=~sum(.x$engis, na.rm=T)),
         capital = map_dbl(.x=matchmyb$data, .f=~mean(.x$capital, na.rm=T)), 
         n = map_dbl(.x=matchmyb$data, .f=~sum(.x$engis, na.rm=T)), 
         N = map_dbl(.x=matchmyb$data, .f=~sum(.x$n, na.rm=T)))%>%
  unnest(cols=c(engis, capital, n, N))
tmyb$N<-ifelse(tmyb$N==0, tmyb$n, tmyb$N)
tmyb$proengis<-tmyb$n/tmyb$N
tmyb<-tmyb[which(tmyb$proengis>0),]
tmyb<-tmyb[order(tmyb$company, tmyb$year),]
tmyb$quantilk<-with(tmyb, cut(capital, 
                              breaks=quantile(capital, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                              labels=c(1:10), include.lowest=TRUE))
tmyb$quantilp<-cut(tmyb$proengis, 
                   breaks=seq(0,1, by=0.0625), include.lowest=TRUE)

tablemy<-ddply(tmyb, 
               c("quantilp"),
               summarize, 
               capital=mean(capital, na.rm=T), 
               companies=length(company))
tablemy%>%
  ggplot()+
  geom_col(aes(x=quantilp, y=capital))+
  geom_smooth(aes(as.numeric(quantilp), capital), method = "loess", level=0.5)+
  geom_label(aes(x=quantilp, y=capital, label=companies), nudge_y =-0.01)+
  ylim(0, NA)+
  labs(x="Percentage of engineers in total employees", y="Mean capital in the coompany", 
       title="Percentage of engineers, mean capital and number of companies, MMYB")+
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5))

tablemy2<-ddply(tmyb, 
                c("quantilk"),
                summarize, 
                proengis=mean(proengis, na.rm=T), 
                companies=length(company), 
                engis=mean(engis))
graphmy1<-tablemy2%>%ggplot()+
  geom_point(aes(x=quantilk, y=proengis))+
  #geom_line(aes(as.numeric(quantilk), proengis))+
  geom_smooth(aes(as.numeric(quantilk), proengis), method = "lm", level=0.75)+
  labs(x = "Deciles in capitalization", y = "Mean percentage of engineers")
graphmy2<-tablemy2%>%ggplot()+
  geom_point(aes(x=quantilk, y=engis))+
  #geom_line(aes(as.numeric(quantilk), engis))+
  geom_smooth(aes(as.numeric(quantilk), engis), method = "lm", level=0.75)+
  labs(x = "Deciles in capitalization", y = "Mean number of engineers")
grid.arrange(graphmy2, graphmy1,  nrow=1,# ncol=2, 
             top=textGrob("Engineers in upper management in deciles of capitalization, MYB"))
cor(tmyb$engis, tmyb$capital)
cor(tmyb$proengis, tmyb$capital)

tablemy1<-ddply(tmyb, 
                c("year"),
                summarize, 
                perengin=mean(proengis, na.rm=T),
                engis=mean(engis),
                companies=length(company),
                capital=round(mean(capital)/1000, digits=0)) 
graphmyb<-tablemy1%>%
  ggplot()+
  geom_col(aes(x=year, y=perengin))+
  geom_smooth(aes(x=as.numeric(year), y=perengin), method = "loess", level=0.45)+
  geom_text(aes(x=year, y=perengin, label=capital), nudge_y =-0.03, angle=90, colour="White")+
  labs(x="Year", y="Percentage of engineers in total employees", 
       title="Figure 3. Percentage of engineers among Listed Management Positions", 
       subtitle = "Mean capital(white)")+
  scale_x_continuous(breaks = round(seq(min(tablemy1$year), max(tablemy1$year), by = 1),1))+
  theme(axis.text.x = element_text(angle=90), 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
graphmyb+scale_y_continuous(limits=c(0.22,0.46),oob = rescale_none)
####Table AMM####
matchamm<-match2[which(match2$cat=="amm"),]%>%
group_by(company, year)%>%
  nest()
tamm<-matchamm%>%
  mutate(engis = map_dbl(.x=matchamm$data, .f=~sum(.x$engis, na.rm=T)),
         capital = map_dbl(.x=matchamm$data, .f=~mean(.x$capital, na.rm=T)), 
         n = map_dbl(.x=matchamm$data, .f=~sum(.x$engis, na.rm=T)), 
         N = map_dbl(.x=matchamm$data, .f=~sum(.x$n, na.rm=T)))%>%
  unnest(cols=c(engis, capital, n, N))
tamm$N<-ifelse(tamm$N==0, tamm$n, tamm$N)
tamm$proengis<-tamm$n/tamm$N
tamm<-tamm[which(tamm$proengis>0),]
tamm<-tamm[order(tamm$company, tamm$year),]
tamm$quantilk<-with(tamm, cut(capital, 
                              breaks=quantile(capital, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                              labels=c(1:10), include.lowest=TRUE))
tamm$quantilp<-cut(tamm$proengis, 
                   breaks=seq(0,1, by=0.0625), include.lowest=TRUE)

tableam<-ddply(tamm, 
               c("quantilp"),
               summarize, 
               capital=mean(capital, na.rm=T), 
               companies=length(company))
tableam%>%
  ggplot()+
  geom_col(aes(x=quantilp, y=capital))+
  geom_smooth(aes(as.numeric(quantilp), capital), method = "loess", level=0.5)+
  geom_label(aes(x=quantilp, y=capital, label=companies), nudge_y =-0.01)+
  ylim(0, NA)+
  labs(x="Percentage of engineers in total employees", y="Mean capital in the coompany", 
       title="Percentage of engineers, mean capital and number of companies, MAMM")+
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5))

tableam2<-ddply(tamm, 
                c("quantilk"),
                summarize, 
                proengis=mean(proengis, na.rm=T), 
                companies=length(company), 
                engis=mean(engis))
grapham1<-tableam2%>%ggplot()+
  geom_point(aes(x=quantilk, y=proengis))+
  #geom_line(aes(as.numeric(quantilk), proengis))+
  geom_smooth(aes(as.numeric(quantilk), proengis), method = "lm", level=0.75)+
  labs(x = "Deciles in capitalization", y = "Mean percentage of engineers")
grapham2<-tableam2%>%ggplot()+
  geom_point(aes(x=quantilk, y=engis))+
  #geom_line(aes(as.numeric(quantilk), engis))+
  geom_smooth(aes(as.numeric(quantilk), engis), method = "lm", level=0.75)+
  labs(x = "Deciles in capitalization", y = "Mean number of engineers")
grid.arrange(grapham2, grapham1,  nrow=1,# ncol=2, 
             top=textGrob("Engineers in upper management in deciles of capitalization, AMM"))

tableam1<-ddply(tamm, 
                c("year"),
                summarize, 
                perengin=mean(proengis, na.rm=T),
                engis=mean(engis),
                companies=length(company),
                capital=round(mean(capital)/1000, digits=0)) 
graphamm<-tableam1%>%
  ggplot()+
  geom_col(aes(x=year, y=perengin))+
  geom_smooth(aes(x=as.numeric(year), y=perengin), method = "loess", level=0.45)+
  geom_text(aes(x=year, y=perengin, label=capital), nudge_y =-0.03, angle=90, colour="White")+
  labs(x="Year", y="Percentage of engineers in total employees", 
       title="Figure 3. Percentage of engineers among Listed Management Positions", 
       subtitle = "Mean capital(white)")+
  theme(axis.text.x = element_text(angle=90), 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
graphamm#+scale_y_continuous(limits=c(0.25,0.52),oob = rescale_none)


####AIME and Alumni####
profis<-read.csv("combo5_2.csv")
profis<-subset(profis, profis$type=="alumni", select=c("year","simpname", "lastname","firstname", "middlename", "position", "edu","source"))
profis<-profis[!is.na(profis$simpname), ]
profis$position<-trimws(tolower(profis$position), which="both")
profis$position<-str_replace_all(profis$position, "['|’]", "")
profis$position<-str_replace_all(profis$position, "(\\b)mgr(\\b)", "manager")
profis$position<-str_replace_all(profis$position, "(\\b)engr(\\b)", "engineer")
profis$position<-str_replace_all(profis$position, "(\\b)supt(\\b)", "superintendent")
profis$position<-str_replace_all(profis$position, "(\\b)asst(\\b)", "assistant")
llave<-c("engineer", "manager", "superintendent", "foreman", "president", "student", "geolog", "consulting", 
         "attorney", "prof", "chemist", "draftsman", "salesman", "architect", "inspector", "clerk", "analyst", 
         "electrician", "metallurg", "hydrographer", "civil", "council", "clergy", "designer", "specialist", 
         "supervisor", "analyst", "deputy", "editor", "electric", "broker", "experiment", "govern", "dealer", 
         "merchant", "owner", "proprietor","contractor", "retired", "died", "teacher", "manufacturer", "machinist",
         "administ", "academ", "graduate", "builder", "assayer", "assistant", "accountant", "adjuster")
for(i in 1:length(llave)){
  profis$position<-ifelse(!is.na(str_extract(profis$position, llave[i])), str_extract(profis$position, llave[i]), profis$position)
}
#write.csv(unique(profis$position), "temp.csv", row.names = F)
keyprofis<-read.csv("keyprofis.csv", na.strings = c("NA", "", " "))
profis<-left_join(profis, keyprofis, by="position")
profis<-subset(profis, profis$position!="student"&profis$position!="graduate")
profisnul<-subset(profis, is.na(profis$role2))
profis<-subset(profis, !is.na(profis$role2))
profis<-profis[which(profis$year>1850),]
profis$gradyear<-ifelse(profis$simpname!=lag(profis$simpname), profis$year, NA)
profis$gradyear[1]<-profis$year[1]
profis<-fill(profis, gradyear, .direction="down")
profis$roleh<-ifelse(profis$simpname!=lead(profis$simpname), profis$role2, NA)
profis$decade<-cut(profis$year, 
                   breaks=seq(1850, 1940, by=10), 
                   labels=c("1850", "1860", "1870", "1880", "1890", "1900", "1910", "1920", "1930"),
                   include.lowest=TRUE)
profis$decgrad<-cut(profis$gradyear, 
                   breaks=seq(1850, 1940, by=10), 
                   labels=c("1850", "1860", "1870", "1880", "1890", "1900", "1910", "1920", "1930"),
                   include.lowest=TRUE)

last<-subset(profis, !is.na(profis$roleh))
last$decade<-last$decgrad
alumni<-subset(profis, profis$source!="taime")

tegresados<-ddply(alumni, 
                  c("decade", "role2"), 
                  summarize, 
                  n=length(year))
tlast<-ddply(last, 
                c("decade", "role2"), 
                summarize, 
                n=length(year))
tegres2<-ddply(alumni, 
               c("decade"), 
               summarize, 
               N=length(year))
tlast2<-ddply(last, 
              c("decade"), 
              summarize, 
              N=length(year))
tegresados<-left_join(tegresados, tegres2, by="decade")
tlast<-left_join(tlast, tlast2, by="decade")
tegresados$percentage<-100*tegresados$n/tegresados$N
tlast$percentage<-100*tlast$n/tlast$N
tegresados$role2<-str_to_title(tegresados$role2)
tlast$role2<-str_to_title(tlast$role2)
library(ggpattern)

ggplot(tegresados, aes(y=percentage, x=decade ,pattern_type = role2)) +
  geom_bar_pattern(position="stack", 
                   pattern              = 'magick',
                   width                = 1,
                   stat                 = "identity",
                   fill                 = 'white',
                   colour               = 'black'
  )+
  scale_pattern_type_manual(values = c(Government = 'horizontal2', Management = 'left45',
                                       Other ='gray100' , Technical = 'vertical3'))+
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5))+
  labs(pattern_type = "Role", x="Decade", y="Percentage")+
  ggtitle("Roles reported by alumni against number of alumni reporting per year")

ggplot(tlast, aes(y=percentage, x=decade ,pattern_type = role2)) +
  geom_bar_pattern(position="stack", 
                   pattern              = 'magick',
                   width                = 1,
                   stat                 = "identity",
                   fill                 = 'white',
                   colour               = 'black'
  )+
  scale_pattern_type_manual(values = c(Government = 'horizontal2', Management = 'left45',
                                       Other ='gray100' , Technical = 'vertical3'))+
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5))+
  labs(pattern_type = "Role", x="Decade", y="Percentage")+
  ggtitle("Latest role reported by Alumni per cohort")

####Gender####
together<-read.csv("together.csv")
toge2<-subset(together, !is.na(together$simpname))
profesionales<-read.csv("profesionales11.csv")
combo<-read.csv("combo5_2.csv")
profes2<-left_join(
  profesionales, 
  subset(combo, select=c("simpname", "firstname", "middlename"))
) %>% distinct()
toge2<-left_join(
  toge2, 
  subset(combo, select=c("simpname", "firstname", "middlename"))
) %>% distinct()
names<-as.vector(rbind(as.vector(combo$firstname), as.vector(combo$middlename))) %>% unique()
names<-names[which(nchar(names)>2)]
library(gender)
library(genderdata)
genero<-gender(names, years=c(1880, 1900))
genero<-subset(genero, select=c("name", "gender"))
library(tidyverse)
profes2<-left_join(profes2, genero, by=c("firstname"="name"))
toge2<-left_join(toge2, genero, by=c("firstname"="name"))
profesg<-count(profes2, "gender")
togeg<-count(toge2, "gender")
crp<-read.csv("crp3.csv")
crp1920<-subset(crp, crp$year==1920)
crp1920$catalogue<-str_extract(crp1920$id, "[:alpha:]*")
crp1920<-subset(crp1920, select=c("year", "catalogue", "company", "capital")) %>%distinct()
t1920<-ddply(crp1920, "catalogue", summarise, capital=mean(capital, na.rm=T))
