title: "Project1"
author: "SAI SHASHANK NUKALA"
date: "2/28/2020"


library(backports)
library(corrplot)
library(devtools)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(gridGraphics)
library(gridtext)
library(Hmisc)
library(janitor)
library(lubridate)
library(magrittr)
library(maptools)
library(reshape2)
library(scales)
library(tidyverse)
library(treemapify)
library(usmap)
library(zeallot)
library(plotly)
library(processx)

#importing the farmer's market data set

fmarket <- read.csv("C:/Users/saish/Downloads/fmarket.csv", header=TRUE, sep= ',')
state_map<- fmarket %>%
  dplyr::group_by(State) %>% summarise(count=n())
colnames(state_map)<- c("state","count")

#plotting state wise farmer's market distribution

us_map_state<- plot_usmap(regions = c("state"),
                          include = c(), exclude = c(), data = state_map,
                          values = "count", theme = theme_map(), labels = FALSE,
                          label_color = "black") +
  scale_fill_continuous(low = "sky blue", high = "midnight blue",guide = "colourbar", name = "Farmer's market count") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
  theme(legend.position = "right")+labs(title="state wise Farmer's market distribution")

ggsave(us_map_state,filename= "US state map.png", path="C:/Users/saish/Documents/project1", dpi = 320)


#Plotting number of farmer's market for product types vs payment types

Credit<- sum(unlist(str_count(fmarket$Credit, "Y")))
WIC<- sum(unlist(str_count(fmarket$WIC, "Y")))
WICcash<- sum(unlist(str_count(fmarket$WICcash, "Y")))
SFMNP<- sum(unlist(str_count(fmarket$SFMNP, "Y")))
SNAP<- sum(unlist(str_count(fmarket$SNAP, "Y")))
Organic<- sum(unlist(str_count(fmarket$Organic, "Y")))
Bakedgoods<- sum(unlist(str_count(fmarket$Bakedgoods, "Y")))
Cheese<- sum(unlist(str_count(fmarket$Cheese, "Y")))
Crafts<- sum(unlist(str_count(fmarket$Crafts, "Y")))
Flowers<- sum(unlist(str_count(fmarket$Flowers, "Y")))
Eggs<- sum(unlist(str_count(fmarket$Eggs, "Y")))
Seafood<- sum(unlist(str_count(fmarket$Seafood, "Y")))
Herbs<- sum(unlist(str_count(fmarket$Herbs, "Y")))
Vegetables<- sum(unlist(str_count(fmarket$Vegetables, "Y")))
Honey<- sum(unlist(str_count(fmarket$Honey, "Y")))
Jams<- sum(unlist(str_count(fmarket$Jams, "Y")))
Maple<- sum(unlist(str_count(fmarket$Maple, "Y")))
Meat<- sum(unlist(str_count(fmarket$Meat, "Y")))
Nursery<- sum(unlist(str_count(fmarket$Nursery, "Y")))
Nuts <- sum(unlist(str_count(fmarket$Nuts, "Y")))
Plants<- sum(unlist(str_count(fmarket$Plants, "Y")))
Poultry<- sum(unlist(str_count(fmarket$Poultry, "Y")))
Prepared<- sum(unlist(str_count(fmarket$Prepared, "Y")))
Soap<- sum(unlist(str_count(fmarket$Soap, "Y")))
Trees<- sum(unlist(str_count(fmarket$Trees, "Y")))
Wine<- sum(unlist(str_count(fmarket$Wine, "Y")))
Coffee<- sum(unlist(str_count(fmarket$Coffee, "Y")))


Products<- c("Organic","Bakedgoods","Cheese","Crafts","Flowers","Eggs","Seafood","Herbs","Vegetables","Honey",
             "Jams","Maple","Meat","Nursery","Nuts","Plants","Poultry","Prepared","Soap","Trees","Wine","Coffee")

Pr_count<- c(Organic,Bakedgoods,Cheese,Crafts,Flowers,Eggs,Seafood,Herbs,Vegetables,Honey,Jams,Maple,Meat,
             Nursery,Nuts,Plants,Poultry,Prepared,Soap,Trees,Wine,Coffee)

Payment<- c("Credit", "WIC", "WICcash","SFMNP","SNAP")

Pa_count<- c(Credit, WIC, WICcash,SFMNP,SNAP)

Product_df<- data.frame(Products,Pr_count)
Payment_df<-data.frame(Payment,Pa_count)


location_df<- fmarket %>%
  group_by(Location)%>% summarise(count=n())

location_df$Location <- as.character(location_df$Location)

location_df[1,1]<- NA
location_df[5,1]<- "Faith-based institution"
location_df[9,1]<- "On a farm from"


NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.ref <- c(NE.name)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")

MW.ref <- c(MW.name)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas","Virgin Islands","Puerto Rico")

S.ref <- c(S.name)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")

W.ref <- c(W.name)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)

rand3<-data.frame(lapply(region.list, "length<-", max(lengths(region.list))))
blank_vec<- c(1:19)
rand4<- cbind(rand3,blank_vec)
rand4<- melt(rand4,id=c("blank_vec"))
rand4<-rand4 %>% drop_na()
region_df <- data.frame(rand4$variable,rand4$value)
colnames(region_df)<- c("Region", "State")
fmarket1<- merge(x=region_df,y=fmarket,by="State",all.x=TRUE)

Region_df<- fmarket1 %>%
  group_by(Region)%>% summarise(count=n())



p1<-ggplot(mapping = aes(x=reorder(Products,Pr_count), y=Pr_count, fill=Pr_count))  +
  geom_bar(stat = "identity") + ylab('number of farmer markets') + xlab('Product types') + coord_flip()+
  theme(legend.position="none")        

p2<- ggplot(mapping = aes(x=reorder(Payment,Pa_count), y=Pa_count, fill=Pa_count))  +
  geom_bar(stat = "identity") + ylab('number of farmer markets') + xlab('Payment types')+coord_flip() +
  theme(legend.position="none")

p3<- location_df %>%drop_na() %>% ggplot(mapping = aes(x=reorder(Location,count), y=count, fill=count))  +
  geom_bar(stat = "identity") + ylab('number of farmer markets') + xlab('Location') + coord_flip()+
  theme(legend.position="none")

p4<- Region_df %>% ggplot(mapping = aes(x=reorder(Region,count), y=count, fill=count))  +
  geom_bar(stat = "identity") + ylab('number of farmer markets') + xlab('USA Regions') +
  theme(legend.position="none")


FM_count<- grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2,top = textGrob("FM count:Product vs Payment vs Region vs Location",
                                                                 gp=gpar(fontsize=14,font=3)))

ggsave(FM_count,filename= "FM count.png", path="C:/Users/saish/Documents/project1", dpi = 400, width=10,height=10)

#treemap for state and counties vs fm count


state_Tree1<- fmarket %>%
  group_by(State,County)%>% summarise(count=n())


state_Tree1<-state_Tree1%>%mutate_all(na_if,"") %>% drop_na()


State_Tree<-state_Tree1%>% ggplot(aes(area=count,label=County,fill=State, subgroup=State))+
  geom_treemap() + geom_treemap_subgroup_border() + 
  geom_treemap_subgroup_text(place="centre",grow= T, alpha=0.4, colour= "black", fontface="italic", min.size=0) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",grow = FALSE)+
  theme(legend.position="none")

ggsave(State_Tree,filename= "State Tree.png", path="C:/Users/saish/Documents/project1", dpi = 400, width=10,height=8) 

#region and state wise distribution of payment types


stacked_region<-data.frame(fmarket1$Region,fmarket1$Credit,fmarket1$WIC,fmarket1$WICcash,
                           fmarket1$SFMNP,fmarket1$SNAP)


colnames(stacked_region)<- c("Region","Credit","WIC","WICcash","SFMNP","SNAP")
stacked_region<-melt(stacked_region, id=c("Region"))

stacked_region1<- stacked_region %>% filter(value=="Y") %>% group_by(Region, variable)%>%
  dplyr::summarise(count = n())

stacked_bar<- stacked_region1 %>% ggplot(mapping = aes(x=reorder(variable,count), y=count, fill=Region)) + 
  geom_bar(stat = "identity")+ ylab('number of farmer markets') + xlab('Payment types')

ggsave(stacked_bar,filename= "Payment vs Region.png", path="C:/Users/saish/Documents/project1",dpi = 400,
       width=10,height=8) 


stacked_df<-data.frame(fmarket$State,fmarket$Credit,fmarket$WIC,fmarket$WICcash,fmarket$SFMNP,fmarket$SNAP)
colnames(stacked_df)<- c("State","Credit","WIC","WICcash","SFMNP","SNAP")
stacked_df<-melt(stacked_df, id=c("State"))
stacked_df1<- stacked_df %>% filter(value=="Y") %>% group_by(State, variable)%>%dplyr::summarise(count = n())
colnames(stacked_df1)<- c("State","Payment","count")

scaterplot1<-stacked_df1 %>% 
  ggplot(mapping = aes(x=reorder(State,count), y=count, group=Payment,colour=Payment),alpha=0.4) +
  geom_point()+ ylab('number of farmer markets') + xlab('US states')+ coord_flip()

ggsave(scaterplot1,filename= "Payment vs State.png", path="C:/Users/saish/Documents/project1",dpi = 400, width=10,height=8) 

#region wise distribution of products

stacked_region2<-data.frame(fmarket1$Region,fmarket1$Organic,fmarket1$Bakedgoods,fmarket1$Cheese,fmarket1$Crafts,
                            fmarket1$Flowers,fmarket1$Eggs,fmarket1$Seafood,fmarket1$Herbs,fmarket1$Vegetables,
                            fmarket1$Honey,fmarket1$Jams,fmarket1$Maple,fmarket1$Meat,fmarket1$Nursery,fmarket1$Nuts,
                            fmarket1$Plants,fmarket1$Poultry,fmarket1$Prepared,fmarket1$Soap,fmarket1$Trees,
                            fmarket1$Wine,fmarket1$Coffee)

colnames(stacked_region2)<- c("Region","Organic","Bakedgoods","Cheese","Crafts","Flowers","Eggs","Seafood","Herbs",
                              "Vegetables","Honey","Jams","Maple","Meat","Nursery","Nuts","Plants","Poultry",
                              "Prepared","Soap","Trees","Wine","Coffee")

stacked_region2<-melt(stacked_region2, id=c("Region"))

stacked_region3<- stacked_region2 %>% filter(value=="Y") %>% group_by(Region, variable)%>%
  dplyr::summarise(count = n())
colnames(stacked_region3)<- c("Region","Product","count")

stacked_region4<-stacked_region3 %>% ggplot(mapping = aes(x=reorder(Product,count), y=count, 
                                                          fill=Region,colour=Region)) + geom_bar(stat = "identity")+
  ylab('Product Types') + xlab('Number of farmer markets')+coord_flip()

ggsave(stacked_region4,filename= "Product vs Region.png", path="C:/Users/saish/Documents/project1",dpi = 400, width=10,height=8)



#social media types and coverage region wise

social_media<-data.frame(fmarket$Website,fmarket$Facebook,fmarket$Twitter,fmarket$Youtube,fmarket$OtherMedia)
colnames(social_media)<- c("Website","Facebook","Twitter","Youtube","OtherMedia")

social_media<- social_media %>%
  group_by(Website,Facebook,Twitter,Youtube,OtherMedia)%>% summarise(count=n()) 

social_media[1,]<- NA

social_media1<-melt(social_media, id=c("count"))

social_media1 <- social_media1 %>% mutate_all(na_if,"") %>% drop_na() %>% group_by(variable)%>% summarise(count=n())



fig <- social_media1 %>% plot_ly(labels = ~variable, values = ~count,marker = list(colors = colors))
fig <- fig %>% add_pie(hole = 0.5)
donut_chart<-fig %>% layout(title = "Social Media %",  showlegend = TRUE,
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



social_media2<-data.frame(fmarket1$Region,fmarket1$Website,fmarket1$Facebook,fmarket1$Twitter,fmarket1$Youtube,
                          fmarket1$OtherMedia)
colnames(social_media2)<- c("Region","Website","Facebook","Twitter","Youtube","OtherMedia")

social_media2<-melt(social_media2, id=c("Region"))

social_media3<- social_media2 %>% mutate_all(na_if,"") %>% drop_na() %>% group_by(Region,variable)%>% 
  summarise(count=n())
colnames(social_media3)<- c("Region","Social_Media","count")

grouped_media<-social_media3%>%ggplot(mapping = aes(x=reorder(Region,count), y=count, fill=Social_Media)) + 
  geom_bar(stat="identity", position="dodge")+xlab('Regionally distributed social media') +
  ylab('Number of farmer markets')

ggsave(grouped_media,filename= "social Media.png", path="C:/Users/saish/Documents/project1",dpi = 400, width=10, height=10)


#seosonality plot and availability of farmer's markets



season_df<- data.frame(fmarket1$Region,fmarket1$Season1Date,fmarket1$Season2Date,fmarket1$Season3Date,
                       fmarket1$Season4Date)
colnames(season_df)<- c("Region","Season1Date","Season2Date","Season3Date","Season4Date")

season_df1<-season_df %>% 
  mutate(Season1Date = as.Date(Season1Date, format = "%m/%d/%y"),
         Season2Date = as.Date(Season2Date, format = "%m/%d/%y"),
         Season3Date = as.Date(Season3Date, format = "%m/%d/%y"),
         Season4Date = as.Date(Season4Date, format = "%m/%d/%y")) 

season_df1$Season1Date<- round_date(season_df1$Season1Date,unit = "month")
season_df1$Season2Date<- round_date(season_df1$Season2Date,unit = "month") 
season_df1$Season3Date<- round_date(season_df1$Season3Date,unit = "month") 
season_df1$Season4Date<- round_date(season_df1$Season4Date,unit = "month")
season_df1$Season1Date <-month(ymd(season_df1$Season1Date), label = TRUE, abbr = FALSE)
season_df1$Season2Date <-month(ymd(season_df1$Season2Date), label = TRUE, abbr = FALSE)
season_df1$Season3Date <-month(ymd(season_df1$Season3Date), label = TRUE, abbr = FALSE)
season_df1$Season4Date <-month(ymd(season_df1$Season4Date), label = TRUE, abbr = FALSE)

season_df2<- melt(season_df1,id=c("Region"))

season_df3 <-season_df2 %>%dplyr::group_by(Region,value) %>% summarise(count=n()) %>% drop_na()
season_df3$value<- factor(season_df3$value, levels=month.name)

Line_graph<-ggplot(season_df3,mapping=aes(x=value, y=count, group=Region, color=Region)) + 
  geom_line()+geom_point() + scale_x_discrete(limits = month.name)+ xlab('Months') + 
  ylab('Number of farmer markets')+labs(title="Commencement of Farmer's Markets Month wise")

ggsave(Line_graph,filename= "Season_line.png", path="C:/Users/saish/Documents/project1", dpi = 400, width=10, height=10)


season_df4<-season_df

season_df4$Season1Date<- format(as.Date(season_df4$Season1Date, format="%d/%m/%Y"),"%Y")
season_df4$Season2Date<- format(as.Date(season_df4$Season2Date, format="%d/%m/%Y"),"%Y")
season_df4$Season3Date<- format(as.Date(season_df4$Season3Date, format="%d/%m/%Y"),"%Y")
season_df4$Season4Date<- format(as.Date(season_df4$Season4Date, format="%d/%m/%Y"),"%Y")

season_df5<- melt(season_df4,id=c("Region"))

season_df5<-season_df5 %>%dplyr::group_by(Region,value) %>% summarise(count=n()) %>% drop_na()

line_graph2<-ggplot(season_df5,mapping=aes(x=value, y=count, group=Region, color=Region)) + 
  geom_line()+geom_point()+xlab('Years') + ylab('Number of farmer markets')+
  labs(title="The growth of farmer's markets over the years")

ggsave(line_graph2,filename= "line-2.png", path="C:/Users/saish/Documents/project1",dpi = 400, width=10, height=10)


season_df6<-data.frame(fmarket1$Season1Date,fmarket1$Season2Date,fmarket1$Season3Date,fmarket1$Season4Date)
colnames(season_df6)<- c("Season1Date","Season2Date","Season3Date","Season4Date")

season_df6$Season1Date<- format(as.Date(season_df6$Season1Date, format="%d/%m/%Y"),"%Y")
season_df6$Season2Date<- format(as.Date(season_df6$Season2Date, format="%d/%m/%Y"),"%Y")
season_df6$Season3Date<- format(as.Date(season_df6$Season3Date, format="%d/%m/%Y"),"%Y")
season_df6$Season4Date<- format(as.Date(season_df6$Season4Date, format="%d/%m/%Y"),"%Y")

blank_vec1<- c(1:8791)
season_df6<- cbind(season_df6,blank_vec1)
season_df6<- melt(season_df6,id=c("blank_vec1"))

year_wise<- data.frame(season_df6$value)
colnames(year_wise)<- c("Year")

year_wise<- year_wise %>%drop_na %>% group_by(Year)%>% summarise(count=n())

line_graph3<-ggplot(year_wise,mapping=aes(x=Year, y=count,group= 1))+geom_line()+geom_point()+xlab('Years') +
  ylab('Number of farmer markets')+labs(title="The Rise of Farmer's Markets since 2010")

ggsave(line_graph3,filename= "line-3.png", path="C:/Users/saish/Documents/project1",dpi = 400, width=10, height=10)