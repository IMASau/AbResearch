#clear environment
#rm(list=ls(all=TRUE))
library(dplyr)
library(ggplot2)
library(scales)
library(scales)
library(tidyr)
library(gdata)
library(xlsx)

juv <- read.xlsx(
 "D:/Fisheries Research/Abalone/AbResearchData/pop/2017/Juvenile_data_2016.xlsx",
 sheetName = "AllSites", col.names = TRUE
)

colnames(juv) <- tolower(colnames(juv))
juv <- rename(juv, survdate = date)
juv$string <- as.factor(juv$string)
juv$plate <- as.factor(juv$plate)

platearea <- 0.503

juv.sl <- filter(juv, !is.na(ab_sl))

#juv$ab_sl <- NAToUnknown(x = juv$ab_sl, unknown = 0)

#ab_count <- ddply(juv, "survdate", summarise, ab_ct = length(ab_sl))

## Because sites were surveyed on different days, can't do a fully crossed complete() function
#ab_count <- filter(juv, !is.na(ab_sl) & ab_sl <= 80) %>%

juv$survindex <- as.factor(paste(juv$location, juv$survdate, juv$string, juv$plate, sep="_"))

ab_count <- filter(juv, ab_sl >=40 & ab_sl <= 60) %>% 
 group_by(survindex) %>%
 summarise(ab_n =n()) %>%  #as.data.frame()
 complete(survindex, fill = list(ab_n = 0)) %>%
 as.data.frame()
 
ab_count$absm <- ab_count$ab_n * (1/platearea)
ab2plus <- extract(ab_count, survindex, regex = "_", into = c("location", "survdate", "string","plate"))


ab2plus <- data.frame(colsplit(ab_count$survindex, pattern="_", c("location", "survdate", "string","plate")), ab_count$survindex, ab_count$ab_n, ab_count$absm)

df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")
df %>% extract(x, c("A", "B"), "([[:alnum:]]+)-([[:alnum:]]+)")


ab2plus <- 

# #replace NA with 0
# juv[is.na(juv)] <- 0
# summary(juv)

juv<-juv[1:6]
#remove NA's
juv <- juv[complete.cases(juv[,5]),]
summary(juv)

# SORTING OUT THE DATES SO THEY ARE IN USEFUL FORMAT
#juv$survdate<-as.survdate(juv$survdate, origin, format="%d/%m/%y")
# juv$survdate <- strptime(as.character(juv$survdate), "%d/%m/%Y")
# juv$survdate <- format(juv$survdate, "%d/%b/%Y")
# juv$survdate <- as.date(juv$survdate, format="%d/%b/%Y")



# Betsey<-droplevels(subset(juv, location== "BI" & string== "2"))
# Betsey10<-droplevels(subset(Betsey, plate >=5))
# Betsey10<-droplevels(subset(Betsey10, plate <=7))

# #remove NAs
# Betsey10[complete.cases(Betsey10[,5]),]


ggplot(juv.sl, aes(x=ab_sl, color=location))+
  geom_histogram(stat = "bin", colour="grey", binwidth = 5)+
  scale_x_continuous(limits=c(0, 150)) +
  
  xlab("Shell Length (mm)") + ylab("Frequency") +
  theme_bw()


ggplot(juv.sl, aes(x=ab_sl)) + 
  ylab("Frequency") +
  xlab("Shell Length (mm)")+
  geom_histogram(alpha = 0.2, binwidth = 5)+
  #ggtitle(paste(dum$SubBlockNo, FishYear))+
  #labs(title= Yeardum$SubBlockNo, size=10)+
  #geom_histogram(binwidth=50)+
  theme_bw()+
  facet_grid(. ~ location)



#rearregage DF giving number of juveniles by location, survdate and string.
Pick <- aggregate(x = juv, by = list(juv$location, juv$survdate, juv$string), FUN = "length")
Pick<-Pick[1:4]
names(Pick)[names(Pick)=="Group.2"] <- "survdate"
names(Pick)[names(Pick)=="Group.1"] <- "Site"
names(Pick)[names(Pick)=="Group.3"] <- "string"
names(Pick)[names(Pick)=="location"] <- "Ab_Sum"

ggplot(ab_count, aes(y=ab_ct, x=survdate)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ location)

#ggplot(Pick, aes(Ab_Sum, fill=survdate)) + geom_bar(position="dodge")


#~~~~~~~~~~~~~~~~~~~~~~~~~BY SITE ~~~~~~~~~~~~~~~~~~~~~~~~~~~
#rearregage DF giving number of juveniles by location and survdate.
BI<-droplevels(subset(Pick, Pick$Site=="BI"))
BI$survdate<-as.factor(BI$survdate)

ab_count <- filter(juv, !is.na(ab_sl) & location =='BI') %>% group_by(location, survdate, string) %>%
 summarise(ab_ct =n()) %>% as.data.frame()


ggplot(ab_count, aes(y=ab_ct, x=survdate, fill=string)) +
  ggtitle("Betsey Island")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88))

+
 # scale_x_discrete(limits=c("27 Jun", "28 Jul", "18 Aug", "25 Sep", "08 Oct", "09 Oct"))

GIII<-droplevels(subset(Pick, Pick$Site=="GIII"))

ggplot(GIII, aes(y=Ab_Sum, x=survdate, fill=string)) +
  ggtitle("George 3rd Rock")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88))+
  scale_x_discrete(limits=c("11 Aug", "21 Sep", "13 Oct"))

BR_B<-droplevels(subset(Pick, Pick$Site=="BR_B"))

ggplot(BR_B, aes(y=Ab_Sum, x=survdate, fill=string)) +
  ggtitle("Black Reef Boulder")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.95,.88))+
  scale_x_discrete(limits=c("12 Aug", "30 Sep", "13 Oct"))



BR_S<-droplevels(subset(Pick, Pick$Site=="BR_S"))

ggplot(BR_S, aes(y=Ab_Sum, x=survdate, fill=string)) +
  ggtitle("Black Reef Slab")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.1,.87))+
  scale_x_discrete(limits=c("24 Jul", "11 Aug", "30 Sep", "13 Oct"))

SP<-droplevels(subset(Pick, Pick$Site=="SP"))

ggplot(SP, aes(y=Ab_Sum, x=survdate, fill=string)) +
  ggtitle("Seymour Point")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.1,.87))+
  scale_x_discrete(limits=c("29 Jul", "20 Aug", "09 Sep"))

TG<-droplevels(subset(Pick, Pick$Site=="TG"))

ggplot(TG, aes(y=Ab_Sum, x=survdate, fill=string)) +
  ggtitle("The Gardens")+
  xlab("Sample date") + 
  ylab("Blacklip Abalone Abundance") +
  geom_bar(stat="identity")+
  ylim(0,50)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.2), 
        text = element_text(size=16),
        legend.position=c(.1,.87))+
  scale_x_discrete(limits=c("29 Jul", "20 Aug", "09 Sep"))

