# ******************************************************************************
#R code for producing charts for the Kraken project
#
#These charts are used to:
#
# 1. Compare different Lagerstätten environments
# 2. Compare differences in which morphological characters are preserved
#in soft tissues (in different genera)
# 3. Compare different time intervals and see why certain time intervals
#have more Lagerstätte
#
#Alissa Watson
#June 26 2024
#
################################################################################
#setwd
setwd("C:/Users/Alissa/Documents/School/GraduateSchool/Summer2024/ResearchProject/data")

#read in data
data <- read.csv("FossilizedBitsCharts.csv", sep = ",")

#Install packages
install.packages("ggplot2")
install.packages("extrafont")

#get packages from library
library(ggplot2)
library(extrafont)

#fonts
#font_import()
#loadfonts(device="win")       #Register fonts for Windows bitmap output
#fonts() 

##Extract data based on different localities 
#Create subset for each locality
uniqueData<- unique(data$LocalityName)

BearGulch <- data[data$LocalityName=="Bear Gulch", ]
MazonCreek <- data[data$LocalityName=="Mazon Creek", ]
Posidonia <- data[data$LocalityName=="Posidonia", ]
ChristianMalford <- data[data$LocalityName=="Christian Malford", ]
LaVoulte <- data[data$LocalityName=="La Voulte", ]
Solnhofen <- data[data$LocalityName=="Solnhofen", ]
Lebanon <- data[data$LocalityName=="Lebanon", ]
YaHaTinda <- data[data$LocalityName=="Ya Ha Tinda", ]
Osteno <- data[data$LocalityName=="Osteno", ]
LiasGroup <- data[data$LocalityName=="Lias Group", ]
Buckhorn <- data[data$LocalityName=="Buckhorn Asphalt", ]
StarkShale <- data[data$LocalityName=="Stark Shale", ]
SchistesCarton <- data[data$LocalityName=="Schistes Cartons", ]
Polzberg <- data[data$LocalityName=="Polzberg", ]
KimmeridgeClay <- data[data$LocalityName=="Kimmeridge Clay", ]

#Create average for all localities
BearGulch<-(sum(BearGulch$Sum))/length(BearGulch$Sum)
  BearGulch<-round(BearGulch, digits = 2)
MazonCreek<-(sum(MazonCreek$Sum))/length(MazonCreek$Sum)
  MazonCreek<-round(MazonCreek, digits = 2)
Posidonia<-(sum(Posidonia$Sum))/length(Posidonia$Sum)
  Posidonia<-round(Posidonia, digits = 2)
ChristianMalford<-(sum(ChristianMalford$Sum))/length(ChristianMalford$Sum)
  ChristianMalford<-round(ChristianMalford, digits = 2)
LaVoulte<-(sum(LaVoulte$Sum))/length(LaVoulte$Sum)
  LaVoulte<-round(LaVoulte, digits = 2)
Solnhofen<-(sum(Solnhofen$Sum))/length(Solnhofen$Sum)
  Solnhofen<-round(Solnhofen, digits = 2)
Lebanon<-(sum(Lebanon$Sum))/length(Lebanon$Sum)
  Lebanon<-round(Lebanon, digits = 2)
YaHaTinda<-(sum(YaHaTinda$Sum))/length(YaHaTinda$Sum)
  YaHaTinda<-round(YaHaTinda, digits = 2)
Osteno<-(sum(Osteno$Sum))/length(Osteno$Sum)
  Osteno<-round(Osteno, digits = 2)
LiasGroup<-(sum(LiasGroup$Sum))/length(LiasGroup$Sum)
  LiasGroup<-round(LiasGroup, digits = 2)
Buckhorn<-(sum(Buckhorn$Sum))/length(Buckhorn$Sum)
  Buckhorn<-round(Buckhorn, digits = 2)
StarkShale<-(sum(StarkShale$Sum))/length(StarkShale$Sum)
  StarkShale<-round(StarkShale, digits = 2)
SchistesCarton<-(sum(SchistesCarton$Sum))/length(SchistesCarton$Sum)
  SchistesCarton<-round(SchistesCarton, digits = 2)
Polzberg<-(sum(Polzberg$Sum))/length(Polzberg$Sum)
  Polzberg<-round(Polzberg, digits = 2)
KimmeridgeClay<-(sum(KimmeridgeClay$Sum))/length(KimmeridgeClay$Sum)
  KimmeridgeClay<-round(KimmeridgeClay, digits = 2)


#Compare all localities with a bar chart
AllLocalitiesAvg<- c(BearGulch, MazonCreek, Posidonia, ChristianMalford, LaVoulte,
                     Solnhofen, Lebanon, YaHaTinda, Osteno, LiasGroup, Buckhorn,
                     StarkShale, SchistesCarton, Polzberg, KimmeridgeClay)
AllLocalitiesName<- c("Bear Gulch", "Mazon Creek", "Posidonia", "Christian Malford",
                      "La Voulte", "Solnhofen", "Lebanon", "Ya Ha Tinda", "Osteno",
                      "Lias Group", "Buckhorn Asphalt", "Stark Shale", 
                      "Schistes Carton", "Polzberg", "Kimmeridge Clay")
#make a data frame
AllLocalities<- data.frame(AllLocalitiesName, AllLocalitiesAvg)
#Make the bar chart
# Basic barplot
#why isn't ylim working??
p<-ggplot(data=AllLocalities, aes(x=AllLocalitiesName, y=AllLocalitiesAvg), ylim(c(0,30)))+
  geom_bar(stat="identity", fill="#72bda3")+
  geom_text(aes(label=AllLocalitiesAvg), vjust=0.4, hjust="inward", color="white", size=3.5)+
  ggtitle("Characters not preserved in Lagerstätten")+ xlab("Lagerstätten Name")+
  ylab("Average Characters not preserved")+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))
  
# Horizontal bar plot
p + coord_flip()

# ******************************************************************************
#Compare all Plattenkalks localities
#Plattenkalks= Bear Gulch, Solnhofen, Lebanon, Osteno
PlattenkalkAvg<- c(BearGulch, Solnhofen, Lebanon, Osteno)
PlattenkalkName<- c("Bear Gulch", "Solnhofen", "Lebanon", "Osteno")
#Create color palette
PlattenkalkColors<-c("#A6A6C9", "#7272AC", "#434371", "#2D2D4D")
#Create a pie chart
pie(PlattenkalkAvg, labels = PlattenkalkAvg, col=PlattenkalkColors,
    border = PlattenkalkColors, main= "Plattenkalk Lagerstätten")
#make legend
legend("topleft", legend = PlattenkalkName, fill = PlattenkalkColors, cex=0.85)

# ******************************************************************************
#Compare all Concretion localities
#Concretion= Mazon Creek, La Voulte, Lias Group
ConcretionAvg<- c(MazonCreek, LaVoulte, LiasGroup)
ConcretionName<- c("Mazon Creek", "La Voulte", "Lias Group")
#Create color palette
ConcretionColors<-c("#e7ca74", "#deb841", "#bf9722")
#Create a pie chart
pie(ConcretionAvg, labels = ConcretionAvg, col=ConcretionColors,
    border = ConcretionColors, main= "Concretion Lagerstätten")
#make legend
legend("topleft", legend = ConcretionName, fill = ConcretionColors, cex=0.85)

# ******************************************************************************
#Compare all Mudstone localities
#Mudstone= Posidonia, Christian Malford, Ya Ha Tinda, Lias Group, Stark Shale
#Schistes Carton, Polzberg, Kimmeridge Clay
MudstoneAvg<- c(Posidonia, ChristianMalford, YaHaTinda, LiasGroup, StarkShale,
                SchistesCarton, Polzberg, KimmeridgeClay)
MudstoneName<- c("Posidonia", "Christian Malford", "Ya Ha Tinda", "Lias Group",
                 "Stark Shale", "Schistes Carton", "Polzberg", "Kimmeridge Clay ")
#Create color palette
MudstoneColors<- c("#EEC7BE", "#E6AB9E", "#E29B8D", "#D97F6D", "#CD533B", 
                   "#A23E2A", "#823221", "#511F15")
#Create a pie chart
pie(MudstoneAvg, labels = MudstoneAvg, col=MudstoneColors,
    border = MudstoneColors, main= "Mudstone Lagerstätten")
#make legend
legend("topleft", legend = MudstoneName, fill = MudstoneColors, cex=0.65)

# ******************************************************************************
#Compare all Bituminous Limestone localities
#Buckhorn = can't compare different localities, but still need to compare with other
#depositional environments
BLimestoneAvg<- Buckhorn
BLimestoneName<- c("Buckhorn Asphalt  ")
#Create color palette
BLimestoneColors<- c("#72bda3")
#Create a pie chart
pie(BLimestoneAvg, labels = BLimestoneAvg, col=BLimestoneColors,
    border = BLimestoneColors, main= "Bituminous Limestone Lagerstätte")
#make legend
legend("topleft", legend = BLimestoneName, fill = BLimestoneColors, cex=0.85)


# ******************************************************************************
#Compare all depositional environments
PlattenkalkAvg<- sum(PlattenkalkAvg)/length(PlattenkalkAvg)
  PlattenkalkAvg<- round(PlattenkalkAvg, digits = 2)
ConcretionAvg<- sum(ConcretionAvg)/length(ConcretionAvg)
  ConcretionAvg<- round(ConcretionAvg, digits = 2)
MudstoneAvg<- sum(MudstoneAvg)/length(MudstoneAvg)
  MudstoneAvg<- round(MudstoneAvg, digits = 2)
LimestoneAvg<-Buckhorn #only one value

DepEnvAvg<- c(PlattenkalkAvg, ConcretionAvg, MudstoneAvg, LimestoneAvg)

DepEnvName<- c("Plattenkalk", "Concretion", "Mudstone", "Bituminous Limestone  ")
#Create a color palette
DepEnvColors<-c("#434371", "#deb841", "#cd533b", "#72bda3")

#Create a pie chart
pie(DepEnvAvg, labels = DepEnvAvg, col=DepEnvColors,
    border = DepEnvColors, main= "Depositional Environments")
#make legend
legend("topleft", legend = DepEnvName, fill = DepEnvColors, cex=0.65)


###############################################################
##Extract data based on different morphological characteristics

#compare this with the depositional environments

#how likely is a character going to be preserved in a certain lagerstatte

#Extract rows per depositional environment
#Plattenkalk, Concretion, Mudstone, Limestone
#Plattenkalks= Bear Gulch, Solnhofen, Lebanon, Osteno
cPlattenkalk<- data[data$LocalityName=="Bear Gulch" | data$LocalityName=="Solnhofen" |
                      data$LocalityName=="Lebanon" | data$LocalityName=="Osteno", ] 

#Concretion= Mazon Creek, La Voulte, Lias Group
cConcretion<-data[data$LocalityName=="Mazon Creek" | data$LocalityName=="La Voulte" |
                    data$LocalityName=="Lias Group", ]

#Mudstone= Posidonia, Christian Malford, Ya Ha Tinda, Lias Group, Stark Shale
#Schistes Carton, Polzberg, Kimmeridge Clay
cMudstone<- data[data$LocalityName=="Posidonia" | data$LocalityName=="Christian Malford" |
                   data$LocalityName=="Ya Ha Tinda" | data$LocalityName=="Lias Group" |
                   data$LocalityName=="Stark Shale" | data$LocalityName=="Schistes Carton" |
                   data$LocalityName=="Polzberg" | data$LocalityName=="Kimmeridge Clay",]

#Bituminous Limestone = Buckhorn
cBLimestone <- data[data$LocalityName=="Buckhorn Asphalt", ]

#average characters from specific columns for each depositional environment

#how likely characters are to be preserved on a 0-1 scale
#0 being always preserved and 1 being never preserved

#MM
MMPlattenkalk<- round(sum(cPlattenkalk$MM)/length(cPlattenkalk$MM), digits = 2)
MMConcretion<- round(sum(cConcretion$MM)/length(cConcretion$MM), digits = 2)
MMMudstone<- round(sum(cMudstone$MM)/length(cMudstone$MM), digits = 2)
MMBLimestone<- round(sum(cBLimestone$MM)/length(cBLimestone$MM), digits = 2)

#HMF
HMFPlattenkalk<- round(sum(cPlattenkalk$HMF)/length(cPlattenkalk$HMF), digits = 2)
HMFConcretion<- round(sum(cConcretion$HMF)/length(cConcretion$HMF), digits = 2)
HMFMudstone<- round(sum(cMudstone$HMF)/length(cMudstone$HMF), digits = 2)
HMFBLimestone<- round(sum(cBLimestone$HMF)/length(cBLimestone$HMF), digits = 2)

#Arms
ArmsPlattenkalk<- round(sum(cPlattenkalk$Arms)/length(cPlattenkalk$Arms), digits = 2)
ArmsConcretion<- round(sum(cConcretion$Arms)/length(cConcretion$Arms), digits = 2)
ArmsMudstone<- round(sum(cMudstone$Arms)/length(cMudstone$Arms), digits = 2)
ArmsBLimestone<- round(sum(cBLimestone$Arms)/length(cBLimestone$Arms), digits = 2)

#Tentacles
TentaclesPlattenkalk<- round(sum(cPlattenkalk$Tentacles)/length(cPlattenkalk$Tentacles), digits = 2)
TentaclesConcretion<- round(sum(cConcretion$Tentacles)/length(cConcretion$Tentacles), digits = 2)
TentaclesMudstone<- round(sum(cMudstone$Tentacles)/length(cMudstone$Tentacles), digits = 2)
TentaclesBLimestone<- round(sum(cBLimestone$Tentacles)/length(cBLimestone$Tentacles), digits = 2)

#TP
TPPlattenkalk<- round(sum(cPlattenkalk$TP)/length(cPlattenkalk$TP), digits = 2)
TPConcretion<- round(sum(cConcretion$TP)/length(cConcretion$TP), digits = 2)
TPMudstone<- round(sum(cMudstone$TP)/length(cMudstone$TP), digits = 2)
TPBLimestone<- round(sum(cBLimestone$TP)/length(cBLimestone$TP), digits = 2)

#CircularSuckers
CircularSuckersPlattenkalk<- round(sum(cPlattenkalk$CircularSuckers)/length(cPlattenkalk$CircularSuckers), digits = 2)
CircularSuckersConcretion<- round(sum(cConcretion$CircularSuckers)/length(cConcretion$CircularSuckers), digits = 2)
CircularSuckersMudstone<- round(sum(cMudstone$CircularSuckers)/length(cMudstone$CircularSuckers), digits = 2)
CircularSuckersBLimestone<- round(sum(cBLimestone$CircularSuckers)/length(cBLimestone$CircularSuckers), digits = 2)

#Onychites
OnychitesPlattenkalk<- round(sum(cPlattenkalk$Onychites)/length(cPlattenkalk$Onychites), digits = 2)
OnychitesConcretion<- round(sum(cConcretion$Onychites)/length(cConcretion$Onychites), digits = 2)
OnychitesMudstone<- round(sum(cMudstone$Onychites)/length(cMudstone$Onychites), digits = 2)
OnychitesBLimestone<- round(sum(cBLimestone$Onychites)/length(cBLimestone$Onychites), digits = 2)

#CLA
CLAPlattenkalk<- round(sum(cPlattenkalk$CLA)/length(cPlattenkalk$CLA), digits = 2)
CLAConcretion<- round(sum(cConcretion$CLA)/length(cConcretion$CLA), digits = 2)
CLAMudstone<- round(sum(cMudstone$CLA)/length(cMudstone$CLA), digits = 2)
CLABLimestone<- round(sum(cBLimestone$CLA)/length(cBLimestone$CLA), digits = 2)

#SuckerRings
SuckerRingsPlattenkalk<- round(sum(cPlattenkalk$SuckerRings)/length(cPlattenkalk$SuckerRings), digits = 2)
SuckerRingsConcretion<- round(sum(cConcretion$SuckerRings)/length(cConcretion$SuckerRings), digits = 2)
SuckerRingsMudstone<- round(sum(cMudstone$SuckerRings)/length(cMudstone$SuckerRings), digits = 2)
SuckerRingsBLimestone<- round(sum(cBLimestone$SuckerRings)/length(cBLimestone$SuckerRings), digits = 2)

#ArmWeb
ArmWebPlattenkalk<- round(sum(cPlattenkalk$ArmWeb)/length(cPlattenkalk$ArmWeb), digits = 2)
ArmWebConcretion<- round(sum(cConcretion$ArmWeb)/length(cConcretion$ArmWeb), digits = 2)
ArmWebMudstone<- round(sum(cMudstone$ArmWeb)/length(cMudstone$ArmWeb), digits = 2)
ArmWebBLimestone<- round(sum(cBLimestone$ArmWeb)/length(cBLimestone$ArmWeb), digits = 2)

#Funnel
FunnelPlattenkalk<- round(sum(cPlattenkalk$Funnel)/length(cPlattenkalk$Funnel), digits = 2)
FunnelConcretion<- round(sum(cConcretion$Funnel)/length(cConcretion$Funnel), digits = 2)
FunnelMudstone<- round(sum(cMudstone$Funnel)/length(cMudstone$Funnel), digits = 2)
FunnelBLimestone<- round(sum(cBLimestone$Funnel)/length(cBLimestone$Funnel), digits = 2)

#Fins
FinsPlattenkalk<- round(sum(cPlattenkalk$Fins)/length(cPlattenkalk$Fins), digits = 2)
FinsConcretion<- round(sum(cConcretion$Fins)/length(cConcretion$Fins), digits = 2)
FinsMudstone<- round(sum(cMudstone$Fins)/length(cMudstone$Fins), digits = 2)
FinsBLimestone<- round(sum(cBLimestone$Fins)/length(cBLimestone$Fins), digits = 2)

#CC
CCPlattenkalk<- round(sum(cPlattenkalk$CC)/length(cPlattenkalk$CC), digits = 2)
CCConcretion<- round(sum(cConcretion$CC)/length(cConcretion$CC), digits = 2)
CCMudstone<- round(sum(cMudstone$CC)/length(cMudstone$CC), digits = 2)
CCBLimestone<- round(sum(cBLimestone$CC)/length(cBLimestone$CC), digits = 2)

#FinCartilage
FinCartilagePlattenkalk<- round(sum(cPlattenkalk$FinCartilage)/length(cPlattenkalk$FinCartilage), digits = 2)
FinCartilageConcretion<- round(sum(cConcretion$FinCartilage)/length(cConcretion$FinCartilage), digits = 2)
FinCartilageMudstone<- round(sum(cMudstone$FinCartilage)/length(cMudstone$FinCartilage), digits = 2)
FinCartilageBLimestone<- round(sum(cBLimestone$FinCartilage)/length(cBLimestone$FinCartilage), digits = 2)

#NFC
NFCPlattenkalk<- round(sum(cPlattenkalk$NFC)/length(cPlattenkalk$NFC), digits = 2)
NFCConcretion<- round(sum(cConcretion$NFC)/length(cConcretion$NFC), digits = 2)
NFCMudstone<- round(sum(cMudstone$NFC)/length(cMudstone$NFC), digits = 2)
NFCBLimestone<- round(sum(cBLimestone$NFC)/length(cBLimestone$NFC), digits = 2)

#BuccalMass
BuccalMassPlattenkalk<- round(sum(cPlattenkalk$BuccalMass)/length(cPlattenkalk$BuccalMass), digits = 2)
BuccalMassConcretion<- round(sum(cConcretion$BuccalMass)/length(cConcretion$BuccalMass), digits = 2)
BuccalMassMudstone<- round(sum(cMudstone$BuccalMass)/length(cMudstone$BuccalMass), digits = 2)
BuccalMassBLimestone<- round(sum(cBLimestone$BuccalMass)/length(cBLimestone$BuccalMass), digits = 2)

#Beaks
BeaksPlattenkalk<- round(sum(cPlattenkalk$Beaks)/length(cPlattenkalk$Beaks), digits = 2)
BeaksConcretion<- round(sum(cConcretion$Beaks)/length(cConcretion$Beaks), digits = 2)
BeaksMudstone<- round(sum(cMudstone$Beaks)/length(cMudstone$Beaks), digits = 2)
BeaksBLimestone<- round(sum(cBLimestone$Beaks)/length(cBLimestone$Beaks), digits = 2)

#Esophagus
EsophagusPlattenkalk<- round(sum(cPlattenkalk$Esophagus)/length(cPlattenkalk$Esophagus), digits = 2)
EsophagusConcretion<- round(sum(cConcretion$Esophagus)/length(cConcretion$Esophagus), digits = 2)
EsophagusMudstone<- round(sum(cMudstone$Esophagus)/length(cMudstone$Esophagus), digits = 2)
EsophagusBLimestone<- round(sum(cBLimestone$Esophagus)/length(cBLimestone$Esophagus), digits = 2)

#Stomach
StomachPlattenkalk<- round(sum(cPlattenkalk$Stomach)/length(cPlattenkalk$Stomach), digits = 2)
StomachConcretion<- round(sum(cConcretion$Stomach)/length(cConcretion$Stomach), digits = 2)
StomachMudstone<- round(sum(cMudstone$Stomach)/length(cMudstone$Stomach), digits = 2)
StomachBLimestone<- round(sum(cBLimestone$Stomach)/length(cBLimestone$Stomach), digits = 2)

#Crop
CropPlattenkalk<- round(sum(cPlattenkalk$Crop)/length(cPlattenkalk$Crop), digits = 2)
CropConcretion<- round(sum(cConcretion$Crop)/length(cConcretion$Crop), digits = 2)
CropMudstone<- round(sum(cMudstone$Crop)/length(cMudstone$Crop), digits = 2)
CropBLimestone<- round(sum(cBLimestone$Crop)/length(cBLimestone$Crop), digits = 2)

#RS
RSPlattenkalk<- round(sum(cPlattenkalk$RS)/length(cPlattenkalk$RS), digits = 2)
RSConcretion<- round(sum(cConcretion$RS)/length(cConcretion$RS), digits = 2)
RSMudstone<- round(sum(cMudstone$RS)/length(cMudstone$RS), digits = 2)
RSBLimestone<- round(sum(cBLimestone$RS)/length(cBLimestone$RS), digits = 2)

#CS
CSPlattenkalk<- round(sum(cPlattenkalk$CS)/length(cPlattenkalk$CS), digits = 2)
CSConcretion<- round(sum(cConcretion$CS)/length(cConcretion$CS), digits = 2)
CSMudstone<- round(sum(cMudstone$CS)/length(cMudstone$CS), digits = 2)
CSBLimestone<- round(sum(cBLimestone$CS)/length(cBLimestone$CS), digits = 2)

#ES #only maybe found in one genera in lebanon (plattenkalk)
ESPlattenkalk<- round(sum(cPlattenkalk$ES)/length(cPlattenkalk$ES), digits = 2)
ESConcretion<- round(sum(cConcretion$ES)/length(cConcretion$ES), digits = 2)
ESMudstone<- round(sum(cMudstone$ES)/length(cMudstone$ES), digits = 2)
ESBLimestone<- round(sum(cBLimestone$ES)/length(cBLimestone$ES), digits = 2)

#RepS
RepSPlattenkalk<- round(sum(cPlattenkalk$RepS)/length(cPlattenkalk$RepS), digits = 2)
RepSConcretion<- round(sum(cConcretion$RepS)/length(cConcretion$RepS), digits = 2)
RepSMudstone<- round(sum(cMudstone$RepS)/length(cMudstone$RepS), digits = 2)
RepSBLimestone<- round(sum(cBLimestone$RepS)/length(cBLimestone$RepS), digits = 2)

#InkSac
InkSacPlattenkalk<- round(sum(cPlattenkalk$InkSac)/length(cPlattenkalk$InkSac), digits = 2)
InkSacConcretion<- round(sum(cConcretion$InkSac)/length(cConcretion$InkSac), digits = 2)
InkSacMudstone<- round(sum(cMudstone$InkSac)/length(cMudstone$InkSac), digits = 2)
InkSacBLimestone<- round(sum(cBLimestone$InkSac)/length(cBLimestone$InkSac), digits = 2)

#LuminousOrgans
LuminousOrgansPlattenkalk<- round(sum(cPlattenkalk$LuminousOrgans)/length(cPlattenkalk$LuminousOrgans), digits = 2)
LuminousOrgansConcretion<- round(sum(cConcretion$LuminousOrgans)/length(cConcretion$LuminousOrgans), digits = 2)
LuminousOrgansMudstone<- round(sum(cMudstone$LuminousOrgans)/length(cMudstone$LuminousOrgans), digits = 2)
LuminousOrgansBLimestone<- round(sum(cBLimestone$LuminousOrgans)/length(cBLimestone$LuminousOrgans), digits = 2)

#FunnelRetractor
FunnelRetractorPlattenkalk<- round(sum(cPlattenkalk$FunnelRetractor)/length(cPlattenkalk$FunnelRetractor), digits = 2)
FunnelRetractorConcretion<- round(sum(cConcretion$FunnelRetractor)/length(cConcretion$FunnelRetractor), digits = 2)
FunnelRetractorMudstone<- round(sum(cMudstone$FunnelRetractor)/length(cMudstone$FunnelRetractor), digits = 2)
FunnelRetractorBLimestone<- round(sum(cBLimestone$FunnelRetractor)/length(cBLimestone$FunnelRetractor), digits = 2)

#AxialNerve
AxialNervePlattenkalk<- round(sum(cPlattenkalk$AxialNerve)/length(cPlattenkalk$AxialNerve), digits = 2)
AxialNerveConcretion<- round(sum(cConcretion$AxialNerve)/length(cConcretion$AxialNerve), digits = 2)
AxialNerveMudstone<- round(sum(cMudstone$AxialNerve)/length(cMudstone$AxialNerve), digits = 2)
AxialNerveBLimestone<- round(sum(cBLimestone$AxialNerve)/length(cBLimestone$AxialNerve), digits = 2)

#Statolith
StatolithPlattenkalk<- round(sum(cPlattenkalk$Statolith)/length(cPlattenkalk$Statolith), digits = 2)
StatolithConcretion<- round(sum(cConcretion$Statolith)/length(cConcretion$Statolith), digits = 2)
StatolithMudstone<- round(sum(cMudstone$Statolith)/length(cMudstone$Statolith), digits = 2)
StatolithBLimestone<- round(sum(cBLimestone$Statolith)/length(cBLimestone$Statolith), digits = 2)

# ******************************************************************************
#make bar charts of all characters in each depositional environment
#Plattenkalk
AllCPlattenkalk<- c(MMPlattenkalk, HMFPlattenkalk, ArmsPlattenkalk, TentaclesPlattenkalk,
                    TPPlattenkalk, CircularSuckersPlattenkalk, OnychitesPlattenkalk, CLAPlattenkalk,
                    SuckerRingsPlattenkalk, ArmWebPlattenkalk, FunnelPlattenkalk, FinsPlattenkalk,
                    CCPlattenkalk, FinCartilagePlattenkalk, NFCPlattenkalk, BuccalMassPlattenkalk,
                    BeaksPlattenkalk, EsophagusPlattenkalk, StomachPlattenkalk, CropPlattenkalk,
                    RSPlattenkalk, ESPlattenkalk, RepSPlattenkalk, InkSacPlattenkalk, LuminousOrgansPlattenkalk,
                    FunnelRetractorPlattenkalk, AxialNervePlattenkalk, StatolithPlattenkalk)
AllCNames<- c("MM", "HMF", "Arms", "Tentacles", "TP", "Circular Suckers", "Onychites",
              "CLA", "Sucker Rings", "Arm Web", "Funnel", "Fins", "CC", "Fin Cartilage",
              "NFC", "Buccal Mass", "Beaks", "Esophagus", "Stomach", "Crop", "RS", "ES", "RepS",
              "Ink Sac", "Luminous Organs", "Funnel Retractor", "Axial Nerve", "Statolith")
#make a data frame
AllPlattenkalk<- data.frame(AllCNames, AllCPlattenkalk)

barPlattenkalk<-ggplot(data=AllPlattenkalk, aes(x=AllCNames, y=AllCPlattenkalk))+
  geom_bar(stat="identity", fill="#7272ac")+
  geom_text(aes(label=AllCPlattenkalk), vjust=0.4, hjust=1, color="white", size=3.5)+
  xlab("Character Name")+
  ylab("Percentage of Characters not preserved")+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))
# Horizontal bar plot
barPlattenkalk + coord_flip() + labs(title="Plattenkalk Characters")

# *******************************************************************************
#Concretion
AllCConcretion<- c(MMConcretion, HMFConcretion, ArmsConcretion, TentaclesConcretion,
                    TPConcretion, CircularSuckersConcretion, OnychitesConcretion, CLAConcretion,
                    SuckerRingsConcretion, ArmWebConcretion, FunnelConcretion, FinsConcretion,
                    CCConcretion, FinCartilageConcretion, NFCConcretion, BuccalMassConcretion,
                    BeaksConcretion, EsophagusConcretion, StomachConcretion, CropConcretion,
                    RSConcretion, ESConcretion, RepSConcretion, InkSacConcretion, LuminousOrgansConcretion,
                    FunnelRetractorConcretion, AxialNerveConcretion, StatolithConcretion)
#make a data frame
AllConcretion<- data.frame(AllCNames, AllCConcretion)

barConcretion<-ggplot(data=AllConcretion, aes(x=AllCNames, y=AllCConcretion))+
  geom_bar(stat="identity", fill="#deb841")+
  geom_text(aes(label=AllCConcretion), vjust=0.4, hjust=1, color="white", size=3.5)+
  xlab("Character Name")+
  ylab("Percentage of Characters not preserved")+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))
# Horizontal bar plot
barConcretion + coord_flip() + labs(title="Concretion Characters")

# ******************************************************************************
#Mudstone
AllCMudstone<- c(MMMudstone, HMFMudstone, ArmsMudstone, TentaclesMudstone,
                   TPMudstone, CircularSuckersMudstone, OnychitesMudstone, CLAMudstone,
                   SuckerRingsMudstone, ArmWebMudstone, FunnelMudstone, FinsMudstone,
                   CCMudstone, FinCartilageMudstone, NFCMudstone, BuccalMassMudstone,
                   BeaksMudstone, EsophagusMudstone, StomachMudstone, CropMudstone,
                   RSMudstone, ESMudstone, RepSMudstone, InkSacMudstone, LuminousOrgansMudstone,
                   FunnelRetractorMudstone, AxialNerveMudstone, StatolithMudstone)
#make a data frame
AllMudstone<- data.frame(AllCNames, AllCMudstone)

barMudstone<-ggplot(data=AllMudstone, aes(x=AllCNames, y=AllCMudstone))+
  geom_bar(stat="identity", fill="#cd533b")+
  geom_text(aes(label=AllCMudstone), vjust=0.4, hjust=1, color="white", size=3.5)+
  xlab("Character Name")+
  ylab("Percentage of Characters not preserved")+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))
# Horizontal bar plot
barMudstone + coord_flip() + labs(title="Mudstone Characters")

# ******************************************************************************
#BLimestone
AllCBLimestone<- c(MMBLimestone, HMFBLimestone, ArmsBLimestone, TentaclesBLimestone,
                 TPBLimestone, CircularSuckersBLimestone, OnychitesBLimestone, CLABLimestone,
                 SuckerRingsBLimestone, ArmWebBLimestone, FunnelBLimestone, FinsBLimestone,
                 CCBLimestone, FinCartilageBLimestone, NFCBLimestone, BuccalMassBLimestone,
                 BeaksBLimestone, EsophagusBLimestone, StomachBLimestone, CropBLimestone,
                 RSBLimestone, ESBLimestone, RepSBLimestone, InkSacBLimestone, LuminousOrgansBLimestone,
                 FunnelRetractorBLimestone, AxialNerveBLimestone, StatolithBLimestone)
#make a data frame
AllBLimestone<- data.frame(AllCNames, AllCBLimestone)

barBLimestone<-ggplot(data=AllBLimestone, aes(x=AllCNames, y=AllCBLimestone))+
  geom_bar(stat="identity", fill="#72bda3")+
  geom_text(aes(label=AllCBLimestone), vjust=0.4, hjust=1, color="white", size=3.5)+
  xlab("Character Name")+
  ylab("Percentage of Characters not preserved")+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))
# Horizontal bar plot
barBLimestone + coord_flip() + labs(title="Bituminous Limestone Characters")

# ******************************************************************************
#Make pie charts comparing different characters
#MM
MMAllLocalities<- c(MMPlattenkalk, MMConcretion, MMMudstone, MMBLimestone)

#Create a color palette
AllEnvColors<-c("#434371", "#deb841", "#cd533b", "#72bda3")
#Name of localities
LocalitiesName<- c("Plattenkalk ", "Concretion ", "Mudstone ", "Bituminous Limestone   ")

#Create a pie chart
pie(MMAllLocalities, labels = MMAllLocalities, col=AllEnvColors,
    border = AllEnvColors, main= "Percentage of Muscular Mantle not preserved")
#make legend
legend("topleft", legend = LocalitiesName, fill = AllEnvColors, cex=0.65)

# ******************************************************************************
#Ink Sac
InkSacAllLocalities<- c(InkSacPlattenkalk, InkSacConcretion, InkSacMudstone, InkSacBLimestone)

#Create a color palette
AllEnvColors<-c("#434371", "#deb841", "#cd533b", "#72bda3")
#Name of localities
LocalitiesName<- c("Plattenkalk ", "Concretion ", "Mudstone ", "Bituminous Limestone   ")

#Create a pie chart
pie(InkSacAllLocalities, labels = InkSacAllLocalities, col=AllEnvColors,
    border = AllEnvColors, main= "Percentage of Ink Sac not preserved")
#make legend
legend("topleft", legend = LocalitiesName, fill = AllEnvColors, cex=0.75)

# ******************************************************************************
#Arms
ArmsAllLocalities<- c(ArmsPlattenkalk, ArmsConcretion, ArmsMudstone, ArmsBLimestone)

#Create a color palette
AllEnvColors<-c("#434371", "#deb841", "#cd533b", "#72bda3")
#Name of localities
LocalitiesName<- c("Plattenkalk ", "Concretion ", "Mudstone ", "Bituminous Limestone   ")

#Create a pie chart
pie(ArmsAllLocalities, labels = ArmsAllLocalities, col=AllEnvColors,
    border = AllEnvColors, main= "Percentage of Arms not preserved")
#make legend
legend("topleft", legend = LocalitiesName, fill = AllEnvColors, cex=0.75)

# ******************************************************************************
#CC
CCAllLocalities<- c(CCPlattenkalk, CCConcretion, CCMudstone, CCBLimestone)

#Create a color palette
AllEnvColors<-c("#434371", "#deb841", "#cd533b", "#72bda3")
#Name of localities
LocalitiesName<- c("Plattenkalk ", "Concretion ", "Mudstone ", "Bituminous Limestone   ")

#Create a pie chart
pie(CCAllLocalities, labels = CCAllLocalities, col=AllEnvColors,
    border = AllEnvColors, main= "Percentage of Cephalic Cartilage not preserved")
#make legend
legend("topleft", legend = LocalitiesName, fill = AllEnvColors, cex=0.75)

# ******************************************************************************
