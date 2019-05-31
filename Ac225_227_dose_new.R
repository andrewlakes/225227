library(reshape2)
library(plyr)
library(dplyr)
library(drc)
library(drfit)
library(ggplot2)
library(deSolve)
library(emdbook)
library(stats)
library(plotly)
library(cowplot)
library(gridExtra)
library(abind)
library(RColorBrewer)
library(tidyr)
library(GenKern)
library(xlsx)

columnnamestras = c("Days",	"Blood",	"Thymus",	"Heart",	"Lungs",	"Kidneys",	"Spleen",	"Liver",	"ART",	"Carcass",	"Tumor")

workbooktras = loadWorkbook("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx")
#sheetsnamesdotatras = names(getSheets(workbook))

Days = read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                  sheetIndex = 1)

#Also add/subtract the average to each error


Average225tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
                       sheetIndex = 2))

minus225tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
          sheetIndex = 3))

plus225tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
          sheetIndex = 4))

Average227tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
          sheetIndex = 5))

minus227tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
          sheetIndex = 6))

plus227tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
          sheetIndex = 7))

Over225tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
          sheetIndex = 8))

minusover225tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
          sheetIndex = 9))

plusover225tras = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
          sheetIndex = 10))

colnames(Average225tras) = columnnamestras
colnames(Average227tras) = columnnamestras
colnames(Over225tras) = columnnamestras

colnames(minus225tras) = columnnamestras
colnames(plus225tras) = columnnamestras
colnames(minus227tras) = columnnamestras
colnames(plus227tras) = columnnamestras
colnames(minusover225tras) = columnnamestras
colnames(plusover225tras) = columnnamestras



#melt

mAverage225tras = melt(Average225tras, id="Days")
colnames(mAverage225tras) = c("times", "Organs", "values")

mminus225tras = melt(minus225tras, id="Days")
colnames(mminus225tras) = c("times", "Organs", "valuesminus")
mplus225tras = melt(plus225tras, id="Days")
colnames(mplus225tras) = c("times", "Organs", "valuesplus")

mAverage227tras = melt(Average227tras, id="Days")
colnames(mAverage227tras) = c("times", "Organs", "values")

mminus227tras = melt(minus227tras, id="Days")
colnames(mminus227tras) = c("times", "Organs", "valuesminus")
mplus227tras = melt(plus227tras, id="Days")
colnames(mplus227tras) = c("times", "Organs", "valuesplus")

mOver225tras = melt(Over225tras, id="Days")
colnames(mOver225tras) = c("times", "Organs", "values")

mminusover225tras = melt(minusover225tras, id="Days")
colnames(mminusover225tras) = c("times", "Organs", "valuesminus")
mplusover225tras = melt(plusover225tras, id="Days")
colnames(mplusover225tras) = c("times", "Organs", "valuesplus")


#bind second data column to error sets

mAverage225errortras = cbind(mminus225tras,mplus225tras[3])
mAverage227errortras = cbind(mminus227tras,mplus227tras[3])
mOver225errortras = cbind(mminusover225tras,mplusover225tras[3])



plot225tras = ggplot()+ 
  geom_point(data=mAverage225tras, aes(x=times, y=values, color=Organs, shape=Organs), size=1, alpha=1, stroke = 1.25)+
  geom_ribbon(data=mAverage225errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.2)+
  
  
  scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "bl", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_log10(breaks=c(lseq(0.000001,100,9)))+
  theme_bw() +
  theme(legend.position="none",plot.margin = unit(c(0.75,0.75,0.75,0.75), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "200 nCi Ac-225-DOTA-Tras Dose (µGy)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))
  #+
  #guides(shape=guide_legend(override.aes = list(size=3)))



plot227tras = ggplot()+ 
  geom_point(data=mAverage227tras, aes(x=times, y=values, color=Organs, shape=Organs), size=1, alpha=1, stroke = 1.25)+
  geom_ribbon(data=mAverage227errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.2)+
  
  
  scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "bl", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_log10(breaks=c(lseq(0.000001,100,9)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(c(0.75,0.75,0.75,0.75), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "20 nCi Ac-227-DOTA-Tras Dose (µGy)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))#+
  #guides(shape=guide_legend(override.aes = list(size=3)))


plotover225tras = ggplot()+ 
  geom_line(data=mOver225tras, aes(x=times, y=values, color=Organs), size=2, alpha=1)+
  geom_ribbon(data=mOver225errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.2)+
  
  
  scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "bl", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_log10(breaks=c(lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="right", plot.margin = unit(c(0.75,0.75,0.75,0.75), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "Ratio Dose (Ac-227/Ac-225)-DOTA-Tras", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))+
  guides(shape=guide_legend(override.aes = list(size=3)))



grid.arrange(arrangeGrob(plot225tras, plot227tras, ncol=2), arrangeGrob(plotover225tras, ncol=1))

