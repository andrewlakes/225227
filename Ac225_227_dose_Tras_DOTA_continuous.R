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
library(ggpubr)

columnnamestras = c("Days",	"Blood",	"Thymus",	"Heart",	"Lungs",	"Kidneys",	"Spleen",	"Liver",	"ART",	"Carcass",	"Tumor")

workbooktras = loadWorkbook("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx")

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
#margins
margins = c(0.25,0.5,0.1,0.25)



mAverage225errortras = cbind(mminus225tras,mplus225tras[3])
mAverage227errortras = cbind(mminus227tras,mplus227tras[3])
mOver225errortras = cbind(mminusover225tras,mplusover225tras[3])

plot225scale = c(1E-06, 1E-04, 1E-02, 1E0, 1E2)

plot225tras = ggplot()+ 
  geom_line(data=mAverage225tras, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mAverage225errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  ggtitle("DOTA-Trastuzumab")+
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10()+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plot225scale),max(plot225scale)), breaks=plot225scale)+#breaks=c(lseq(0.000001,100,9)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(y=element_blank(), x=element_blank(), color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        plot.title = element_text(hjust = 0.5, size=18))
  #+
  #guides(shape=guide_legend(override.aes = list(size=3)))

plot227scales = c(1E-08,1E-06,1E-4,1E-2,1E0,1E2)

plot227tras = ggplot()+ 
  geom_line(data=mAverage227tras, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mAverage227errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10()+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plot227scales),max(plot227scales)), breaks=plot227scales)+#breaks=c(lseq(0.000001,100,9)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(y=element_blank(), x=element_blank(), color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))#+
  #guides(shape=guide_legend(override.aes = list(size=3)))

plotover225scale=c(1E-05,1E-02,1E1,1E4)

plotover225tras = ggplot()+ 
  geom_line(data=mOver225tras, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mOver225errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10()+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plotover225scale),max(plotover225scale)), breaks=plotover225scale)+#breaks=c(lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="right", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(y=element_blank(), x = "Time (days)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
  #guides(shape=guide_legend(override.aes = list(size=3)))



#grid.arrange(arrangeGrob(plot225tras, plot227tras, ncol=2), arrangeGrob(plotover225tras, ncol=1))










####Start DOTA####

columnnamesDota = c("Days",	"Blood",	"Thymus",	"Heart",	"Lungs",	"Kidneys",	"Spleen",	"Liver",	"ART",	"Carcass")

workbookDota = loadWorkbook("2019_5_31_import_to_R_DOTA_225227.xlsx")

Days = read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                 sheetIndex = 1)

#Also add/subtract the average to each error


Average225Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                      sheetIndex = 2))

minus225Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                    sheetIndex = 3))

plus225Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                   sheetIndex = 4))

Average227Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                      sheetIndex = 5))

minus227Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                    sheetIndex = 6))

plus227Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                   sheetIndex = 7))

Over225Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                   sheetIndex = 8))

minusover225Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                        sheetIndex = 9))

plusover225Dota = cbind(Days,read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                       sheetIndex = 10))

colnames(Average225Dota) = columnnamesDota
colnames(Average227Dota) = columnnamesDota
colnames(Over225Dota) = columnnamesDota

colnames(minus225Dota) = columnnamesDota
colnames(plus225Dota) = columnnamesDota
colnames(minus227Dota) = columnnamesDota
colnames(plus227Dota) = columnnamesDota
colnames(minusover225Dota) = columnnamesDota
colnames(plusover225Dota) = columnnamesDota



#melt

mAverage225Dota = melt(Average225Dota, id="Days")
colnames(mAverage225Dota) = c("times", "Organs", "values")

mminus225Dota = melt(minus225Dota, id="Days")
colnames(mminus225Dota) = c("times", "Organs", "valuesminus")
mplus225Dota = melt(plus225Dota, id="Days")
colnames(mplus225Dota) = c("times", "Organs", "valuesplus")

mAverage227Dota = melt(Average227Dota, id="Days")
colnames(mAverage227Dota) = c("times", "Organs", "values")

mminus227Dota = melt(minus227Dota, id="Days")
colnames(mminus227Dota) = c("times", "Organs", "valuesminus")
mplus227Dota = melt(plus227Dota, id="Days")
colnames(mplus227Dota) = c("times", "Organs", "valuesplus")

mOver225Dota = melt(Over225Dota, id="Days")
colnames(mOver225Dota) = c("times", "Organs", "values")

mminusover225Dota = melt(minusover225Dota, id="Days")
colnames(mminusover225Dota) = c("times", "Organs", "valuesminus")
mplusover225Dota = melt(plusover225Dota, id="Days")
colnames(mplusover225Dota) = c("times", "Organs", "valuesplus")


#bind second data column to error sets

mAverage225errorDota = cbind(mminus225Dota,mplus225Dota[3])
mAverage227errorDota = cbind(mminus227Dota,mplus227Dota[3])
mOver225errorDota = cbind(mminusover225Dota,mplusover225Dota[3])

plot225scale = c(1E-06, 1E-04, 1E-02, 1E0, 1E2)

#200 nCi Ac-225-DOTA-Dota Dose (Gy)
plot225Dota = ggplot()+ 
  geom_line(data=mAverage225Dota, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mAverage225errorDota, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  ggtitle("DOTA")+
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10()+#breaks=c()0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plot225scale),max(plot225scale)),breaks=plot225scale)+#breaks=c(lseq(0.000001,100,9)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x=element_blank(), y = "Ac-225 Dose (Gy)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        plot.title = element_text(hjust = 0.5, size=18))
#+
#guides(shape=guide_legend(override.aes = list(size=3)))

plot227scales = c(1E-08,1E-06,1E-4,1E-2,1E0,1E2)

#20 nCi Ac-227-DOTA-Dota Dose (Gy)
plot227Dota = ggplot()+ 
  geom_line(data=mAverage227Dota, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mAverage227errorDota, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10()+#breaks=c()0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plot227scales),max(plot227scales)), breaks=plot227scales)+#breaks=c(lseq(0.000001,100,9)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x=element_blank(), y = "Ac-227 Dose (Gy)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))#+
#guides(shape=guide_legend(override.aes = list(size=3)))

plotover225scale=c(1E-05,1E-02,1E1,1E4)

#Ratio Dose (Ac-227/Ac-225)-DOTA-Dota
plotover225Dota = ggplot()+ 
  geom_line(data=mOver225Dota, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mOver225errorDota, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10()+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plotover225scale),max(plotover225scale)), breaks=plotover225scale)+#breaks=c(#lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "Ac-227/Ac-225", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))+
  guides(shape=guide_legend(override.aes = list(size=3)))



#grid.arrange(arrangeGrob(plot225Dota, plot227Dota, ncol=2), arrangeGrob(plotover225Dota, ncol=1))
#plot225tras, plot227tras, ncol=2), arrangeGrob(plotover225tras, ncol=1))

#grid.arrange(arrangeGrob(plot225Dota, plot225tras, plot227Dota, plot227tras, plotover225Dota, plotover225tras, ncol=2))
ggarrange(plot225Dota, plot225tras, plot227Dota, plot227tras, plotover225Dota, plotover225tras, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")

