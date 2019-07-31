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
library(grid)

unifiedScale1 = c(1E-08, 1E-04, 1E-0, 1E4)
unifiedScale2 = c(1E-11, 1E-07, 1E-03, 1E1)

#subtract positive error, and add negative error! since cubic spline interp


columnnamestras = c("Days",	"Blood",	"Thymus",	"Heart",	"Lungs",	"Kidneys",	"Spleen",	"Liver",	"ART",	"Carcass",	"Tumor")

workbooktras = loadWorkbook("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx")

Days = read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                  sheetIndex = 1)

#error imported as ave +/- stdev


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

#convert per day from centiGray in excel
day225tras = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
                                   sheetIndex = 11))

minusday225tras = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
                                        sheetIndex = 12))

plusday225tras = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
                                       sheetIndex = 13))

day227tras = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
                                  sheetIndex = 14))

minusday227tras = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
                                       sheetIndex = 15))

plusday227tras = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_Tras_225227.xlsx",
                                      sheetIndex = 16))




colnames(Average225tras) = columnnamestras
colnames(Average227tras) = columnnamestras
colnames(Over225tras) = columnnamestras
colnames(day225tras) = columnnamestras
colnames(day227tras) = columnnamestras

colnames(minus225tras) = columnnamestras
colnames(plus225tras) = columnnamestras
colnames(minus227tras) = columnnamestras
colnames(plus227tras) = columnnamestras
colnames(minusover225tras) = columnnamestras
colnames(plusover225tras) = columnnamestras
colnames(minusday225tras) = columnnamestras
colnames(plusday225tras) = columnnamestras
colnames(minusday227tras) = columnnamestras
colnames(plusday227tras) = columnnamestras


#remove negative error values for logarithmic plotting
minus225tras[minus225tras < 1E-08] = 1E-08
plus225tras[plus225tras < 1E-08] = 1E-08
minus227tras[minus227tras < 1E-08] = 1E-08
plus227tras[plus227tras < 1E-08] = 1E-08
minusover225tras[minusover225tras < 1E-08] = 1E-08
plusover225tras[plusover225tras < 1E-08] = 1E-08
minusday225tras[minusday225tras < 1E-11] = 1E-11
plusday225tras[plusday225tras < 1E-11] = 1E-11
minusday227tras[minusday227tras < 1E-11] = 1E-11
plusday227tras[plusday227tras < 1E-11] = 1E-11


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


mday225tras = melt(day225tras, id="Days")
colnames(mday225tras) = c("times", "Organs", "values")

mminusday225tras = melt(minusday225tras, id="Days")
colnames(mminusday225tras) = c("times", "Organs", "valuesminus")
mplusday225tras = melt(plusday225tras, id="Days")
colnames(mplusday225tras) = c("times", "Organs", "valuesplus")

mday227tras = melt(day227tras, id="Days")
colnames(mday227tras) = c("times", "Organs", "values")

mminusday227tras = melt(minusday227tras, id="Days")
colnames(mminusday227tras) = c("times", "Organs", "valuesminus")
mplusday227tras = melt(plusday227tras, id="Days")
colnames(mplusday227tras) = c("times", "Organs", "valuesplus")


#bind second data column to error sets
#margins
margins = c(0.25,0.5,0.1,0.25)



mAverage225errortras = cbind(mminus225tras,mplus225tras[3])
mAverage227errortras = cbind(mminus227tras,mplus227tras[3])
mOver225errortras = cbind(mminusover225tras,mplusover225tras[3])
mday225errortras = cbind(mminusday225tras,mplusday225tras[3])
mday227errortras = cbind(mminusday227tras,mplusday227tras[3])

plot225scale = unifiedScale1

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

#plot227scales = c(1E-08,1E-08,1E-4,1E-2,1E0,1E2)
plot227scales = unifiedScale1

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

plotover225scale = unifiedScale1

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
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(y=element_blank(), x = "Time (days)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
  #guides(shape=guide_legend(override.aes = list(size=3)))



#grid.arrange(arrangeGrob(plot225tras, plot227tras, ncol=2), arrangeGrob(plotover225tras, ncol=1))


plotday225scale = unifiedScale2

plotday225tras = ggplot()+ 
  geom_line(data=mday225tras, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mday225errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  ggtitle("DOTA-Trastuzumab")+
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(1,100,10000))+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plotday225scale),max(plotday225scale)), breaks=plotday225scale)+#breaks=c(lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(y=element_blank(), x = element_blank(), color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        plot.title = element_text(hjust = 0.5, size=18))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
#guides(shape=guide_legend(override.aes = list(size=3)))

plotday227scale = unifiedScale2

plotday227tras = ggplot()+ 
  geom_line(data=mday227tras, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mday227errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(1,100,10000))+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plotday227scale),max(plotday227scale)), breaks=plotday227scale)+#breaks=c(lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(y=element_blank(), x = "Time (days)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))





####Start DOTA####

columnnamesDota = c("Days",	"Blood",	"Thymus",	"Heart",	"Lungs",	"Kidneys",	"Spleen",	"Liver",	"ART",	"Carcass")

workbookDota = loadWorkbook("2019_5_31_import_to_R_DOTA_225227.xlsx")

Days = read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                 sheetIndex = 1)




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

#convert per day from centiGray in excel
day225Dota = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                   sheetIndex = 11))

minusday225Dota = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                        sheetIndex = 12))

plusday225Dota = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                       sheetIndex = 13))

day227Dota = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                   sheetIndex = 14))

minusday227Dota = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                        sheetIndex = 15))

plusday227Dota = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_DOTA_225227.xlsx",
                                       sheetIndex = 16))

colnames(Average225Dota) = columnnamesDota
colnames(Average227Dota) = columnnamesDota
colnames(Over225Dota) = columnnamesDota
colnames(day225Dota) = columnnamesDota
colnames(day227Dota) = columnnamesDota

colnames(minus225Dota) = columnnamesDota
colnames(plus225Dota) = columnnamesDota
colnames(minus227Dota) = columnnamesDota
colnames(plus227Dota) = columnnamesDota
colnames(minusover225Dota) = columnnamesDota
colnames(plusover225Dota) = columnnamesDota
colnames(minusday225Dota) = columnnamesDota
colnames(plusday225Dota) = columnnamesDota
colnames(minusday227Dota) = columnnamesDota
colnames(plusday227Dota) = columnnamesDota



#remove negative error values for logarithmic plotting
minus225Dota[minus225Dota < 1E-08] = 1E-08
plus225Dota[plus225Dota < 1E-08] = 1E-08
minus227Dota[minus227Dota < 1E-08] = 1E-08
plus227Dota[plus227Dota < 1E-08] = 1E-08
minusover225Dota[minusover225Dota < 1E-08] = 1E-08
plusover225Dota[plusover225Dota < 1E-08] = 1E-08
minusday225Dota[minusday225Dota < 1E-11] = 1E-11
plusday225Dota[plusday225Dota < 1E-11] = 1E-11
minusday227Dota[minusday227Dota < 1E-11] = 1E-11
plusday227Dota[plusday227Dota < 1E-11] = 1E-11





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

mday225Dota = melt(day225Dota, id="Days")
colnames(mday225Dota) = c("times", "Organs", "values")

mminusday225Dota = melt(minusday225Dota, id="Days")
colnames(mminusday225Dota) = c("times", "Organs", "valuesminus")
mplusday225Dota = melt(plusday225Dota, id="Days")
colnames(mplusday225Dota) = c("times", "Organs", "valuesplus")

mday227Dota = melt(day227Dota, id="Days")
colnames(mday227Dota) = c("times", "Organs", "values")

mminusday227Dota = melt(minusday227Dota, id="Days")
colnames(mminusday227Dota) = c("times", "Organs", "valuesminus")
mplusday227Dota = melt(plusday227Dota, id="Days")
colnames(mplusday227Dota) = c("times", "Organs", "valuesplus")

#bind second data column to error sets

mAverage225errorDota = cbind(mminus225Dota,mplus225Dota[3])
mAverage227errorDota = cbind(mminus227Dota,mplus227Dota[3])
mOver225errorDota = cbind(mminusover225Dota,mplusover225Dota[3])
mday225errorDota = cbind(mminusday225Dota,mplusday225Dota[3])
mday227errorDota = cbind(mminusday227Dota,mplusday227Dota[3])

#plot225scale = c(1E-08, 1E-04, 1E-02, 1E0, 1E2)

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

#plot227scales = c(1E-08,1E-08,1E-4,1E-2,1E0,1E2)

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

#plotover225scale=c(1E-08,1E-02,1E1,1E4)

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



#plotday225scale=c(1E-09,1E-07,1E-5,1E-3,1E-1,1E1)

#Ratio Dose (Ac-227/Ac-225)-DOTA-Dota
plotday225Dota = ggplot()+ 
  geom_line(data=mday225Dota, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mday225errorDota, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  ggtitle("DOTA")+
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(1,100,10000))+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plotday225scale),max(plotday225scale)), breaks=plotday225scale)+#breaks=c(#lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = element_blank(), y = "Ac-225 (Gy/day)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        plot.title = element_text(hjust = 0.5, size=18))+
  guides(shape=guide_legend(override.aes = list(size=3)))



#plotday227scale=c(1E-09,1E-07,1E-5,1E-3,1E-1,1E1)

#Ratio Dose (Ac-227/Ac-227)-DOTA-Dota
plotday227Dota = ggplot()+ 
  geom_line(data=mday227Dota, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mday227errorDota, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(1,100,10000))+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plotday227scale),max(plotday227scale)), breaks=plotday227scale)+#breaks=c(#lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "Ac-227 (Gy/day)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))+
  guides(shape=guide_legend(override.aes = list(size=3)))

#grid.arrange(arrangeGrob(plot225Dota, plot227Dota, ncol=2), arrangeGrob(plotover225Dota, ncol=1))
#plot225tras, plot227tras, ncol=2), arrangeGrob(plotover225tras, ncol=1))

#grid.arrange(arrangeGrob(plot225Dota, plot225tras, plot227Dota, plot227tras, plotover225Dota, plotover225tras, ncol=2))
#grid.arrange(arrangeGrob(plot225Dota, plot225tras, ncol=2), arrangeGrob(plot227Dota, plot227tras+theme(legend.position="bottom"), ncol=1))




####Start HOPO####


columnnamesHOPO = c("Days",	"Blood",	"Thymus",	"Heart",	"Lungs",	"Kidneys",	"Spleen",	"Liver",	"ART",	"Carcass")

workbookHOPO = loadWorkbook("2019_5_31_import_to_R_HOPO_225227.xlsx")

Days = read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                 sheetIndex = 1)




Average225HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                      sheetIndex = 2))

minus225HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                    sheetIndex = 3))

plus225HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                   sheetIndex = 4))

Average227HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                      sheetIndex = 5))

minus227HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                    sheetIndex = 6))

plus227HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                   sheetIndex = 7))

Over225HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                   sheetIndex = 8))

minusover225HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                        sheetIndex = 9))

plusover225HOPO = cbind(Days,read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                       sheetIndex = 10))

#convert per day from centiGray in excel
day225HOPO = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                       sheetIndex = 11))

minusday225HOPO = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                            sheetIndex = 12))

plusday225HOPO = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                           sheetIndex = 13))

day227HOPO = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                       sheetIndex = 14))

minusday227HOPO = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                            sheetIndex = 15))

plusday227HOPO = cbind(Days,0.01*read.xlsx("2019_5_31_import_to_R_HOPO_225227.xlsx",
                                           sheetIndex = 16))

colnames(Average225HOPO) = columnnamesHOPO
colnames(Average227HOPO) = columnnamesHOPO
colnames(Over225HOPO) = columnnamesHOPO
colnames(day225HOPO) = columnnamesHOPO
colnames(day227HOPO) = columnnamesHOPO

colnames(minus225HOPO) = columnnamesHOPO
colnames(plus225HOPO) = columnnamesHOPO
colnames(minus227HOPO) = columnnamesHOPO
colnames(plus227HOPO) = columnnamesHOPO
colnames(minusover225HOPO) = columnnamesHOPO
colnames(plusover225HOPO) = columnnamesHOPO
colnames(minusday225HOPO) = columnnamesHOPO
colnames(plusday225HOPO) = columnnamesHOPO
colnames(minusday227HOPO) = columnnamesHOPO
colnames(plusday227HOPO) = columnnamesHOPO


#remove negative error values for logarithmic plotting
minus225HOPO[minus225HOPO < 1E-08] = 1E-08
plus225HOPO[plus225HOPO < 1E-08] = 1E-08
minus227HOPO[minus227HOPO < 1E-08] = 1E-08
plus227HOPO[plus227HOPO < 1E-08] = 1E-08
minusover225HOPO[minusover225HOPO < 1E-08] = 1E-08
plusover225HOPO[plusover225HOPO < 1E-08] = 1E-08
minusday225HOPO[minusday225HOPO < 1E-11] = 1E-11
plusday225HOPO[plusday225HOPO < 1E-11] = 1E-11
minusday227HOPO[minusday227HOPO < 1E-11] = 1E-11
plusday227HOPO[plusday227HOPO < 1E-11] = 1E-11




#melt

mAverage225HOPO = melt(Average225HOPO, id="Days")
colnames(mAverage225HOPO) = c("times", "Organs", "values")

mminus225HOPO = melt(minus225HOPO, id="Days")
colnames(mminus225HOPO) = c("times", "Organs", "valuesminus")
mplus225HOPO = melt(plus225HOPO, id="Days")
colnames(mplus225HOPO) = c("times", "Organs", "valuesplus")

mAverage227HOPO = melt(Average227HOPO, id="Days")
colnames(mAverage227HOPO) = c("times", "Organs", "values")

mminus227HOPO = melt(minus227HOPO, id="Days")
colnames(mminus227HOPO) = c("times", "Organs", "valuesminus")
mplus227HOPO = melt(plus227HOPO, id="Days")
colnames(mplus227HOPO) = c("times", "Organs", "valuesplus")

mOver225HOPO = melt(Over225HOPO, id="Days")
colnames(mOver225HOPO) = c("times", "Organs", "values")

mminusover225HOPO = melt(minusover225HOPO, id="Days")
colnames(mminusover225HOPO) = c("times", "Organs", "valuesminus")
mplusover225HOPO = melt(plusover225HOPO, id="Days")
colnames(mplusover225HOPO) = c("times", "Organs", "valuesplus")

mday225HOPO = melt(day225HOPO, id="Days")
colnames(mday225HOPO) = c("times", "Organs", "values")

mminusday225HOPO = melt(minusday225HOPO, id="Days")
colnames(mminusday225HOPO) = c("times", "Organs", "valuesminus")
mplusday225HOPO = melt(plusday225HOPO, id="Days")
colnames(mplusday225HOPO) = c("times", "Organs", "valuesplus")

mday227HOPO = melt(day227HOPO, id="Days")
colnames(mday227HOPO) = c("times", "Organs", "values")

mminusday227HOPO = melt(minusday227HOPO, id="Days")
colnames(mminusday227HOPO) = c("times", "Organs", "valuesminus")
mplusday227HOPO = melt(plusday227HOPO, id="Days")
colnames(mplusday227HOPO) = c("times", "Organs", "valuesplus")

#bind second data column to error sets

mAverage225errorHOPO = cbind(mminus225HOPO,mplus225HOPO[3])
mAverage227errorHOPO = cbind(mminus227HOPO,mplus227HOPO[3])
mOver225errorHOPO = cbind(mminusover225HOPO,mplusover225HOPO[3])
mday225errorHOPO = cbind(mminusday225HOPO,mplusday225HOPO[3])
mday227errorHOPO = cbind(mminusday227HOPO,mplusday227HOPO[3])

#plot225scale = c(1E-08, 1E-04, 1E-02, 1E0, 1E2)

#200 nCi Ac-225-HOPO-HOPO Dose (Gy)
plot225HOPO = ggplot()+ 
  geom_line(data=mAverage225HOPO, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mAverage225errorHOPO, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  ggtitle("HOPO")+
  
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

#plot227scales = c(1E-08,1E-08,1E-4,1E-2,1E0,1E2)

#20 nCi Ac-227-HOPO-HOPO Dose (Gy)
plot227HOPO = ggplot()+ 
  geom_line(data=mAverage227HOPO, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mAverage227errorHOPO, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
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

#plotover225scale=c(1E-08,1E-02,1E1,1E4)

#Ratio Dose (Ac-227/Ac-225)-HOPO-HOPO
plotover225HOPO = ggplot()+ 
  geom_line(data=mOver225HOPO, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mOver225errorHOPO, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
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



#plotday225scale=c(1E-09,1E-07,1E-5,1E-3,1E-1,1E1)

#Ratio Dose (Ac-227/Ac-225)-HOPO-HOPO
plotday225HOPO = ggplot()+ 
  geom_line(data=mday225HOPO, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mday225errorHOPO, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  ggtitle("HOPO")+
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(1,100,10000))+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plotday225scale),max(plotday225scale)), breaks=plotday225scale)+#breaks=c(#lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = element_blank(), y = "Ac-225 (Gy/day)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        plot.title = element_text(hjust = 0.5, size=18))+
  guides(shape=guide_legend(override.aes = list(size=3)))



#plotday227scale=c(1E-09,1E-07,1E-5,1E-3,1E-1,1E1)

#Ratio Dose (Ac-227/Ac-227)-HOPO-HOPO
plotday227HOPO = ggplot()+ 
  geom_line(data=mday227HOPO, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mday227errorHOPO, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(1,100,10000))+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "b", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_continuous()+#limits = c(min(plotday227scale),max(plotday227scale)), breaks=plotday227scale)+#breaks=c(#lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(margins, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "Ac-227 (Gy/day)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))+
  guides(shape=guide_legend(override.aes = list(size=3)))

#grid.arrange(arrangeGrob(plot225Dota, plot227Dota, ncol=2), arrangeGrob(plotover225Dota, ncol=1))
#plot225tras, plot227tras, ncol=2), arrangeGrob(plotover225tras, ncol=1))

#grid.arrange(arrangeGrob(plot225Dota, plot225tras, plot227Dota, plot227tras, plotover225Dota, plotover225tras, ncol=2))
#grid.arrange(arrangeGrob(plot225Dota, plot225tras, ncol=2), arrangeGrob(plot227Dota, plot227tras+theme(legend.position="bottom"), ncol=1))












#####To create legend ONLY####

plot225tras1 = ggplot()+ 
  geom_line(data=mAverage225tras, aes(x=times, y=values, color=Organs), size=1, alpha=1)+
  geom_ribbon(data=mAverage225errortras, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.1)+
  #ggtitle("DOTA-Trastuzumab")+
  
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  #scale_x_log10()+#breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  #annotation_logticks(base = 10, sides = "b", scaled = TRUE,
   #                   short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
    #                  colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  #scale_y_continuous()+#limits = c(min(plot225scale),max(plot225scale)), breaks=plot225scale)+#breaks=c(lseq(0.000001,100,9)))+
  theme_bw() +
  theme(legend.position="bottom", plot.margin = unit(margins, "cm"))+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #labs(y=element_blank(), x=element_blank(), color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))
#+
#guides(shape=guide_legend(override.aes = list(size=3)))


#create legend
legend <- get_legend(plot225tras1)
#as_ggplot(legend)


grid.arrange(arrangeGrob(legend, ncol=1), arrangeGrob(plot225Dota, plot225tras, plot227Dota, plot227tras, plotover225Dota, plotover225tras, ncol=2), heights=c(1,8))
grid.arrange(arrangeGrob(as_ggplot(legend), ncol=1), arrangeGrob(plotday225HOPO, plotday227HOPO, ncol=1), heights=c(1,8))


grid.arrange(arrangeGrob(legend, ncol=1), arrangeGrob(plot225HOPO, plot227HOPO, plotover225HOPO, ncol=1), heights=c(1,8))
grid.arrange(arrangeGrob(as_ggplot(legend), ncol=1), arrangeGrob(plotday225Dota, plotday225tras, plotday227Dota, plotday227tras, ncol=2), heights=c(1,8))



##OLD VERSION
#grid.arrange(arrangeGrob(legend, ncol=1), arrangeGrob(plot225HOPO, plot225Dota, plot225tras, plot227HOPO, plot227Dota, plot227tras, plotover225HOPO, plotover225Dota, plotover225tras, ncol=3), heights=c(1,8))
#grid.arrange(arrangeGrob(as_ggplot(legend), ncol=1), arrangeGrob(plotday225HOPO, plotday225Dota, plotday225tras, plotday227HOPO, plotday227Dota, plotday227tras, ncol=3), heights=c(1,8))


