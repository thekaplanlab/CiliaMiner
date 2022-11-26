library(ggplot2)
library(viridis)
library(hrbrthemes)
library(stringr)
library(readxl)


#Reordering Plot Data

barplot_figure <- read_xlsx("barplot excel.xlsx",sheet = 1)

purelist <- barplot_figure[,1:2]
purelist[,3] <- "Purelist"
colnames(purelist)[2] <- "Value"

atypical<- barplot_figure[,c(1,3)]
atypical[,3] <- "Atypical"
colnames(atypical)[2] <- "Value"


secondary <- barplot_figure[,c(1,4)]
secondary[,3] <- "Secondary"
colnames(secondary)[2] <- "Value"

motile <- barplot_figure[,c(1,5)]
motile[,3] <- "Motile"
colnames(motile)[2] <- "Value"


barplot <- rbind(purelist,atypical,secondary,motile)
colnames(barplot)[3] <- "Type"
colnames(barplot)[1] <- "Localization"



#### Generate Plot


colors <- c("#8ecae6","#00b4d8","#0070c0","#219ebc","#70d6ff",
            "#6B99C3","#999999","#073b4c","#118ab2","#D2D2D4",
            "#00a8e8","#007ea7","#7dcfb6","#3e92cc")

ggplot(barplot, aes(fill = Localization, y=factor(Type,levels = c("Atypical","Secondary","Purelist","Motile")), x=Value)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual("legend", values = colors) +
  theme_void() +
  theme(legend.title = element_text(color = "black", size = 16,face="bold"),
        legend.text = element_text(color = "black", size = 14,face="bold"),
        legend.position="bottom",
        axis.text.y = element_text(color = "black",face = "bold",size =14),
        plot.title = element_text(hjust = 0.5))+
  xlab("")











