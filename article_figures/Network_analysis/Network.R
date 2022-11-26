### Interaction network ----
#install.packages('sna')
library(GGally)
library(network)
#library(readxl)


# Build network object

all_scores2<-read.table("files_for_network/all_scores2.txt", sep = "\t", header = TRUE, fill = TRUE)
ciliaryGenes<-read.table("files_for_network/ciliaryGenes.txt", sep = "\t", header = TRUE)
negativeCiliary<-read.table("files_for_network/negativeCiliary.txt", sep = "\t", header = TRUE)
ciliarynet<-read.table("files_for_network/protein_interaction_file.txt", sep = "\t", header = TRUE)

glist<-c("NIPBL", "SMC1A", "SMC3", "RAD21", "HDAC8", "ANKRD11", "BRD4") #gene_list

# **************************************************************
###### this will draw edges between selected genes and their interactome
ciliarynet2<-ciliarynet[ciliarynet$Interactor_A %in% glist | ciliarynet$Interactor_B %in% glist,]

# **************************************************************

ciliarynet2$is.ciliary<-ifelse(ciliarynet2$Interactor_B %in% ciliaryGenes$Gene_name, "Ciliary", 
                               ifelse(ciliarynet2$Interactor_B %in% negativeCiliary$Gene_name, "Non-ciliary", "Unknown"))

ciliarynet2 <- ciliarynet2[ciliarynet2$is.ciliary != "Unknown",]
ciliarynet2 <- ciliarynet2[ciliarynet2$is.ciliary == "Ciliary",]


cilsimpnet<-unique(ciliarynet2[,1:2])
cilsimpnet<-cilsimpnet[cilsimpnet$Interactor_A != cilsimpnet$Interactor_B,]

nw <- network(cilsimpnet, directed = TRUE, matrix.type = "edgelist", loops = FALSE)

vertexnames<-network.vertex.names(nw)

a <- vertexnames[vertexnames %in% ciliaryGenes$Gene_name]

vertexnames <- a

nw %v% "is.ciliary" = ifelse(vertexnames %in% ciliaryGenes$Gene_name, "Ciliary", "Non-ciliary")


#Generate Plot
set.seed(2)
ggnet2(nw, color = "tomato", node.size = 15, label = TRUE, label.size = 4, mode = "kamadakawai")






