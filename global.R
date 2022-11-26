
# Shiny app packages
library(shiny) 
library(shinythemes)     # Themes for shiny
library(shinyjs)         # JavaScript library 
library(shinyWidgets)    # Widgets library for using Shiny application
library(shinyBS)         # Bootstrap library
library(shinydashboard)  # Customized Dashboard for shiny
library(shinycssloaders) # Css loaders 

# Packages for data manipulation and figures 
library(DT)              # Data table library
library(plyr)            # Data manipulation library
library(dplyr)           # Data manipulation library
library(tidyr)           # Tidyverse Package 
library(readxl)          # Excel files reader library
library(dqshiny)         # Autocomplete input function 
library(icons)           # Creative icon package
library(heatmaply)       # Heatmap  plot package
library(ggplot2)         # ggplot package
library(downloader)      # Downloader function
library(stringr)         # String manipulation library
library(base)            # R base functions 




load("env/env.rdata")   # Load Requiring Table

icons <- icon_set("www/") #Icon set 

#Standard Heat map Matrix 
{
wh_matrix <- select(pure_ciliopathy_features, c(2,2:27 ))
wh_matrix <- data.matrix(wh_matrix)

wh_matrix2 <- select(secondary_ciliopathy_features, c(2,2:23 ))
wh_matrix2 <- data.matrix(wh_matrix2)

wh_matrix3 <- select(gene_localisations_ciliacarta, c(2,2:4))
wh_matrix3 <- data.matrix(wh_matrix3)}


#Merge pure and secondary Ciliopathies
list_Df <- rbind(purelist,secondarylist)





#Color change function was used reordering symptoms icon colour.
color_change <- function(ciliopathy_input,secondary_ciliopathy = FALSE){
  exception_list <- c("STAR Syndrome","Leber Congenital Amaurosis","Carpenter Syndrome")
  #Receives input from the user.
  if(secondary_ciliopathy == FALSE){
    if(ciliopathy_input %in% exception_list){
      #Create standart color table
      color_table <- data.frame('General Titles' = unique(for_main_symptome_titles_primary$`General Titles`), color = "#D6DBDF")
      detection <- for_main_symptome_titles_secondary[,ciliopathy_input]
      
      detection <- which(detection == 1)
      
      detection <- for_main_symptome_titles_secondary[detection,1]
      
      detection <- unique(detection)
      
      detected_symptome <- which(color_table$General.Titles %in% detection)
      
      #The color of figures changes if they're seen in selected ciliopathy.
      
      color_table[detected_symptome,2] <- "#F1C40F"
      color_table <- as.data.frame(t(color_table))
      colnames(color_table) <- color_table[1,]
      color_table <- color_table[-1,]
      
    }
    else{
      color_table <- data.frame('General Titles' = unique(for_main_symptome_titles_primary$`General Titles`), color = "#D6DBDF")
      
      detection <- for_main_symptome_titles_primary[,ciliopathy_input]
      
      detection <- which(detection == 1)
      
      detection <- for_main_symptome_titles_primary[detection,1]
      
      detection <- unique(detection)
      
      detected_symptome <- which(color_table$General.Titles %in% detection)
      
      color_table[detected_symptome,2] <- "#F1C40F"
      color_table <- as.data.frame(t(color_table))
      colnames(color_table) <- color_table[1,]
      color_table <- color_table[-1,]
    }}
  if (secondary_ciliopathy == TRUE){
    
    color_table <- data.frame('General Titles' = unique(for_main_symptome_titles_secondary$`General Titles`), color = "#D6DBDF")
    detection <- for_main_symptome_titles_secondary[,ciliopathy_input]
    
    detection <- which(detection == 1)
    
    detection <- for_main_symptome_titles_secondary[detection,1]
    
    detection <- unique(detection)
    
    detected_symptome <- which(color_table$General.Titles %in% detection)
    
    #The color of figures changes if they're seen in selected ciliopathy.
    color_table[detected_symptome,2] <- "#F1C40F"
    color_table <- as.data.frame(t(color_table))
    colnames(color_table) <- color_table[1,]
    color_table <- color_table[-1,]
    
    
  }
  return (color_table)}

#description_render Function creates explanation of the represented diseases
description_render <- function(your_table,ciliopathy_name){
  #Input came from Ciliopathy Names Page.
  #Check secondary or purelist input.
  if(nrow(your_table) == nrow(purelist)){
    
    if (length(ciliopathy_name) > 1){
      #Number of ciliopathy bigger than one, a description must create step by step.
      for (i in 1:length(ciliopathy_name)){
        detection <- which(list_Df[1] == ciliopathy_name[i])
        descrip <- list_Df[detection[1],2]
        known_name <- gsub("\\d","",descrip)
        known_name <- ifelse(known_name == "PKD","ADPKD/ARPKD",known_name)
        known_name <- ifelse(known_name == "USH1B","USH",known_name)
        known_name <- ifelse(known_name == "SMDA","SMD",known_name)
        known_name <- ifelse(known_name == "CILD","PCD",known_name)
        known_name <- ifelse(known_name == "USHB","USH",known_name)
        #Check its first description or not. 
        if(exists("description") == FALSE){
          description <- paste0(known_name,":",ciliopathy_name[i],",")
        }
        #Purelist table has multiple columns for PKD like ADPKD/ARPKD. This checkpoint was created To avoid multiple description.
        else if (!(str_detect(description,"ADPKD/ARPKD") == TRUE & known_name == "ADPKD/ARPKD")){
          if (i == length(ciliopathy_name)){
            description <- paste0(description,known_name,":",ciliopathy_name[i])
          }
          else{
            description <- paste0(description,known_name,":",ciliopathy_name[i],", ")
          }

        }
      }
      return(description)
    }
    else {
      #If input number smaller than two, create standard description.
      detection <- which(list_Df[1] == ciliopathy_name)
      descrip <- list_Df[detection[1],2]
      known_name <- gsub("\\d","",descrip)
      known_name <- ifelse(known_name == "PKD","ADPKD/ARPKD",known_name)
      known_name <- ifelse(known_name == "USH1B","USH",known_name)
      known_name <- ifelse(known_name == "SMDA","SMD",known_name)
      known_name <- ifelse(known_name == "CILD","PCD",known_name)
      known_name <- ifelse(known_name == "USHB","USH",known_name)
      if (known_name %in% c("BBS","JBTS","NPHP","MKS")){
        description2 <- paste0("BBS: Bardet-Biedl Syndrome, JBTS: Joubert Syndrome, NPHP: Nephronophthisis, MKS: Meckel-Gruber Syndrome")
      }
      else{
        description2 <- paste0("BBS: Bardet-Biedl Syndrome, JBTS: Joubert Syndrome, NPHP: Nephronophthisis, MKS: Meckel-Gruber Syndrome, ",known_name,":",ciliopathy_name)
      }
      
      return(description2)
    }
  }
  #Create description for secondary ciliopathy
  if (nrow(your_table) == nrow(secondarylist)){
    if (length(ciliopathy_name) > 1){
      for (x in 1:length(ciliopathy_name)){
        detection <- which(list_Df[1] == ciliopathy_name[x])
        descrip <- list_Df[detection[1],2]
        known_name <- gsub("\\d","",descrip)
        known_name <- ifelse(known_name == "PKD","ADPKD/ARPKD",known_name)
        known_name <- ifelse(known_name == "USH1B","USH",known_name)
        known_name <- ifelse(known_name == "SMDA","SMD",known_name)
        known_name <- ifelse(known_name == "CILD","PCD",known_name)
        known_name <- ifelse(known_name == "USHB","USH",known_name)
        if(exists("description3") == FALSE){
          description3 <- paste0(known_name,":",ciliopathy_name[1],",")
        }
        else{
          if (x == length(ciliopathy_name)){
            description3 <- paste0(description3,known_name,":",ciliopathy_name[1])
          }
          else{
            description3 <- paste0(description3,known_name,":",ciliopathy_name[1],", ")
          }
        }
      }
      return(description3)
    }
    else{
      #Secondary ciliopathies' standart description. 
      detection <- which(list_Df[1] == ciliopathy_name)
      descrip <- list_Df[detection[1],2]
      known_name <- gsub("\\d","",descrip)
      known_name <- ifelse(known_name == "PKD","ADPKD/ARPKD",known_name)
      known_name <- ifelse(known_name == "USH1B","USH",known_name)
      known_name <- ifelse(known_name == "SMDA","SMD",known_name)
      known_name <- ifelse(known_name == "CILD","PCD",known_name)
      known_name <- ifelse(known_name == "USHB","USH",known_name)
      if(known_name %in% c("ATLD","HDCA","MDB")){
        description4 <- paste0("ATLD: Ataxia-telangiectasia-like Disorder, HDCA: Congenital Heart Disease, MDB: Medulloblastoma")
      }
      else{
        description4 <- paste0("ATLD: Ataxia-telangiectasia-like Disorder, HDCA: Congenital Heart Disease, MDB: Medulloblastoma, ",known_name,":",ciliopathy_name)
      }
      
      return(description4)
    }
  }
    
}



# Heatmap_arrange function was used for creating various heatmaps.
# ciliopathy_name input came from users that can be one or multiple. Features table and your_table inputs depend on pure or secondary ciliopathy. 
heatmap_arrange <- function(your_table,features_table,ciliopathy_name){
  ##Where input count bigger than one, heat map creates step by step.
  if(length(ciliopathy_name) > 1){
    i = 1
    while (i < length(ciliopathy_name)){
      for (name in 1:length(ciliopathy_name)){
        q <- which(your_table[1] == ciliopathy_name[name])
        known_name <- your_table$Abbreviation[q[1]]
        known_name <- gsub("\\d","",known_name)
        known_name <- ifelse(known_name == "PKD","ADPKD/ARPKD",known_name)
        known_name <- ifelse(known_name == "USH1B","USH",known_name)
        known_name <- ifelse(known_name == "SMDA","SMD",known_name)
        known_name <- ifelse(known_name == "CILD","PCD",known_name)
        known_name <- ifelse(known_name == "USHB","USH",known_name)
        if (nrow(your_table) == nrow(purelist) ){
          if (i == 1){
            standart_heatmap <- select(features_table,known_name)
            i = i+1
          }
          else{
            selected_heatmap <- select(features_table,known_name)
            standart_heatmap[,i] <- selected_heatmap
            i = i+1}
        }
        else{
          if (i == 1){
            standart_heatmap <- select(features_table,known_name)
            i = i+1
          }
          else{
            selected_heatmap <- select(features_table,known_name)
            standart_heatmap[,i] <- selected_heatmap
            i = i+1}
        }}
      standart_heatmap <- standart_heatmap[!duplicated(t(standart_heatmap))]
    return(standart_heatmap)}
  }
  
  #Where input count smaller than two, user input adds to standart heat map. 
  else{
    q <- which(your_table[1] == ciliopathy_name)
    known_name <- your_table$Abbreviation[q[1]]
    known_name <- gsub("\\d","",known_name)
    known_name <- ifelse(known_name == "PKD","ADPKD/ARPKD",known_name)
    known_name <- ifelse(known_name == "USH1B","USH",known_name)
    known_name <- ifelse(known_name == "SMDA","SMD",known_name)
    known_name <- ifelse(known_name == "CILD","PCD",known_name)
    known_name <- ifelse(known_name == "USHB","USH",known_name)
    #Sub tab name purelist or secondary.
    if (nrow(your_table) == nrow(purelist) ){
      standart_heatmap <- select(pure_ciliopathy_features,"BBS","JBTS","NPHP","MKS")
      if (known_name == "BBS" | known_name == "JBTS" | known_name == "NPHP" | known_name == "MKS"){
        return(standart_heatmap)
      }
      else {
        selected_heatmap <- select(features_table,known_name)
        standart_heatmap[,5] <- selected_heatmap
        return(standart_heatmap)
      }
      
    }
    else{
      standart_heatmap <- select(secondary_ciliopathy_features,"ATLD","HDCA","MDB")
      if (known_name == "ATLD" | known_name == "HDCA" | known_name == "MDB"){
        return(standart_heatmap)
      }
      else {
        selected_heatmap <- select(features_table,known_name)
        standart_heatmap[,4] <- selected_heatmap
        return(standart_heatmap)
      }
    }}}


#check_multiple_id and reference_sorter functions adds reference,gene id,omim number etc. links to tables. 
{
  #Checking multiple ids, and split two columns. 
check_multiple_id <- function (your_table2,your_table_col) {
  ids <- strsplit(your_table2[[your_table_col]],", ")
  for (i in 1:length(ids)){
    for (x in 1:length(ids[[i]])){
      if (is.na(ids[[i]][x])){
        ids[[i]][x] <- ids[[i]][x]
      }
      else if (str_detect(ids[[i]][x],"/") == TRUE) {
        ids[[i]][x] <-  paste0("<a href= '","https://doi.org/",ids[[i]][x],"' target='_blank '>",ids[[i]][x],"</a>")
      }
      else {
        ids[[i]][x] <-  paste0("<a href= '","https://pubmed.ncbi.nlm.nih.gov/",ids[[i]][x],"' target='_blank '>",ids[[i]][x],"</a>")
      }
    }
  }
  ids <- sapply(ids, paste, collapse=",")
  your_table2[[your_table_col]] <- ids
  your_table2[[your_table_col]] <- str_replace(your_table2[[your_table_col]],",","<br>")
  return(your_table2)
  }

# Creating reference links for tables.
reference_sorter <- function (your_table) {
  
  your_table$'Human Gene ID' <- paste0("<a href= '","https://www.ncbi.nlm.nih.gov/gene/",your_table$'Human Gene ID',"' target='_blank '>",your_table$'Human Gene ID',"</a>")
  if (length(which(your_table$'Human Gene ID' == "<a href= 'https://www.ncbi.nlm.nih.gov/gene/NA' target='_blank '>NA</a>")) >0 ){
    human_which <- which(your_table$'Human Gene ID' == "<a href= 'https://www.ncbi.nlm.nih.gov/gene/NA' target='_blank '>NA</a>")
    your_table$'Human Gene ID'[human_which] <- "No Gene ID"
  }
  
  your_table$'OMIM Phenotype Number' <- paste0("<a href= '","https://www.omim.org/entry/",your_table$'OMIM Phenotype Number',"' target='_blank '>",your_table$'OMIM Phenotype Number',"</a>")
  if (length(which(your_table$'OMIM Phenotype Number' == "<a href= 'https://www.omim.org/entry/No MIM Number' target='_blank '>No MIM Number</a>")) > 0){
    omim_which <- which(your_table$'OMIM Phenotype Number' == "<a href= 'https://www.omim.org/entry/No MIM Number' target='_blank '>No MIM Number</a>")
    your_table$'OMIM Phenotype Number'[omim_which] <- "No MIM Number"
  }
  
  your_table$'Gene MIM Number' <- paste0("<a href= '","https://www.omim.org/entry/",your_table$'Gene MIM Number',"' target='_blank '>",your_table$'Gene MIM Number',"</a>")
  if(length(which(your_table$'Gene MIM Number' == "<a href= 'https://www.omim.org/entry/NA' target='_blank '>NA</a>")) > 0 ){
    mim_which <- which(your_table$'Gene MIM Number' == "<a href= 'https://www.omim.org/entry/NA' target='_blank '>NA</a>")
    your_table$'Gene MIM Number'[mim_which] <- "No MIM Number"
  }
  
  if(length(which(your_table$`Gene MIM Number` == "<a href= 'https://www.omim.org/entry/No MIM Number' target='_blank '>No MIM Number</a>")) > 0 ){
    mim_which <- which(your_table$`Gene MIM Number` == "<a href= 'https://www.omim.org/entry/No MIM Number' target='_blank '>No MIM Number</a>")
    your_table$'Gene MIM Number'[mim_which] <- "No MIM Number"
  }
  
  if (length(which(colnames(your_table) =='Disease/Gene Reference')) > 0){
    gene_ref <- which(colnames(your_table) =='Disease/Gene Reference')
    your_table <-  check_multiple_id(your_table,gene_ref)
    local_ref <- which(colnames(your_table) == "Localisation Reference")
    your_table <- check_multiple_id(your_table,local_ref)
  }
  
  if (length(which(colnames(your_table) =="Cilia Related Process Pubmed ID")) > 0){
    gene_ref <- which(colnames(your_table) =="Cilia Related Process Pubmed ID")
    your_table <- check_multiple_id(your_table,gene_ref)
    your_table <- your_table[,1:5]
  }
  
  if(length(colnames(your_table)) == 8 ) {
    local_ref <- which(colnames(your_table) =='Localisation Reference')
    your_table <- check_multiple_id(your_table,local_ref)
    your_table <- your_table[,1:7]
  }

  return(your_table)}
}

#Symptomes and Disease Page, symptomes table creator. 

symptom_detector <- function(input_name) {
  clinical_features <- rbind(clinical_feature_pure,clinical_feature_secondary)
  features <- clinical_features[str_detect(clinical_features$Clinical_features,input_name),]
  ciliopathies <- unique(features$Ciliopathy)
  pures <- purelist[(purelist$Ciliopathy %in% ciliopathies),]
  secondary <- secondarylist[(secondarylist$Ciliopathy %in% ciliopathies),]
  result_table <- rbind(pures,secondary)
  result_table <- result_table[order(result_table$Ciliopathy),]
  return(result_table)
}

disease_symptom <- function(disease_name){
  if(disease_name %in% clinical_feature_pure$Ciliopathy){
    symptoms <- clinical_feature_pure[clinical_feature_pure$Ciliopathy == disease_name,]
    symptoms$gene <- paste0("<a href= '","https://www.ncbi.nlm.nih.gov/gene/?term=homo+sapiens%20",symptoms$gene,"' target='_blank '>",symptoms$gene,"</a>")
    symptoms$OMIM_number <- paste0("<a href= '","https://www.omim.org/clinicalSynopsis/",symptoms$OMIM_number,"' target='_blank '>",symptoms$OMIM_number,"</a>")
    symptoms <- symptoms %>% 
      group_by(Clinical_features) %>% summarise(gene = paste(unique(gene), collapse = ", "),
                                                OMIM_number = paste(unique(OMIM_number), collapse = ", "))
    colnames(symptoms)[1] <- "Clinical Features"
    colnames(symptoms)[2] <- "Human Gene Name"
    colnames(symptoms)[3] <- "OMIM Number"
    return(symptoms)
  }
  else {
    symptoms <- clinical_feature_secondary[clinical_feature_secondary$Ciliopathy == disease_name,]
    symptoms$gene <- paste0("<a href= '","https://www.ncbi.nlm.nih.gov/gene/?term=homo+sapiens%20",symptoms$gene,"' target='_blank '>",symptoms$gene,"</a>")
    symptoms$OMIM_number <- paste0("<a href= '","https://www.omim.org/clinicalSynopsis/",symptoms$OMIM_number,"' target='_blank '>",symptoms$OMIM_number,"</a>")
    symptoms <- symptoms %>% 
      group_by(Clinical_features) %>% summarise(gene = paste(unique(gene), collapse = ", "),
                                                OMIM_number = paste(unique(OMIM_number), collapse = ", "))
    colnames(symptoms)[1] <- "Clinical Features"
    colnames(symptoms)[2] <- "Human Gene Name"
    colnames(symptoms)[3] <- "OMIM Clinical Synopsis Number"
    return(symptoms)
  }
  
  
}
