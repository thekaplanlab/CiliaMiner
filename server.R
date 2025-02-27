library(shiny)
library(shinythemes)
set.seed(500)
server <- function(input, output, session) {
  

  #Homepage 
  {
  
  #Homepage Gene Search
  output$search_table <- DT::renderDataTable({
    if( !(toupper((input$search)) %in% toupper(unique(searching_gene)))){
      if ( input$search != ""){ # Send sweet alert 
        sendSweetAlert(
          session = session,
          title = "This is not a ciliopathy related gene",
          text = tags$span(tags$h3("Please try another gene")),
          type = "info",
          html = TRUE,
          showCloseButton = TRUE
        )
      }
    }
     #Create Result Table
    if(toupper((input$search)) %in% toupper(unique(searching_gene)) ){
      {colnames(homosapiens_ciliopathy)[3] <- "OMIM<br>Phenotype Number"
        colnames(homosapiens_ciliopathy)[4] <- "Disease/Gene<br>Reference"
        colnames(homosapiens_ciliopathy)[5] <- "Human<br>Gene ID"
        colnames(homosapiens_ciliopathy)[6] <- "Human Gene<br>Name"
        colnames(homosapiens_ciliopathy)[8] <- "Subcellular<br>Localization"
        colnames(homosapiens_ciliopathy)[7] <- "Gene MIM<br>Number"
        colnames(homosapiens_ciliopathy)[9] <- "Localisation<br>Reference"}
      homosapiens_ciliopathy <- homosapiens_ciliopathy[,c(1,6,8,10)]
      
      DT::datatable(homosapiens_ciliopathy,rownames = NULL,escape = FALSE,
                    options = list(dom = 'lt',initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                   server = FALSE,
                                   pageLength = 10,
                                   autoWidth = FALSE,columnDefs = list(list(width = 'fit-content', targets = c(1,2), className="dt-center"),
                                                                       list(visible=FALSE, targets = c(3)),
                                                                       list(targets = c(0,2), searchable = FALSE),
                                                                       list(width = '15%', targets = c(0), className="dt-left")),
                                   lengthMenu = c(10,25,50,100,200),
                                   search = list(search = input$search)))
    }})

  #Homepage Ciliopathy Search
  output$search_cilia_table <- DT::renderDataTable({

    if(input$disease_search %in% unique(homosapiens_ciliopathy$Ciliopathy)){
      which_row <- which(homosapiens_ciliopathy$Ciliopathy == input$disease_search)
      homosapiens_ciliopathy <- homosapiens_ciliopathy[which_row,]
      {colnames(homosapiens_ciliopathy)[3] <- "OMIM<br>Phenotype Number"
        colnames(homosapiens_ciliopathy)[4] <- "Disease/Gene<br>Reference"
        colnames(homosapiens_ciliopathy)[5] <- "Human<br>Gene ID"
        colnames(homosapiens_ciliopathy)[6] <- "Human Gene<br>Name"
        colnames(homosapiens_ciliopathy)[8] <- "Subcellular<br>Localization"
        colnames(homosapiens_ciliopathy)[7] <- "Gene MIM<br>Number"
        colnames(homosapiens_ciliopathy)[9] <- "Localisation<br>Reference"}
      homosapiens_ciliopathy <- homosapiens_ciliopathy[,c(1,6,8,10)]
      DT::datatable(homosapiens_ciliopathy,rownames = NULL,escape = FALSE,
                    options = list(dom = 'lt',initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                   server = FALSE,
                                   pageLength = 10,
                                   autoWidth = FALSE,columnDefs = list(list(width = 'fit-content', targets = c(1,2), className="dt-center"),
                                                                       list(targets = c(0:2), searchable = FALSE),
                                                                       list(visible=FALSE, targets = c(3)),
                                                                       list(width = '15%', targets = c(0), className="dt-left")),
                                   lengthMenu = c(10,25,50,100,200)))
    }})
  
  #Homepage Plots
  {
    #Bar plot Localisations of Ciliopathy Associated Genes
    output$barplot <- renderPlotly({
      plot_ly(bar_plot, 
              y = ~Gene_number, 
              x=~Ciliary_Localisation,
              type = 'bar',
              color =~Ciliary_Localisation,
              mode='markers',
              size = ~Gene_number,
              marker = list(opacity = 0.5,  sizemode = 'diameter'))%>% 
        config(displayModeBar = FALSE) %>% 
        layout(
          title = 'Localisations of Ciliopathy Associated Genes',
          xaxis = list(showgrid = FALSE,fixedrange = TRUE, title="Ciliary Localisations", visible= FALSE),
          yaxis = list(showgrid = FALSE,fixedrange = TRUE, visible= FALSE))
    })
    
    #Pie chart Classification Based on Ciliopathy Genes
    output$piechart <- renderPlotly({
      plot_ly(gene_numbers_d,
              labels=~Disease, 
              values = ~Gene_numbers, 
              type = 'pie',
              textposition = 'outside',
              textinfo = 'label+percent',
              insidetextfont = list(color = 'Paired2'), 
              hoverinfo = 'text',
              marker = list(colors = colors,
                            line = list(color = 'Paired2', width = 1)),
              showlegend = FALSE
      )%>% 
        config(displayModeBar = FALSE) %>% 
        layout(
          title = 'Classification Based on Ciliopathy Genes')
    })
    
    #Bubble Plot Publication Numbers of Ciliopathy Related Genes
    #Plot  can change based on Gene Search Input
    output$bubble <- renderPlotly({
      
      if( !((input$search) %in% homosapiens_ciliopathy$`Human Gene Name`)){
        plot_ly(publication, 
                x = ~year, 
                y = ~publication_number, 
                hovertemplate = paste('<b>Publication</b>: %{y}',
                                      '<br><b>Year</b>: %{x}<br>',
                                      '<b>%{text}</b><extra></extra>'), #Reorder Hover
                text = ~gene_name,
                type = 'scatter', 
                mode = 'marker',
                size = ~publication_number,
                color = ~publication_number,
                marker = list(opacity = 0.5,  sizemode = 'diameter'),
                sizes = c(10, 25)
        )%>% config(displayModeBar = FALSE,modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))%>%
          layout(
            title = 'Publication Numbers of Ciliopathy Related Genes (2000-2024)',
            xaxis = list(showgrid = T,fixedrange = TRUE,title="Publication Years"),
            yaxis = list(showgrid = FALSE,fixedrange = TRUE, visible= FALSE))
      }
      else if(input$search %in% homosapiens_ciliopathy$`Human Gene Name`){
        #Subscript table with using input gene
        publication_table <- publication_table[publication_table$gene_name == input$search,]
        publication_table <- publication_table[!is.na(publication_table$publication_number),]
        publication_table$publication_number <- as.integer(publication_table$publication_number)
        plot_ly(publication_table,
                x = ~year, 
                y = ~publication_number,
                hovertemplate = paste('<b>Publication</b>: %{y}',
                                      '<br><b>Year</b>: %{x}<br>',
                                      '<b>%{text}</b><extra></extra>'), #Reorder Hover
                text = ~gene_name, 
                type = 'scatter', 
                mode = 'marker',
                size = ~publication_number,
                color = ~publication_number,
                marker = list(opacity = 0.5,  sizemode = 'diameter'),
                sizes = c(10, 25)
        )%>% config(displayModeBar = FALSE,modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))%>%
          layout(
            title = 'Publication Numbers of Ciliopathy Related Genes (2000-2022)',
            xaxis = list(showgrid = T,fixedrange = TRUE,title="Publication Years"),
            yaxis = list(showgrid = FALSE,fixedrange = TRUE, visible= FALSE))
      }
    })
  }
  
  }


  #Ciliopathy Names Page
  {
  
  #Pure Ciliopathies Table 
  {
    #Observe Heatmap element and Reactive All and Clear Button
    observeEvent(input$all, {
      updateMultiInput(
        session = session,
        inputId = "heatmaps",
        selected = unique(purelist$Ciliopathy)
      )
    })
    
    observeEvent(input$namepures,{
      updateMultiInput(
        session = session,
        inputId = "heatmaps",
        selected = character(0)
      )
    })
    
    observeEvent(input$none, {
      updateMultiInput(
        session = session,
        inputId = "heatmaps",
        selected = character(0)
      )
    })
    
    #Output Pure Ciliopathies Table
  output$namepuresout <-    DT::renderDataTable({
    purelist <- reference_sorter(purelist)
    
    #colnames visualization
    { colnames(purelist)[3] <- "OMIM<br>Phenotype Number"
      colnames(purelist)[4] <- "Disease/Gene<br>Reference"
      colnames(purelist)[5] <- "Human<br>Gene ID"
      colnames(purelist)[6] <- "Human Gene<br>Name"
      colnames(purelist)[8] <- "Subcellular<br>Localization"
      colnames(purelist)[7] <- "Gene MIM<br>Number"
      colnames(purelist)[9] <- "Localisation<br>Reference"}
    if(input$namepures != "All Pure Ciliopathy"){
    purelist <- subset(purelist, purelist$Ciliopathy == input$namepures)
    row.names(purelist) <- NULL
    DT::datatable(purelist,escape = FALSE,extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',
                                 autoWidth = TRUE,
                                 searchHighlight = TRUE,
                                 columnDefs = list(list(targets = 1, className = "dt-left"),
                                                   list(width = 'fit-content', targets = c(2:9), className="dt-center"),
                                                   list(visible = FALSE,targets = c(10,11))),
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))  }
    DT::datatable(purelist,escape = FALSE,extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',
                                 autoWidth = TRUE,
                                 searchHighlight = TRUE,
                                 columnDefs = list(list(targets = 1, className = "dt-left"),
                                                   list(width = 'fit-content', targets = c(2:9), className="dt-center"),
                                                   list(visible = FALSE,targets = c(10,11))),
                                 lengthMenu = c(10,20,50,100,nrow(purelist)),
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) 
    
       })
  #Output heatmap for pure ciliopathies
  #If heatmap option is empty or user changes the ciliopathy name, heatmap returns the standard option. 
  output$namepuresout2 <- renderPlotly({
    #Return All Ciliopathy Heatmap
    if((input$namepures == "All Pure Ciliopathy") == TRUE){wh_matrix4 <- heatmap_arrange(purelist,pure_ciliopathy_features,"Joubert Syndrome")}
    
    #Return Standard heatmap
    if(is.null(input$heatmaps) & (input$namepures != "All Pure Ciliopathy") ) {wh_matrix4 <-heatmap_arrange(purelist,pure_ciliopathy_features,input$namepures)}
    
    #Reorder heatmap by using user input
    else if(!is.null(input$heatmaps)){wh_matrix4 <-heatmap_arrange(purelist,pure_ciliopathy_features,input$heatmaps)}
    heatmaply(wh_matrix4, 
              seriate = "GW", 
              colors = OrRd,
              main="Clinical Features",
              xlab = "Ciliopathy Types",
              ylab = "Clinical Symptomes",
              fontsize_row = 10,
              fontsize_col = 10,
              dendrogram = "none",
              plot_method= c("plotly"),
              hide_colorbar = TRUE,
              Colv=TRUE,
              Rowv = TRUE,
              dist_method = "euclidean",
              labRow = pure_ciliopathy_features$"Ciliopathy / Clinical Features", 
              label_names =  c("Clinical Feature", "Ciliopathy", "Value"),
              row_dend_left = FALSE,
              showticklabels = c(TRUE, FALSE))
  })
  
  #Text changes based on selected Ciliopathies
  output$outputtext <- renderText({
    if(input$namepures != "All Pure Ciliopathy"){
      if(is.null(input$heatmaps)) {heatmap_descript <-description_render(your_table = purelist, input$namepures)}
      if(!is.null(input$heatmaps)){heatmap_descript <-description_render(your_table = purelist, input$heatmaps)}
    }else{
      heatmap_descript <- "BBS: Bardet-Biedl Syndrome, JBTS: Joubert Syndrome, NPHP: Nephronophthisis, MKS: Meckel-Gruber Syndrome"
      
    }

    return(heatmap_descript)
    })
  
  #Disease Symptom Summary Figures. Colors changes from disease to disease.
  {
    output$Aural <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else{color_table <- color_change(input$namepures)
      color <- color_table$`Aural Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "assistive-listening-systems","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Aural Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    
    output$neural <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Neural Anomalies`[1]
      }
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      
      
      h4(popify(el = icon(name = "brain","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Neural Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$ophthalmic <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Ophthalmic Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "eye","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Ophthalmic Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$skeletal <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Skeletal Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "bone","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Skeletal Anomalies"
                ),placement="left",options = list(container = "body")))})
    
    output$respiratory <-renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Respiratory Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "lungs","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Respiratory Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$hormonal <-renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Hormonal Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "balance-scale-left","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Hormonal Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$Reproductive <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Reproductive Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "venus-mars","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Reproductive Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$Facial <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Facial Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "grin","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Facial Anomalies"
                ),placement="left",options = list(container = "body")))
    })
    
    output$cerebral <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Cerebral Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-neurology`,scale = 3, fill = color))
    })
    
    output$Renal <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Renal Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-kidney`,scale = 3, fill = color))
    })
    
    output$Coronary <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namepures)
        color <- color_table$`Coronary and Vascular Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-cardiology`,scale = 3, fill = color))
    })
    
    output$Nasal <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else{
        color_table <- color_change(input$namepures)
        color <- color_table$`Nasal Anomalies`[1]}
      
      h4(icon_style(icons$`i-nose`,scale = 3, fill = color))
    })
    
    output$liver <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else{color_table <- color_change(input$namepures)
      color <- color_table$`Liver Anomalies`[1]}
      h4(icon_style(icons$`i-liver`,scale = 3, fill = color))
    })
    
    
    
    
    
    
    output$Cognitive <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else{
        color_table <- color_change(input$namepures)
        color <- color_table$`Cognitive Anomalies`
      }
      h4(icon_style(icons$`i-mental-health`,scale = 3, fill = color)) })
    
    output$Digestive <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else{
        color_table <- color_change(input$namepures)
        color <- color_table$`Digestive Anomalies`[1]
      }
      h4(icon_style(icons$`i-stomach`,scale = 3, fill = color))
    })
    output$Organ <- renderUI({
      if(input$namepures == "All Pure Ciliopathy"){color <- "#F1C40F" }
      else{color_table <- color_change(input$namepures)
      color <- color_table$`Organ Anomalies`[1]}
      
      h4(icon_style(icons$`i-internal-medicine`,scale = 3, fill = color))
    })
    
  }
  
  }
  
  #Secondary Ciliopathies Table
  {
    #Observe Heatmap element and Reactive All and Clear Button
    observeEvent(input$namesecondary,{
      updateMultiInput(
        session = session,
        inputId = "heatmaps2",
        selected = character(0)
      )
    })
    
    observeEvent(input$all2, {
      updateMultiInput(
        session = session,
        inputId = "heatmaps2",
        selected = unique(secondarylist$Ciliopathy)
      )
    })
    
    observeEvent(input$none2, {
      updateMultiInput(
        session = session,
        inputId = "heatmaps2",
        selected = character(0)
      )
    })
    
    
  #Output Secondary Ciliopathies Table
  output$namesecondsout <- DT::renderDataTable({
    secondarylist <- reference_sorter(secondarylist)
    #colnames visualization
    { colnames(secondarylist)[3] <- "OMIM<br>Phenotype Number"
      colnames(secondarylist)[4] <- "Disease/Gene<br>Reference"
      colnames(secondarylist)[5] <- "Human<br>Gene ID"
      colnames(secondarylist)[6] <- "Human Gene<br>Name"
      colnames(secondarylist)[8] <- "Subcellular<br>Localization"
      colnames(secondarylist)[7] <- "Gene MIM<br>Number"
      colnames(secondarylist)[9] <- "Localisation<br>Reference"
    colnames(secondarylist)[1] <- "Disease"}
    
    if(input$namesecondary =="All Secondary Ciliopathy"){
      DT::datatable(secondarylist,escape = FALSE,extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   autoWidth = TRUE,
                                   searchHighlight = TRUE,
                                   columnDefs = list(list(targets = 1, className = "dt-left"),
                                                     list(width = 'fit-content', targets = c(2:9), className="dt-center"),
                                                     list(targets = c(10,11),visible = FALSE)),
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    }else{
      secondarylist <- subset(secondarylist, secondarylist$Disease == input$namesecondary)
      row.names(secondarylist) <- NULL
      DT::datatable(secondarylist,escape = FALSE,extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   autoWidth = TRUE,
                                   searchHighlight = TRUE,
                                   lengthMenu = c(10,20,50,100,nrow(secondarylist)),
                                   columnDefs = list(list(targets = 1, className = "dt-left"),
                                                     list(width = 'fit-content', targets = c(2:9), className="dt-center"),
                                                     list(targets = c(10,11),visible = FALSE)),
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    }}) 

  #If heatmap option is empty or user changes the ciliopathy name, heatmap returns the standard option. 
  output$namesecondsout2 <- renderPlotly({
    #Return All Ciliopathy Heatmap
    if((input$namesecondary == "All Secondary Ciliopathy") == TRUE) {wh_matrix4 <-heatmap_arrange(secondarylist,secondary_ciliopathy_features,"Ataxia-telangiectasia-like Disorder")}
    if(is.null(input$heatmaps2) & (input$namesecondary != "All Secondary Ciliopathy")) {wh_matrix4 <-heatmap_arrange(secondarylist,secondary_ciliopathy_features,input$namesecondary)}
    else if(!is.null(input$heatmaps2)){wh_matrix4 <-heatmap_arrange(secondarylist,secondary_ciliopathy_features,input$heatmaps2)}
    

    heatmaply(wh_matrix4, 
              seriate = "GW", 
              colors = OrRd,
              main="Clinical Features",
              xlab = "Ciliopathy Types",
              ylab = "Clinical Symptomes",
              fontsize_row = 10,
              fontsize_col = 10,
              dendrogram = "none",
              plot_method= c("plotly"),
              hide_colorbar = TRUE,
              Colv=TRUE,
              Rowv = TRUE,
              dist_method = "euclidean",
              labRow = secondary_ciliopathy_features$"Ciliopathy / Clinical Features", 
              label_names =  c("Clinical Feature", "Ciliopathy", "Value"),
              row_dend_left = FALSE,
              showticklabels = c(TRUE, FALSE))
    
  })
  
  #Text changes based on selected Ciliopathies
  output$outputtext2 <- renderText({
    if(input$namesecondary != "All Secondary Ciliopathy"){
      if(is.null(input$heatmaps2)) {heatmap_descript2 <-description_render(your_table = secondarylist, input$namesecondary)}
      if(!is.null(input$heatmaps2)){heatmap_descript2 <-description_render(your_table = secondarylist, input$heatmaps2)}
    }
    else{
      heatmap_descript2 <- "ATLD: Ataxia-telangiectasia-like Disorder, HDCA: Congenital Heart Disease, MDB: Medulloblastoma"
    }
    
    return(heatmap_descript2)
  })
  
  #Disease Symptom Summary Figures. Colors vary from disease to disease. 
  {
    output$Aural1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Aural Anomalies`[1]
      }
      h4(popify(el = icon(name = "assistive-listening-systems","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Aural Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$neural1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else{
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Neural Anomalies`[1]
        
      }
      h4(popify(el = icon(name = "brain","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Neural Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$ophthalmic1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Ophthalmic Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "eye","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Ophthalmic Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$skeletal1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Skeletal Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "bone","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Skeletal Anomalies"
                ),placement="left",options = list(container = "body")))})
    
    output$Other <-renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$Others
      }
      h4(popify(el = icon(name = "plus","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Other Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$hormonal1 <-renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Hormonal Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "balance-scale-left","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Hormonal Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$respiratory1 <-renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Respiratory Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "lungs","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Respiratory Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$Reproductive1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Reproductive Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "venus-mars","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Reproductive Anomalies"
                ),placement="top",options = list(container = "body")))})
    output$Facial1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Facial Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "grin","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Facial Anomalies"
                ),placement="left",options = list(container = "body")))})
    
    output$cerebral1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Cerebral Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-neurology`,scale = 3, fill = color))
    })
    output$Renal1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else{
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Renal Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-kidney`,scale = 3, fill = color))
    })
    output$Coronary1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Coronary and Vascular Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-cardiology`,scale = 3, fill = color))
    })
    output$Nasal1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Nasal Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-nose`,scale = 3, fill = color)) })
    output$liver1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Liver Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-liver`,scale = 3, fill = color))
    })
    output$Digestive1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$'Digestive Anomalies'[1]
      }
      
      h4(icon_style(icons$`i-stomach`,scale = 3, fill = color))
    })
    output$Organ1 <- renderUI({
      if(input$namesecondary == "All Secondary Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$namesecondary,TRUE)
        color <- color_table$`Organ Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-internal-medicine`,scale = 3, fill = color))
    })
    
  }
  
  
}
  
  #Atypical Ciliopathies Table
  {
    output$atypical_ciliopathy <- DT::renderDataTable({
      atypical_ciliopathy <- reference_sorter(atypical_ciliopathy)
      colnames(atypical_ciliopathy)[1] <- "Disease"
      colnames(atypical_ciliopathy)[2] <- "Human<br>Gene ID"
      colnames(atypical_ciliopathy)[3] <- "Human Gene<br>Name"
      colnames(atypical_ciliopathy)[5] <- "Subcellular<br>Localization"
      colnames(atypical_ciliopathy)[4] <- "Gene MIM<br>Number"
      colnames(atypical_ciliopathy)[6] <- "Localisation<br>Reference"
      DT::datatable(atypical_ciliopathy, escape = FALSE,extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                   server = FALSE,
                                   autoWidth = TRUE,
                                   searchHighlight = TRUE,
                                   columnDefs = list(list(targets = 1, className = "dt-left"),
                                                     list(targets = 7, visible = FALSE)),
                                   lengthMenu = c(10,25,50,100,nrow(atypical_ciliopathy)),
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))})
  }
  
  
  #Potential Ciliopathy Genes page Table and Heatmap
  
  {
    output$potential_ciliopathy_genes <- DT::renderDataTable({
      potential_ciliopathy_genes <- reference_sorter(potential_ciliopathy_genes)
      colnames(potential_ciliopathy_genes)[1] <- "Human<br>Gene ID"
      colnames(potential_ciliopathy_genes)[2] <- "Human Gene<br>Name"
      colnames(potential_ciliopathy_genes)[3] <- "Gene MIM<br>Number"
      colnames(potential_ciliopathy_genes)[4] <- "Cilia Related Process<br>Pubmed ID"
      DT::datatable(potential_ciliopathy_genes, escape = FALSE,extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                   server = FALSE,
                                   autoWidth = TRUE,
                                   searchHighlight = TRUE,
                                   columnDefs = list(list(targets = 1, className = "dt-left"),
                                                     list(targets = 5, visible = FALSE)),
                                   lengthMenu = c(10,25,50,100,nrow(potential_ciliopathy_genes)),
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))})
    
    
    
    output$potential_ciliopathy_genes_heatmap<-renderPlotly({
      heatmaply(wh_matrix3, 
                seriate = "GW", 
                colors = OrRd,
                main="Candidate Ciliary Genes Localisations",
                xlab = "Subcellular Ciliary Localisation",
                fontsize_row = 10,
                fontsize_col = 10,
                dendrogram = "none",
                plot_method= c("plotly"),
                hide_colorbar = TRUE,
                Colv=TRUE,
                Rowv = TRUE,
                dist_method = "euclidean",
                labRow = gene_localisations_ciliacarta$"Human Gene Name", 
                label_names =  c("Localisation", "Gene", "Value"),
                row_dend_left = FALSE,
                showticklabels = c(TRUE, FALSE))
    }) }
  
  }
  

  #Gene Search Page 
  {
    output$search_gene_table <- DT::renderDataTable({
      if( !(toupper((input$search_gene)) %in% toupper(unique(searching_gene)))){
        if ( input$search_gene != ""){ # Send sweet alert 
          sendSweetAlert(
            session = session,
            title = "This is not a ciliopathy related gene",
            text = tags$span(tags$h3("Please try another gene")),
            type = "info",
            html = TRUE,
            showCloseButton = TRUE
          )
        }
      }
      #Create Result Table
      if(toupper((input$search_gene)) %in% toupper(unique(searching_gene))){
        search_result_table <- gene_searh_function(input$search_gene)
        DT::datatable(search_result_table, escape = FALSE,extensions = 'Buttons',width = "auto",
                      options = list(dom = 'Blfrtip', 
                                     initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                     server = FALSE,
                                     autoWidth = TRUE,
                                     searchHighlight = TRUE,
                                     columnDefs = list(list(width = 'fit-content', targets = c(1), className="dt-left"),
                                       list(width = 'fit-content', targets = c(2:ncol(search_result_table)), className="dt-center"),
                                                       list(visible=FALSE, targets = c(ncol(search_result_table)))),
                                     lengthMenu = c(10,25,50,100,nrow(search_result_table)),
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
      }})
    
    
    
    
    
    
  }
  
  
  #Ciliopathy Genes and Orthologs Page
  {
  
  #Homosapiens Ciliopathy Table
  {
  output$homosapiens_ciliopathy <- DT::renderDataTable({
    homosapiens_ciliopathy <-reference_sorter(homosapiens_ciliopathy)
    {colnames(homosapiens_ciliopathy)[3] <- "OMIM<br>Phenotype Number"
    colnames(homosapiens_ciliopathy)[4] <- "Disease/Gene<br>Reference"
    colnames(homosapiens_ciliopathy)[5] <- "Human<br>Gene ID"
    colnames(homosapiens_ciliopathy)[6] <- "Human Gene<br>Name"
    colnames(homosapiens_ciliopathy)[8] <- "Subcellular<br>Localization"
    colnames(homosapiens_ciliopathy)[7] <- "Gene MIM<br>Number"
    colnames(homosapiens_ciliopathy)[9] <- "Localisation<br>Reference"}
    DT::datatable(homosapiens_ciliopathy, escape = FALSE,extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', 
                                 initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                 server = FALSE,
                                 autoWidth = FALSE,
                                 searchHighlight = TRUE,
                                 columnDefs = list(list(width = 'fit-content', targets = c(4,6,8), className="dt-center"),
                                                   list(width = 'fit-content', targets = c(5), className="dt-center"),
                                                   list(width = 'fit-content', targets = c(3,9), className="dt-center"),
                                                   list(width = '3%', targets = c(2), className="dt-center"),
                                                   list(visible=FALSE, targets = c(10,11)),
                                                   list(width = 'fit-content',targets = c(7), className="dt-center"),
                                                   list(width = '15%', targets = c(1), className="dt-left")),
                                 lengthMenu = c(10,25,50,100,nrow(homosapiens_ciliopathy)),
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  }
  
  #Ortholog Human Celegans Table
  {
  output$ortholog_human_celegans <- DT::renderDataTable({
    ortholog_human_celegans$'Wormbase ID' <- paste0("<a href= '","https://wormbase.org/search/gene/*",ortholog_human_celegans$'Wormbase ID',"' target='_blank '>",ortholog_human_celegans$'Wormbase ID',"</a>")
    ortholog_human_celegans$'C. elegans Gene Name' <- paste0("<a href= '"," https://www.ncbi.nlm.nih.gov/gene/?term=Caenorhabditis+elegans ",ortholog_human_celegans$'C. elegans Gene Name',"' target='_blank '>",ortholog_human_celegans$'C. elegans Gene Name',"</a>")
    ortholog_human_celegans <- reference_sorter(ortholog_human_celegans)
    {colnames(ortholog_human_celegans)[3] <- "OMIM<br>Phenotype Number"
    colnames(ortholog_human_celegans)[4] <- "Disease/Gene<br>Reference"
    colnames(ortholog_human_celegans)[6] <- "C. elegans<br>Sequence ID"
    colnames(ortholog_human_celegans)[7] <- "C. elegans<br>Gene Name"
    colnames(ortholog_human_celegans)[8] <- "Human<br>Gene ID"
    colnames(ortholog_human_celegans)[9] <- "Human Gene<br>Name"
    colnames(ortholog_human_celegans)[11] <- "Subcellular<br>Localization"
    colnames(ortholog_human_celegans)[10] <- "Gene MIM<br>Number"
    colnames(ortholog_human_celegans)[12] <- "Localisation<br>Reference"
    }
    DT::datatable(ortholog_human_celegans, escape = FALSE,extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',  
                                 initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                 server = FALSE,
                                 autoWidth = TRUE,
                                 searchHighlight = TRUE,
                                 columnDefs = list(list(targets = 1, className = "dt-left"),
                                                   list(targets = c(2:12), className="dt-center"),
                                                   list(targets = c(13,14),visible=FALSE)),
                                 lengthMenu = c(10,25,50,100,nrow(ortholog_human_celegans)),
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })}
  
  #Ortholog Human Mmusculus Table
  {  output$ortholog_human_mmusculus <- DT::renderDataTable({
    ortholog_human_mmusculus$'Mus musculus Gene Name' <- paste0("<a href= '"," https://www.ncbi.nlm.nih.gov/gene/?term=Mus+musculus ",ortholog_human_mmusculus$'Mus musculus Gene Name',"' target='_blank '>",ortholog_human_mmusculus$'Mus musculus Gene Name',"</a>")
    ortholog_human_mmusculus <- reference_sorter(ortholog_human_mmusculus)
    {colnames(ortholog_human_mmusculus)[3] <- "OMIM<br>Phenotype Number"
      colnames(ortholog_human_mmusculus)[4] <- "Disease/Gene<br>Reference"
      colnames(ortholog_human_mmusculus)[5] <- "Human<br>Gene ID"
      colnames(ortholog_human_mmusculus)[6] <- "Human Gene<br>Name"
      colnames(ortholog_human_mmusculus)[7] <- "Gene MIM<br>Number"
      colnames(ortholog_human_mmusculus)[8] <- "Mus musculus<br>Gene Name"
      colnames(ortholog_human_mmusculus)[9] <- "Subcellular<br>Localization"
      colnames(ortholog_human_mmusculus)[10] <- "Localisation<br>Reference"
    }
    DT::datatable(ortholog_human_mmusculus, escape = FALSE,extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', 
                                 server = FALSE,
                                 autoWidth = TRUE,
                                 initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                 searchHighlight = TRUE,
                                 columnDefs = list(list(targets = 1, className = "dt-left"),
                                                   list(targets = c(2:10), className="dt-center"),
                                                   list(visible = FALSE, targets = c(11,12))),
                                 lengthMenu = c(10,25,50,100,nrow(ortholog_human_mmusculus)),
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })}
  
  #Ortholog Human Drosophila Table
  {  output$ortholog_human_drosophila <- DT::renderDataTable({
    ortholog_human_drosophila$`Drosophila melanogaster Gene Name` <- paste0("<a href= '"," https://www.ncbi.nlm.nih.gov/gene/?term=Drosophila+melanogaster "
                                                                            ,ortholog_human_drosophila$`Drosophila melanogaster Gene Name`,"' target='_blank '>",ortholog_human_drosophila$`Drosophila melanogaster Gene Name`,"</a>")
    ortholog_human_drosophila <- reference_sorter(ortholog_human_drosophila)
    {colnames(ortholog_human_drosophila)[3] <- "OMIM<br>Phenotype Number"
      colnames(ortholog_human_drosophila)[4] <- "Disease/Gene<br>Reference"
      colnames(ortholog_human_drosophila)[5] <- "Human<br>Gene ID"
      colnames(ortholog_human_drosophila)[6] <- "Human Gene<br>Name"
      colnames(ortholog_human_drosophila)[7] <- "Gene MIM<br>Number"
      colnames(ortholog_human_drosophila)[8] <- "D. melanogaster<br>Gene Name"
      colnames(ortholog_human_drosophila)[9] <- "Subcellular<br>Localization"
      colnames(ortholog_human_drosophila)[10] <- "Localisation<br>Reference"
    }
    DT::datatable(ortholog_human_drosophila, escape = FALSE,extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', 
                                 server = FALSE,
                                 autoWidth = TRUE,
                                 initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                 searchHighlight = TRUE,
                                 columnDefs = list(list(targets = 1, className = "dt-left"),
                                                   list(targets = c(11,12), visible = FALSE),
                                                   list(targets = c(2:10), className="dt-center")),
                                 lengthMenu = c(10,25,50,100,nrow(ortholog_human_drosophila)),
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

  })}
  
  #Ortholog Human Xlaevis Table
  {
   output$ortholog_human_xlaevis <- DT::renderDataTable({
    ortholog_human_xlaevis$'Xenopus laevis Gene Name' <- paste0("<a href= '"," https://www.ncbi.nlm.nih.gov/gene/?term=Xenopus+laevis ",ortholog_human_xlaevis$'Xenopus laevis Gene Name',"' target='_blank '>",ortholog_human_xlaevis$'Xenopus laevis Gene Name',"</a>")
    ortholog_human_xlaevis <- reference_sorter(ortholog_human_xlaevis)
    {colnames(ortholog_human_xlaevis)[3] <- "OMIM<br>Phenotype Number"
      colnames(ortholog_human_xlaevis)[4] <- "Disease/Gene<br>Reference"
      colnames(ortholog_human_xlaevis)[5] <- "Human<br>Gene ID"
      colnames(ortholog_human_xlaevis)[6] <- "Human Gene<br>Name"
      colnames(ortholog_human_xlaevis)[7] <- "Gene MIM<br>Number"
      colnames(ortholog_human_xlaevis)[8] <- "Xenopus laevis<br>Gene Name"
      colnames(ortholog_human_xlaevis)[9] <- "Subcellular<br>Localization"
      colnames(ortholog_human_xlaevis)[10] <- "Localisation<br>Reference"
    }
    DT::datatable(ortholog_human_xlaevis, escape = FALSE,extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', #Hepsinin bir anlamı var L ekleyince length geldi. 
                                 server = FALSE,
                                 autoWidth = TRUE,
                                 initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                 searchHighlight = TRUE,
                                 columnDefs = list(list(targets = 1, className = "dt-left"),
                                                   list(targets = c(2:10), className="dt-center"),
                                                   list(visible = FALSE, targets = c(11,12))),
                                 lengthMenu = c(10,25,50,100,nrow(ortholog_human_xlaevis)),
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })}
  
  #Ortholog Human Drerio Table
  {
  output$ortholog_human_drerio <- DT::renderDataTable({
    ortholog_human_drerio$'Danio rerio Gene Name' <- paste0("<a href= '"," https://www.ncbi.nlm.nih.gov/gene/?term=Danio+rerio ",ortholog_human_drerio$'Danio rerio Gene Name',"' target='_blank '>",ortholog_human_drerio$'Danio rerio Gene Name',"</a>")
    ortholog_human_drerio <- reference_sorter(ortholog_human_drerio)
    {colnames(ortholog_human_drerio)[3] <- "OMIM<br>Phenotype Number"
      colnames(ortholog_human_drerio)[4] <- "Disease/Gene<br>Reference"
      colnames(ortholog_human_drerio)[5] <- "Human<br>Gene ID"
      colnames(ortholog_human_drerio)[6] <- "Human Gene<br>Name"
      colnames(ortholog_human_drerio)[7] <- "Gene MIM<br>Number"
      colnames(ortholog_human_drerio)[8] <- "Danio rerio<br>Gene Name"
      colnames(ortholog_human_drerio)[9] <- "Subcellular<br>Localization"
      colnames(ortholog_human_drerio)[10] <- "Localisation<br>Reference"
    }
    DT::datatable(ortholog_human_drerio, escape = FALSE,extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', 
                                 server = FALSE,
                                 autoWidth = TRUE,
                                 initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                 searchHighlight = TRUE,
                                 columnDefs = list(list(targets = 1, className = "dt-left"),
                                                   list(targets = c(2:10), className="dt-center"),
                                                   list(visible = FALSE, targets = c(11,12))),
                                 lengthMenu = c(10,25,50,100,nrow(ortholog_human_drerio)),
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })}
    #Ortholog Human Chlamydomonas reinhardtiis Table
    {  output$ortholog_human_creinhardtii <- DT::renderDataTable({
      ortholog_human_creinhardtii$`C. reinhardtii Gene Name` <- paste0("<a href= '"," https://www.ncbi.nlm.nih.gov/gene/?term=Chlamydomonas+reinhardtiis "
                                                                       ,ortholog_human_creinhardtii$`C. reinhardtii Gene Name`,"' target='_blank '>",ortholog_human_creinhardtii$`C. reinhardtii Gene Name`,"</a>")
      ortholog_human_creinhardtii <- reference_sorter(ortholog_human_creinhardtii)
      {colnames(ortholog_human_creinhardtii)[3] <- "OMIM<br>Phenotype Number"
        colnames(ortholog_human_creinhardtii)[4] <- "Disease/Gene<br>Reference"
        colnames(ortholog_human_creinhardtii)[5] <- "Human<br>Gene ID"
        colnames(ortholog_human_creinhardtii)[6] <- "Human Gene<br>Name"
        colnames(ortholog_human_creinhardtii)[7] <- "Gene MIM<br>Number"
        colnames(ortholog_human_creinhardtii)[8] <- "C. reinhardtii<br>Gene Name"
        colnames(ortholog_human_creinhardtii)[9] <- "Subcellular<br>Localization"
        colnames(ortholog_human_creinhardtii)[10] <- "Localisation<br>Reference"
      }
      DT::datatable(ortholog_human_creinhardtii, escape = FALSE,extensions = 'Buttons',
                    options = list(dom = 'Blfrtip', 
                                   server = FALSE,
                                   autoWidth = TRUE,
                                   initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                   searchHighlight = TRUE,
                                   columnDefs = list(list(targets = 1, className = "dt-left"),
                                                     list(targets = c(11,12), visible = FALSE),
                                                     list(targets = c(2:10), className="dt-center")),
                                   lengthMenu = c(10,25,50,100,nrow(ortholog_human_creinhardtii)),
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
      
    })}

  }
  

  
  
  
  
  
  #Symptomes and Disease Page 
  {
    {xs <- reactive({ #Reactive element for detection user input
      xa <- symptom_detector(input$symptome)})
    observe({updateSelectInput(session, "symptome2",
                               label = paste("Select Ciliopathy"),
                               choices = c("All",unique(xs()[,1])),
                               selected = "All")})
    
    #Symptomes Table
    output$primary_symp <- DT::renderDataTable({
      if(input$search_option == "disease_bsd"){
        shinyjs::disable("symptome")
        shinyjs::disable("symptome2")
        shinyjs::disable("disp")
        shinyjs::enable("disease_bsd_search")
        symptom_result <- disease_symptom(input$disease_bsd_search)
        DT::datatable(symptom_result,escape = FALSE,extensions = 'Buttons',
                      options = list(dom = 'Blfrtip', 
                                     initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                     server = FALSE, 
                                     autoWidth = TRUE,
                                     searchHighlight = TRUE,
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
        
      }
      else if (input$search_option == "symptom_bsd"){
        shinyjs::enable("symptome")
        shinyjs::enable("symptome2")
        shinyjs::enable("disp")
        shinyjs::disable("disease_bsd_search")
        primary_symp2 <- symptom_detector(input$symptome)
        primary_symp2 <- reference_sorter(primary_symp2)
        {colnames(primary_symp2)[3] <- "OMIM<br>Phenotype Number"
          colnames(primary_symp2)[4] <- "Disease/Gene<br>Reference"
          colnames(primary_symp2)[5] <- "Human<br>Gene ID"
          colnames(primary_symp2)[6] <- "Human Gene<br>Name"
          colnames(primary_symp2)[7] <- "Gene MIM<br>Number"
          colnames(primary_symp2)[8] <- "Subcellular<br>Localization"
          colnames(primary_symp2)[9] <- "Localisation<br>Reference"
        }
        row.names(primary_symp2) <- NULL
        #Status of Table Display Type
        if (input$disp == "norm"){
          observeEvent(input$disp, {
            shinyjs::enable("symptome2")
          })
          if (input$symptome2 != "All"){
            primary_symp1 <- which(primary_symp2$Ciliopathy == as.character(input$symptome2))
            primary_symp2 <- primary_symp2[primary_symp1,]
            row.names(primary_symp2) <- NULL
            {colnames(primary_symp2)[3] <- "OMIM<br>Phenotype Number"
              colnames(primary_symp2)[4] <- "Disease/Gene<br>Reference"
              colnames(primary_symp2)[5] <- "Human<br>Gene ID"
              colnames(primary_symp2)[6] <- "Human Gene<br>Name"
              colnames(primary_symp2)[7] <- "Gene MIM<br>Number"
              colnames(primary_symp2)[8] <- "Subcellular<br>Localization"
              colnames(primary_symp2)[9] <- "Localisation<br>Reference"
            }
          }}
        else if (input$disp == "name"){
          observeEvent(input$disp, {
            shinyjs::disable("symptome2")
          })
          
          primary_symp2 <- data.frame(Ciliopathy = unique(primary_symp2$Ciliopathy))
          row.names(primary_symp2) <- NULL
        }
        
        ifelse((nrow(primary_symp2) > 100),lengthMenus <- c(10,25,50,100,nrow(primary_symp2)),lengthMenus <- c(10,25,50,100))
        ifelse((ncol(primary_symp2) > 9),columnDef <- list(list(targets = c(2,3,4,5,6,7,8,9),className = "dt-center"),
                                                           list(targets = 1,className = "dt-left"),
                                                           list(visible = FALSE, targets = c(10,11))), 
               columnDef<- list(list(targets = "_all", className="dt-center")))
        
        DT::datatable(primary_symp2,escape = FALSE,extensions = 'Buttons',
                      options = list(dom = 'Blfrtip', 
                                     initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}"),
                                     server = FALSE, 
                                     autoWidth = TRUE,
                                     searchHighlight = TRUE,
                                     columnDefs = columnDef,
                                     lengthMenu = lengthMenus,
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
      }
      
    })}
    
  }
  
  
#Motile Ciliopathies
  output$motile_table <- DT::renderDataTable({
    motile <- reference_sorter(motile)
    #colnames visualization
    { colnames(motile)[3] <- "OMIM<br>Phenotype Number"
      colnames(motile)[4] <- "Disease/Gene<br>Reference"
      colnames(motile)[5] <- "Human<br>Gene ID"
      colnames(motile)[6] <- "Human Gene<br>Name"
      colnames(motile)[8] <- "Subcellular<br>Localization"
      colnames(motile)[7] <- "Gene MIM<br>Number"
      colnames(motile)[9] <- "Localisation<br>Reference"
      colnames(motile)[1] <- "Ciliopathy"}
    if(input$name_motile =="All Motile Ciliopathy"){
      row.names(motile) <- NULL
      DT::datatable(motile,escape = FALSE,extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   autoWidth = TRUE,
                                   searchHighlight = TRUE,
                                   columnDefs = list(list(targets = 1, className = "dt-left"),
                                                     list(width = 'fit-content', targets = c(2:9), className="dt-center"),
                                                     list(targets = c(10,11),visible = FALSE)),
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    }else{
      motile <- subset(motile, motile$Ciliopathy == input$name_motile)
      row.names(motile) <- NULL
      DT::datatable(motile,escape = FALSE,extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   autoWidth = TRUE,
                                   searchHighlight = TRUE,
                                   lengthMenu = c(10,20,50,100,nrow(secondarylist)),
                                   columnDefs = list(list(targets = 1, className = "dt-left"),
                                                     list(width = 'fit-content', targets = c(2:9), className="dt-center"),
                                                     list(targets = c(10,11),visible = FALSE)),
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    }})
  
  #Motile Heatmap
  output$heatmap_motile <- renderPlotly({
    motile_heatmap1 <- pure_ciliopathy_features[,c("Ciliopathy / Clinical Features","PCD")]
    motile_heatmap2 <- secondary_ciliopathy_features[,c("Ciliopathy / Clinical Features","BHD","EJM")]
    motile_heatmap <- merge(motile_heatmap1,motile_heatmap2,by="Ciliopathy / Clinical Features",all = TRUE)
    motile_heatmap[is.na(motile_heatmap)] <- 0
    labrow = motile_heatmap$`Ciliopathy / Clinical Features`
    motile_heatmap <- motile_heatmap[-1]
    heatmaply(motile_heatmap, 
              seriate = "GW", 
              colors = OrRd,
              main="Clinical Features",
              xlab = "Ciliopathy Types",
              ylab = "Clinical Symptomes",
              fontsize_row = 10,
              fontsize_col = 10,
              dendrogram = "none",
              plot_method= c("plotly"),
              hide_colorbar = TRUE,
              Colv=TRUE,
              Rowv = TRUE,
              dist_method = "euclidean",
              labRow = labrow, 
              label_names =  c("Clinical Feature", "Ciliopathy", "Value"),
              row_dend_left = FALSE,
              showticklabels = c(TRUE, FALSE))})
  #Symptom Summary Figures
  {
    output$Aural2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Aural Anomalies`[1]
      }
      h4(popify(el = icon(name = "assistive-listening-systems","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Aural Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$neural2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Neural Anomalies`[1]
        
      }
      h4(popify(el = icon(name = "brain","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Neural Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$ophthalmic2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Ophthalmic Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "eye","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Ophthalmic Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$skeletal2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Skeletal Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "bone","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Skeletal Anomalies"
                ),placement="left",options = list(container = "body")))})
    
    output$Other2 <-renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$Others
      }
      h4(popify(el = icon(name = "plus","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Other Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$hormonal2 <-renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Hormonal Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "balance-scale-left","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Hormonal Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$respiratory2 <-renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Respiratory Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "lungs","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Respiratory Anomalies"
                ),placement="top",options = list(container = "body")))})
    
    output$Reproductive2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Reproductive Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "venus-mars","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Reproductive Anomalies"
                ),placement="top",options = list(container = "body")))})
    output$Facial2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Facial Anomalies`[1]
      }
      
      h4(popify(el = icon(name = "grin","fa-3x",style = paste0("color:",color,";"), lib = "font-awesome"), title = "",
                content = paste0("Facial Anomalies"
                ),placement="left",options = list(container = "body")))})
    
    output$cerebral2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Cerebral Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-neurology`,scale = 3, fill = color))
    })
    output$Renal2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Renal Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-kidney`,scale = 3, fill = color))
    })
    output$Coronary2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Coronary and Vascular Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-cardiology`,scale = 3, fill = color))
    })
    output$Nasal2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Nasal Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-nose`,scale = 3, fill = color)) })
    output$liver2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Liver Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-liver`,scale = 3, fill = color))
    })
    output$Digestive2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$'Digestive Anomalies'[1]
      }
      
      h4(icon_style(icons$`i-stomach`,scale = 3, fill = color))
    })
    output$Organ2 <- renderUI({
      if(input$name_motile == "All Motile Ciliopathy"){color <- "#F1C40F" }
      else {
        color_table <- color_change(input$name_motile,TRUE)
        color <- color_table$`Organ Anomalies`[1]
      }
      
      h4(icon_style(icons$`i-internal-medicine`,scale = 3, fill = color))
    })
    
  }
  

}