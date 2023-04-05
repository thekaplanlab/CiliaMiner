
ui <- 
  
  navbarPage("CiliaMiner", collapsible = TRUE, inverse = TRUE, theme = shinytheme("simplex"),
             #Home page
             {tabPanel("Homepage",
                       tags$head(includeHTML(("google-analytics/google-analytics.html"))),
                       tags$head(
                         tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
                         tags$link(rel = "shortcut icon", href = "favicon.png"),
                         tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png"),
                         tags$style(HTML(".navbar {font-size:large;")),
                         tags$style(HTML(".navbar .navbar-nav {float:left; margin: 0; display:flex; width: 90%; justify-content: space-between;   }   ")),
                         tags$style(HTML(".navbar-brand {font-size: large;"))),
                       fixedRow(
                         column(6,
                                p(strong("CiliaMiner"), style = "font-size:50px; color: #FF4500"),
                                headerPanel(h1("",align = "center")),
                                  tags$head(tags$style("#search_table table {background-color: #74b3ce; }", media="screen", type="text/css")),
                                  tags$head(tags$style("#search_cilia_table table {background-color: #74b3ce ; }", media="screen", type="text/css")),
                                  tabsetPanel(type= "pills",
                                              
                                              #Gene Search Input
                                              tabPanel("Gene Search",column(11,align="left",
                                                                            #Search Box
                                                                            searchInput(inputId = "search",label = "",
                                                                                                        placeholder = "Search by Human Gene Name and Gene ID",
                                                                                                        btnSearch = icon("search"),
                                                                                                        btnReset = icon("remove",verify_fa = FALSE),
                                                                                                        width = "90%",),
                                                                            tags$style(type="text/css", "#search_text {font-size: 160%;height:50px}"),
                                                                            tags$style(type="text/css", "#search_search {font-size: x-large;height:50px}"),
                                                                            tags$style(type="text/css", "#search_reset {font-size: x-large;height:50px}"),
                                                                            tags$style(type="text/css", ".nav-pills {font-size: x-large;}"),
                                                                            HTML(rep("<br/><br/>", 1)),
                                                                            #Gene Search Result Table
                                                                            fixedRow(box(width = 12,background = "orange",status = 'primary',align = "left",
                                                                                         dataTableOutput('search_table'),br(),style = "font-size:100%; width:100%")),)),
                                              #Ciliopathy Search Input
                                              tabPanel("Ciliopathy Search",column(12, align = "left",
                                                                                  #Search Box
                                                                                  autocomplete_input(id = "disease_search", label = "", unique(homosapiens_ciliopathy$Ciliopathy),placeholder = "Search by Disease Name",
                                                                                                     create = TRUE,max_options = 3,hide_values = FALSE,width = "80%"),
                                                                                  tags$style(type="text/css", "#disease_search {font-size: x-large;height:50px}"),
                                                                                  #Popover for Search Box
                                                                                  bsPopover(id = "disease_search", content = "Select a ciliopathy type", title = "", placement = "right",trigger = "hover",
                                                                                            options = list(container = "body")),
                                                                                  HTML(rep("<br/><br/>", 1)),
                                                                                  #Ciliopathy Search Result Table
                                                                                  fixedRow(column(width = 12,align = "left",dataTableOutput('search_cilia_table'),br(),style = "font-size:100%; width:100%")),),
                                                       
                                              )
                                ),
                         ),
                         #Homepage graphs are bubble, piechart, and barplot respectively.  
                         column(6,align = "center",verticalLayout(HTML(rep("<br/><br/><br/><br/>", 1)),shinycssloaders::withSpinner(plotlyOutput("bubble",height = "100%",width = "100%"),type = 5,color = "#C81E1E") ,br(),
                                                                 plotlyOutput("piechart",height = "50%"),br(),
                                                                 plotlyOutput("barplot",height = "50%")),
                                  
                                
                                ),
                         ))},
                      
             #Ciliopathy Names Page
             {tabPanel("Ciliopathy Names",
                      fluidPage(
                        tabsetPanel(
                          #Pure Ciliopathies Panel
                          
                          {tabPanel("Primary Ciliopathies",
                                    #Tags for tooltip
                                   tags$head(tags$style(HTML("#Digestive .tooltip {width: 300px;}"))),
                                   tags$head(tags$style(HTML( ".dropdown-toggle .css {background-color: red; }" ))),
                                   br(),
                                   p("This website contains a list of human genes for primary ciliopathies. The localization of protein products of disease-associated genes, articles, and disease/gene-related extensions are give"),
                                   br(),
                                   #sidebar
                                   sidebarLayout(
                                     sidebarPanel(
                                       #Pure Ciliopathy input
                                       pickerInput(inputId = "namepures",label = "Choose a pure ciliopathy",
                                                   choices = c("All Pure Ciliopathy",unique(purelist$Ciliopathy)),options = list('live-search' = TRUE,size = 7,'style' = "btn-link")),
                                       #Heatmap input 
                                       multiInput(
                                         inputId = "heatmaps", label = "Choose a Pure Ciliopathy for Heatmap:",
                                         choices = unique(purelist$Ciliopathy),
                                         selected = "Standart"),
                                       actionButton("all", "All"),
                                       actionButton("none", "Clear"),
                                       div(style = "margin-top: 30px"),
                                       p(strong("    Disease Symptom Summary"),style = "font-size: 200%;text-align:center;color:#bd552e;"),
                                       br(),
                                       #Disease Symptom Summary Figures.
                                       splitLayout(htmlOutput("Aural",align = "center"),htmlOutput("neural",align = "center"),htmlOutput("ophthalmic",align = "center"),htmlOutput("skeletal",align = "center")),
                                       splitLayout(htmlOutput("respiratory",align = "center"),htmlOutput("hormonal", align = "center"),htmlOutput("Reproductive",align = "center"),htmlOutput("Facial",align = 'center')),
                                       splitLayout(htmlOutput("cerebral",style = "text-align:center;width:initial"),htmlOutput("Renal",style = "text-align:center;width:initial"),htmlOutput("Coronary",style = "text-align:center;width:initial"),htmlOutput("Nasal",style = "text-align:center;width:initial")),
                                       splitLayout(htmlOutput("liver",style = "text-align:center;width:initial"),htmlOutput("Cognitive",style = "text-align:center;width:initial"),htmlOutput("Digestive",style = "text-align:center;width:initial"),htmlOutput("Organ",style = "text-align:center;width:initial")),
                                       #Disease Symptom Summary Figures' Popovers
                                       bsPopover(id = "cerebral", content = "Cerabral Anomalies", title = "", placement = "top",trigger = "hover"),
                                       bsPopover(id = "Renal", content = "Renal Anomalies", title = "", placement = "top",trigger = "hover"),
                                       bsPopover(id = "Coronary", content = "Coronary and Vascular Anomalies", title = "", placement = "top",trigger = "hover"),
                                       bsPopover(id = "Nasal", content = "Nasal Anomalies", title = "", placement = "left",trigger = "hover"),
                                       bsPopover(id = "liver", content = "Liver Anomalies", title = "", placement = "top",trigger = "hover"),
                                       bsPopover(id = "Cognitive", content = "Cognitive Anomalies", title ="", placement = "top", trigger = "hover"),
                                       bsPopover(id = "Digestive", content = "Digestive Anomalies", title ="", placement = "top", trigger = "hover"),
                                       bsPopover(id = "Organ", content = "Organ Anomalies", title ="", placement = "left", trigger = "hover"),
                                       width =3),
                                     mainPanel(
                                       #Result Table output
                                       fluidRow(column(6,dataTableOutput('namepuresout'),h5("*: Uncertain localisation data from related paper"),style = "font-size:100%; width:100%")),
                                       br(),
                                       p("The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                       #Heatmap Output
                                       shinycssloaders::withSpinner(plotlyOutput('namepuresout2'),type = 5,color = "#C81E1E"),
                                       br(),
                                       br(),
                                       br(),
                                       #Explanation of the represented diseases
                                       textOutput("outputtext"),
                                     ),
                                     position = c("left"),
                                     fluid = TRUE
                                   ))},
                          
                          #Secondary Ciliopathies Panel
                          {tabPanel("Secondary Diseases",
                                   br(),
                                   p("This website contains a list of human genes for secondary diseases. The localization of protein products of disease-associated genes, articles, and disease/gene-related extensions are give"),
                                   br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       #Secondary Ciliopathy input
                                       pickerInput(inputId = "namesecondary",label = "Choose a secondary ciliopathy",
                                                   choices = c("All Secondary Ciliopathy",unique(secondarylist$Ciliopathy)),options = list('live-search' = TRUE,size = 7,'style' = "btn-link"),
                                                   choicesOpt = list( content = stringr::str_trunc(c("All Secondary Ciliopathy",unique(secondarylist$Ciliopathy)), width = 70)
                                       )),
                                       #Disease heatmap input
                                       multiInput(
                                         inputId = "heatmaps2", label = "Choose Secondary Ciliopathy for Heatmap:",
                                         choices = unique(secondarylist$Ciliopathy),
                                         selected = "Standart"),
                                       actionButton("all2", "All"),
                                       actionButton("none2", "Clear"),
                                       div(style = "margin-top: 30px"),
                                       p(strong("    Disease Symptom Summary"),style = "font-size: 200%;text-align:center;color:#bd552e;"),
                                       br(),
                                       #Disease Symptom Summary Figures.
                                       splitLayout(htmlOutput("Aural1",align = "center"),htmlOutput("neural1",align = "center"),htmlOutput("ophthalmic1",align = "center"),htmlOutput("skeletal1",align = "center")),
                                       splitLayout(htmlOutput("respiratory1",align = "center"),htmlOutput("hormonal1", align = "center"),htmlOutput("Reproductive1",align = "center"),htmlOutput("Facial1",align = 'center')),
                                       splitLayout(htmlOutput("cerebral1",style = "text-align:center;width:initial"),htmlOutput("Renal1",style = "text-align:center;width:initial"),htmlOutput("Coronary1",style = "text-align:center;width:initial"),htmlOutput("Nasal1",style = "text-align:center;width:initial")),
                                       splitLayout(htmlOutput("liver1",style = "text-align:center;width:initial"),htmlOutput("Other",style = "text-align:center;width:initial"),htmlOutput("Digestive1",style = "text-align:center;width:initial"),htmlOutput("Organ1",style = "text-align:center;width:initial")),
                                       #Disease Symptom Summary Figures' Popovers
                                       bsPopover(id = "cerebral1", content = "Cerabral Anomalies", title = "", placement = "top",trigger = "hover"),
                                       bsPopover(id = "Renal1", content = "Renal Anomalies", title = "", placement = "top",trigger = "hover"),
                                       bsPopover(id = "Coronary1", content = "Coronary and Vascular Anomalies", title = "", placement = "top",trigger = "hover"),
                                       bsPopover(id = "Nasal1", content = "Nasal Anomalies", title = "", placement = "left",trigger = "hover"),
                                       bsPopover(id = "liver1", content = "Liver Anomalies", title = "", placement = "top",trigger = "hover"),
                                       bsPopover(id = "Digestive1", content = "Digestive Anomalies", title ="", placement = "top", trigger = "hover"),
                                       bsPopover(id = "Organ1", content = "Organ Anomalies", title ="", placement = "left", trigger = "hover"),
                                       width =3),
                                     #Secondary Ciliopathies Table Output
                                     mainPanel(
                                       fluidRow(column(7,dataTableOutput('namesecondsout'),style = "font-size:100%; width:100%")),
                                       br(),
                                       p("The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                       shinycssloaders::withSpinner(plotlyOutput('namesecondsout2'),type = 5,color = "#C81E1E"),
                                       br(),
                                       br(),
                                       br(),
                                       #Explanation of the represented diseases
                                       textOutput("outputtext2"),
                                     ),
                                     position = c("left"),
                                     fluid = TRUE
                                   ))},
                          #Motile Ciliopathies Panel
                          {tabPanel("Motile Ciliopathies",
                                    br(),
                                    p("This website lists human genes of motile ciliopathies. The localization of protein products of disease-associated genes, articles, and disease/gene-related extensions are given."),
                                    br(),
                                    sidebarLayout(
                                      sidebarPanel(
                                        #Motile Ciliopathy input
                                        pickerInput(inputId = "name_motile",label = "Choose a secondary ciliopathy",
                                                    choices = c("All Motile Ciliopathy",unique(motile$Ciliopathy)),options = list('live-search' = TRUE,size = 7,'style' = "btn-link"),
                                                    choicesOpt = list( content = stringr::str_trunc(c("All Motile Ciliopathy",unique(motile$Ciliopathy)), width = 70))),
                                        br(),
                                        p(strong("    Disease Symptom Summary"),style = "font-size: 200%;text-align:center;color:#bd552e;"),
                                        #Disease Symptom Summary Figures.
                                        splitLayout(htmlOutput("Aural2",align = "center"),htmlOutput("neural2",align = "center"),htmlOutput("ophthalmic2",align = "center"),htmlOutput("skeletal2",align = "center")),
                                        splitLayout(htmlOutput("respiratory2",align = "center"),htmlOutput("hormonal2", align = "center"),htmlOutput("Reproductive2",align = "center"),htmlOutput("Facial2",align = 'center')),
                                        splitLayout(htmlOutput("cerebral2",style = "text-align:center;width:initial"),htmlOutput("Renal2",style = "text-align:center;width:initial"),htmlOutput("Coronary2",style = "text-align:center;width:initial"),htmlOutput("Nasal2",style = "text-align:center;width:initial")),
                                        splitLayout(htmlOutput("liver2",style = "text-align:center;width:initial"),htmlOutput("Other2",style = "text-align:center;width:initial"),htmlOutput("Digestive2",style = "text-align:center;width:initial"),htmlOutput("Organ2",style = "text-align:center;width:initial")),
                                        #Disease Symptom Summary Figures' Popovers
                                        bsPopover(id = "cerebral2", content = "Cerabral Anomalies", title = "", placement = "top",trigger = "hover"),
                                        bsPopover(id = "Renal2", content = "Renal Anomalies", title = "", placement = "top",trigger = "hover"),
                                        bsPopover(id = "Coronary2", content = "Coronary and Vascular Anomalies", title = "", placement = "top",trigger = "hover"),
                                        bsPopover(id = "Nasal2", content = "Nasal Anomalies", title = "", placement = "left",trigger = "hover"),
                                        bsPopover(id = "liver2", content = "Liver Anomalies", title = "", placement = "top",trigger = "hover"),
                                        bsPopover(id = "Digestive2", content = "Digestive Anomalies", title ="", placement = "top", trigger = "hover"),
                                        bsPopover(id = "Organ2", content = "Organ Anomalies", title ="", placement = "left", trigger = "hover"),
                                        width = 3),
                                      mainPanel(
                                        fluidRow(column(7,shinycssloaders::withSpinner(dataTableOutput('motile_table'),type = 5,color = "#C81E1E"),
                                                        style = "font-size:100%; width:100%")),
                                        shinycssloaders::withSpinner(plotlyOutput('heatmap_motile'),type = 5,color = "#C81E1E"),
                                      ),position = c("left"),
                                      fluid = TRUE ))},
                          
                          #Atypical Ciliopathies Panel
                          {tabPanel("Atypical Ciliopathies",
                          br(),
                          p("Unclassified ciliopathy disease-related genes have been collected on this page using the search term 'ciliopathy'."),
                          br(),
                          #Atypical Ciliopathies Table Output
                          mainPanel(fluidRow(column(6,dataTableOutput('atypical_ciliopathy'),style = "font-size:100%; width:150%"))))},
                          
                          #Potential Ciliopathies Panel
                          tabPanel("Potential Ciliopathy Genes",
                          br(),
                          p("The list of candidate ciliopathy genes in this subtab includes genes that are primarily found in the cilia as well as genes that are associated with the formation and maintenance of cilia."),
                          br(),
                          #Potential Ciliopathies Table and Heatmap output
                          mainPanel(splitLayout(dataTableOutput('potential_ciliopathy_genes'),p(),
                                                                   plotlyOutput("potential_ciliopathy_genes_heatmap",height = '600%'),cellWidths = c("75%","5%","73%"),
                                                cellArgs = list(style = "padding:10px")
                            
                          ),h5("*: Uncertain ciliary process from related paper")
                                           )
                            
                          
                          ))))},
             #Gene Search Page
             {
               tabPanel("Gene Search"
                        
                        
                        
                        
                        
                        )
             },

             #Ciliopathy Gene Orthologs Page
             {
             tabPanel("Ciliopathy Genes and Orthologs",
                      br(),
                      tabPanel("Organisms"),
                      fluidPage(
                        tabsetPanel(
                          #Homo sapiens Genes and Orthologs
                          tabPanel(em("Homo sapiens"),
                                   br(),
                                   p("This page lists all human genes based on both pure and secondary ciliopaties with localisation referances.
                                      The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                   br(),
                                   fluidPage(
                                     column(6,
                                            #Homo sapiens Table Output
                                            div(DT::dataTableOutput("homosapiens_ciliopathy")),h5("*: Uncertain localisation data from related paper"),
                                            style = "font-size:100%; width:100%"))),
                          #Mus musculus Genes and Orthologs
                          tabPanel(em("Mus musculus"),
                                   br(),
                                   p("This page lists small house mouse ciliopathy genes which are orthologs with human genes.
                                      The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                   br(),
                                   fluidPage(
                                     column(6,
                                            #Mus musculus Table Output
                                            div(DT::dataTableOutput("ortholog_human_mmusculus")),h5("*: Uncertain localisation data from related paper"),
                                            style = "font-size:100%; width:100%"))),
                          #Danio rerio Genes and Orthologs
                          tabPanel(em("Danio rerio"),
                                   br(),
                                   p("This page lists zebrafish ciliopathy genes which are orthologs with human genes.
                                      The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                   br(),
                                   fluidPage(
                                     column(6,
                                            #Danio rerio Table Output
                                            div(DT::dataTableOutput("ortholog_human_drerio")),h5("*: Uncertain localisation data from related paper"),
                                            style = "font-size:100%; width:100%"))),
                          #Xenopus laevis Genes and Orthologs
                          tabPanel(em("Xenopus laevis"),
                                   br(),
                                   p("This page lists clawed frog ciliopathy genes which are orthologs with human genes.
                                      The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                   br(),
                                   fluidPage(
                                     column(6,
                                            #Xenopus laevis Table Output
                                            div(DT::dataTableOutput('ortholog_human_xlaevis')),h5("*: Uncertain localisation data from related paper"),
                                            style = "font-size:100%; width:100%")
                                   )),
                          #Drosophila melanogaster Genes and Orthologs
                          tabPanel(em("Drosophila melanogaster"),
                                   br(),
                                   p("This page lists fruit fly ciliopathy genes which are orthologs with human genes.
                                      The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                   br(),
                                   fluidPage(
                                     column(6,
                                            #Drosophila melanogaster Table Output
                                            div(DT::dataTableOutput("ortholog_human_drosophila")),h5("*: Uncertain localisation data from related paper"),
                                            style = "font-size:100%; width:100%"))),
                          #Caenorhabditis elegans Genes and Orthologs
                          tabPanel(em("Caenorhabditis elegans"),
                                   br(),
                                   p("This page lists nematode worm ciliopathy genes which are orthologs with human genes.
                                      The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                   br(),
                                   fluidPage(
                                     column(6,
                                            #Caenorhabditis elegans Table Output
                                            div(DT::dataTableOutput("ortholog_human_celegans")),
                                            style = "font-size:100%; width:100%"))),
                          #Chlamydomonas reinhardtii Genes and Orthologs
                          tabPanel(em("Chlamydomonas reinhardtii"),
                                   br(),
                                   p("This page lists unicellular green algea ciliopathy genes which are orthologs with human genes.
                                      The Disease/Gene Reference column in the table offers a PubMed ID relating to associated diseases and genes, whilst the ciliary localization column displays data from mammalian and worm studies."),
                                   br(),
                                   fluidPage(
                                     column(6,
                                            #Chlamydomonas reinhardtiis Table Output
                                            div(DT::dataTableOutput("ortholog_human_creinhardtii")),
                                            style = "font-size:100%; width:100%")))
                          )))},
             
             #Symptomes and Diseases Page
             {
             tabPanel("Symptoms and Diseases",
                      fluidPage(
                        shinyjs::useShinyjs(),
                        br(),
                        br(),
                        #Syptoms Select Box
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("search_option","Choose Search Option: ",
                                         c("Disease Based" = "disease_bsd",
                                           "Symptoms Based" = "symptom_bsd")),
                            selectInput("disease_bsd_search","Choose a Disease",
                                        choices = unique(list_Df$Ciliopathy)),
                            autocomplete_input(id = 'symptome', label = 'Search a symptom', placeholder = "Search by Symptom Name",
                                               unique(c(clinical_feature_pure$Clinical_features,clinical_feature_secondary$Clinical_features)),
                                               create = TRUE,max_options = 8,hide_values = FALSE,width = "100%"),
                            selectInput("symptome2",'Choose a ciliopathy',choices = NULL),
                            radioButtons("disp", "Display Type:",
                                         c("Normal" = "norm",
                                           "Just Name" = "name")),
                            width =3),
                          mainPanel(
                            fluidRow(column(6,div(DT::dataTableOutput("primary_symp")),style = "font-size:100%; width:100%"))
                          ),
                          position = c("left"),
                          fluid = TRUE
                        )))},
             
             #Submit Your Gene Page

             {
             tabPanel("Submit Your Gene",fluidPage(
               #Submit Gene Form
                      tags$iframe(src = 'https://forms.gle/knog1LnXVnDW57fL6',
                                  width = '50%',
                                  style="height: 100vh;",
                                  id = "google_form",
                                  frameborder = 0,
                                  marginheight = 0)),align = "center")
               },
             
             #About Page
             {
             tabPanel("About",
                      fluidPage(
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(strong("Coming soon..."), style = "font-size:50px; color:#FF4500",
                          align = "center")))})
