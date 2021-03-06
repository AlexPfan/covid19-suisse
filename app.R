library(shiny)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(sf)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(plotly)
library(shinythemes)

# CSV impots
coronaCH <- read.csv("0.CoronaCH.csv")
coronaCanton <- read.csv("0.CoronaCanton.csv")

#Spatial data
coronaSpatial <- read.csv("0.CoronaSpatial.csv")
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "ZH"] <- "1"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "BE"] <- "2"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "LU"] <- "3"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "UR"] <- "4"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "SZ"] <- "5"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "OW"] <- "6"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "NW"] <- "7"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "GL"] <- "8"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "ZG"] <- "9"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "FR"] <- "10"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "SO"] <- "11"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "BS"] <- "12"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "BL"] <- "13"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "SH"] <- "14"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "AI"] <- "15"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "AR"] <- "16"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "SG"] <- "17"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "GR"] <- "18"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "AG"] <- "19"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "TG"] <- "20"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "TI"] <- "21"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "VD"] <- "22"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "VS"] <- "23"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "NE"] <- "24"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "GE"] <- "25"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "JU"] <- "26"
names(coronaSpatial )[names(coronaSpatial ) == "KANTONSNUM"] <- "KTNR"

# read cantonal borders
canton_geo <- read_sf("map/g2k20.shp")
# read country borders
country_geo <- read_sf("map/g2l20.shp")
# read lakes
lake_geo <- read_sf("map/g2s20.shp")

#Data work
coronaSpatial$KTNR <- as.numeric(coronaSpatial$KTNR)
dat_merged <- merge(canton_geo, coronaSpatial, by="KTNR")
dat_merged$CasPositifs26_3 <- as.numeric(dat_merged$CasPositifs30_3)
dat_merged$CasPositifsParTete26_3 <- as.numeric(dat_merged$CasPositifsParTete30_3)

#HTML files
htmlPresentation1 <- "Presentation1.html"
htmlPresentation2 <- "Presentation2.html"
htmlCantons <- "DescriptionCantons.html"
htmlSuisse <- "DescriptionSuisse.html"

# Renaming columns in CoronaCanton dataset
names(coronaCanton)[names(coronaCanton) == "date"] <- "Date"
names(coronaCanton)[names(coronaCanton) == "canton"] <- "Canton"
names(coronaCanton)[names(coronaCanton) == "population"] <- "Population"
names(coronaCanton)[names(coronaCanton) == "O65_perc"] <- "Population > 65ans"
names(coronaCanton)[names(coronaCanton) == "beds"] <- "Lits d'hôpitaux"
names(coronaCanton)[names(coronaCanton) == "bedsPerCapita"] <- "Lits par habitant"
names(coronaCanton)[names(coronaCanton) == "tested_pos"] <- "Positifs"
names(coronaCanton)[names(coronaCanton) == "TotalConfCases"] <- "Confirmés"
names(coronaCanton)[names(coronaCanton) == "TotalDeaths"] <- "Total_Décès"
names(coronaCanton)[names(coronaCanton) == "TotalCured"] <- "Total Guéris"
names(coronaCanton)[names(coronaCanton) == "SortisDeHôpital"] <- "Sortis de l'hôpital"
names(coronaCanton)[names(coronaCanton) == "posPer100k"] <- "PositifsPar100000"

# Renaming columns in CoronaCH dataset
names(coronaCH)[names(coronaCH) == "date"] <- "Date"
names(coronaCH)[names(coronaCH) == "canton"] <- "Canton"
names(coronaCH)[names(coronaCH) == "population"] <- "Population"
names(coronaCH)[names(coronaCH) == "O65_perc"] <- "Population > 65ans"
names(coronaCH)[names(coronaCH) == "beds"] <- "Lits d'hôpitaux"
names(coronaCH)[names(coronaCH) == "bedsPerCapita"] <- "Lits par habitant"
names(coronaCH)[names(coronaCH) == "tested_pos"] <- "Positifs"
names(coronaCH)[names(coronaCH) == "TotalConfCases"] <- "Confirmés"
names(coronaCH)[names(coronaCH) == "TotalDeaths"] <- "Décès"
names(coronaCH)[names(coronaCH) == "TotalCured"] <- "Guéris"

# Formating date in coronaCanton data so it shows properly in the tables + graphs
coronaCanton$Date <- as.Date(coronaCanton$Date , format = "%d.%m.%y")
#corona$Date <- format(corona$Date, format="%d.%m")

# Formating date in coronaCH data so it shows properly in the tables + graphs
coronaCH$Date <- as.Date(coronaCH$Date , format = "%d.%m.%y")
#corona$Date <- format(corona$Date, format="%d.%m")

# Define UI for app
ui <- fluidPage(
    
    #Shiny app theme
    #theme = "bootstrap.css",
    theme = shinytheme("superhero"),
    
    #Shiny app theme selector (use to test themes)
    #shinythemes::themeSelector(),
    
    # App title 
    titlePanel("Situation du Coronavirus en Suisse"),
    h6(em("Dernière mise à jour des données: 10:24, 02/04/2020")),
    
    #sidebarLayout(
        
        #sidebarPanel(
        #     helpText("Choisissez le canton que vous souhaitez visualiser sous forme de graphique ou tableau"),
        #     selectInput(inputId = "Canton", label =  "Canton",
        #                 choices =  unique(as.character(coronaCanton$Canton)),
        #                 selected = "AG", multiple = FALSE),
        #     
        #     h5("Dernière mise à jour des données: 22:39 25/03/2020")
        #),
        
        mainPanel(
            tabsetPanel(type = "pills",
                tabPanel("Introduction",
                         fluidRow(
                             column(12, htmlOutput("presentation1")),
                             column(12, plotOutput("coronaSpatialParTete1")),
                             column(12, htmlOutput("presentation2"))
                         )
                            
                         ),
                tabPanel("Suisse",
                         fluidRow(
                             htmlOutput("htmlSuisse"),
                             tabsetPanel(
                                 tabPanel("Cartes",
                                          fluidRow(
                                              column(12, plotOutput("coronaSpatialParTete2")),
                                              column(12, plotOutput("coronaSpatial"))
                                          )),
                                 tabPanel("Graphiques",
                                          fluidRow(
                                              column(12, plotOutput("coronaCasesCH")),
                                              column(12, plotOutput("coronaCasesGraphDeathsCH"))
                                          )),
                                 tabPanel("Tableau",
                                          dataTableOutput("coronaCasesTableDeaths")
                                          )
                             )
                             
                             
                         )),
                tabPanel("Canton", 
                         fluidRow(
                             helpText("Choisissez ci-dessous le canton que vous souhaitez visualiser:"),
                             selectInput(inputId = "Canton", label =  "Canton",
                                         choices =  unique(as.character(coronaCanton$Canton)),
                                         selected = "AG", multiple = FALSE),
                             htmlOutput("htmlCanton"),
                             tabsetPanel(
                                 tabPanel("Graphiques",
                                          plotOutput("coronaCasesCanton"),
                                          plotOutput("coronaCasesCantonDeces")
                                 ),
                                 tabPanel("Tableau",
                                          dataTableOutput("tableau")
                                 )
                             )
                         )
                ),
                #tabPanel("Tableaux", dataTableOutput("tableau")), #no longer used..
                tabPanel("Démographie", dataTableOutput("tableauDemographique"))
            )
            
        )
    #)
)

# Define server logic
server <- function(input, output) {
    
    # Plot de tous les cas du canton choisi
    output$coronaCasesCanton <- renderPlot ({
        #corona$Date <- format(corona$Date, format="%d.%m")
        ggplot(subset(coronaCanton, coronaCanton$Canton==input$Canton), aes(x=Date, y=PositifsPar100000, colour = Canton, group = Canton)) +
            geom_line(color="#e61523", size=2) +
            theme_ipsum() +
            ggtitle(label = "Evolution des personnes positives au COVID-19 par 100'000 habitants", subtitle = paste("Canton: ", input$Canton, sep='')) +
            xlab("Date") + ylab("Nombre de personnes") + 
            theme(plot.title = element_text(color="white", size=15, face="bold", hjust=0.5),
                  plot.subtitle = element_text(color="red", size=14, face="bold.italic"),
                  axis.title.x = element_text(color="white", size=12, face="bold", hjust=0.5),
                  axis.title.y = element_text(color="white", size=12, face="bold", hjust=0.5),
                  axis.text.x = element_text(angle = 45, color = "white"),
                  axis.text.y = element_text(color = "white"),
                  plot.background = element_rect(fill = "#2b3e50", color = NA)
            )
    })
    
    # Plot de tous les décès du canton choisi
    output$coronaCasesCantonDeces <- renderPlot ({
        ggplot(subset(coronaCanton, coronaCanton$Canton==input$Canton), aes(x=Date, y=Total_Décès, colour = Canton, group = Canton)) +
            geom_line(color="#e61523", size=2) +
            theme_ipsum() +
            ggtitle(label = "Evolution des décès du au COVID-19", subtitle = paste("Canton: ", input$Canton, sep='')) +
            xlab("Date") + ylab("Nombre de personnes") + 
            theme(plot.title = element_text(color="white", size=15, face="bold", hjust=0.5),
                  plot.subtitle = element_text(color="red", size=14, face="bold.italic"),
                  axis.title.x = element_text(color="white", size=12, face="bold", hjust=0.5),
                  axis.title.y = element_text(color="white", size=12, face="bold", hjust=0.5),
                  axis.text.x = element_text(angle = 45, color = "white"),
                  axis.text.y = element_text(color = "white"),
                  plot.background = element_rect(fill = "#2b3e50", color = NA)
            )
    })
    
    # Plot du ratio de tous les cas par canton
    output$coronaCasesAll <- renderPlot({
        ggplot(subset(coronaCanton, coronaCanton$Canton==input$Canton), aes(x=Date, y=Pos_Pop_rate, colour = Canton, group = Canton)) +
            geom_line(color="#5b61bd", size=2) +
            theme_economist() +
            ggtitle("Evolution du ratio des personnes positives au COVID-19", subtitle = paste("Canton: ", input$Canton, sep='')) +
            xlab("Date") + ylab("Nombre de cas / population") + 
            theme(plot.title = element_text(color="black", size=15, face="bold", hjust=0.5),
                  plot.subtitle = element_text(color="blue", size=14, face="bold.italic"),
                  axis.title.x = element_text(color="black", size=12, face="bold", hjust=0.5),
                  axis.title.y = element_text(color="black", size=12, face="bold", hjust=0.5),
                  axis.text.x = element_text(angle=45),
            )
    })
    
    #Spatial map "cas positifs"
    output$coronaSpatial <- renderPlot ({
                ggplot(
                    data = dat_merged
                ) + 
                    geom_sf(
                        mapping = aes(
                        fill = CasPositifs31_3,
                        ),
                        color = "white",
                        size = 0.1
                    ) +
                    scale_fill_continuous(high = "#d03109", low = "#f6beae",
                                  name = "Cas positifs")+
                    scale_size(guide = "legend"
                    )+
                    geom_sf(
                        data = canton_geo,
                        fill = "transparent",
                        color = "white",
                        size = 0.1
                    ) +
                    geom_sf(
                        data = lake_geo,
                        fill = "#D6F1FF",
                        color = "transparent"
                    ) +
                    labs(x = NULL,
                        y = NULL,
                        title = "Nombre de personnes testées positives",
                        subtitle = "Aperçu par canton, état au 31 mars 2020") +
            theme_tufte() +
            theme(
                plot.background = element_rect(fill = "#2b3e50", color = NA),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.title = element_text(color="white"),
                plot.subtitle = element_text(color="white"),
                legend.title = element_text(color="white"),
                legend.text = element_text(color="white"),
                )
            }, bg="#2b3e50", execOnResize=T)
    
    #Spatial map "cas positifs par tete"
    output$coronaSpatialParTete1 <- output$coronaSpatialParTete2 <- renderPlot ({
        ggplot(
            data = dat_merged
        ) +
            geom_sf(
                mapping = aes(
                    fill = CasPositifsParTete31_3,
                ),
                color = "white",
                size = 0.1
            ) +
            scale_fill_continuous(high = "#d03109", low = "#f6beae",
                                  name = "Cas positifs \npar 100'000 \nhabitants")+
            scale_size(guide = "legend"
            )+
            geom_sf(
                data = canton_geo,
                fill = "transparent",
                color = "white",
                size = 0.1
            ) +
            geom_sf(
                data = lake_geo,
                fill = "#D6F1FF",
                color = "transparent"
            ) +
            labs(x = NULL,
                 y = NULL,
                 title = "Nombre de personnes testées positives par 100'000 habitants",
                 subtitle = "Aperçu par canton, état au 31 mars 2020") +
            theme_tufte() +
            theme(
                plot.background = element_rect(fill = "#2b3e50", color = NA),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.title = element_text(color="white"),
                plot.subtitle = element_text(color="white"),
                legend.title = element_text(color="white"),
                legend.text = element_text(color="white"),
            )
        
    }, bg="#2b3e50", execOnResize=T) 
    
    
    # Vue de tous les cas de corona en CH
    output$coronaCasesCH <- renderPlot({
        ggplot(subset(coronaCH, Canton == "CH"), aes(x=Date, y=Positifs, colour = Canton, group = Canton)) +
            geom_line(color="#588c3e", size=2) +
            theme_ipsum() +
            ggtitle("Evolution des personnes positives \n au COVID-19 en Suisse") +
            xlab("Date") + ylab("Nombre de personnes") + 
            theme(plot.title = element_text(color="white", size=15, face="bold", hjust=0.5),
                  plot.subtitle = element_text(color="red", size=14, face="bold.italic"),
                  axis.title.x = element_text(color="white", size=12, face="bold", hjust=0.5),
                  axis.title.y = element_text(color="white", size=12, face="bold", hjust=0.5),
                  axis.text.x = element_text(angle = 45, color = "white"),
                  axis.text.y = element_text(color = "white"),
                  plot.background = element_rect(fill = "#2b3e50", color = NA)
            )
    })
    
    # Plot de tous les morts en CH
    output$coronaCasesGraphDeathsCH <- renderPlot({
        ggplot(subset(coronaCH), aes(x=Date, y=Décès)) +
            geom_line(color="#588c3e", size=2) +
            theme_ipsum() +
            ggtitle("Evolution des décès dûs \n au COVID-19 en Suisse") +
            xlab("Date") + ylab("Nombre de personnes") + 
            theme(plot.title = element_text(color="white", size=15, face="bold", hjust=0.5),
                  plot.subtitle = element_text(color="red", size=14, face="bold.italic"),
                  axis.title.x = element_text(color="white", size=12, face="bold", hjust=0.5),
                  axis.title.y = element_text(color="white", size=12, face="bold", hjust=0.5),
                  axis.text.x = element_text(angle = 45, color = "white"),
                  axis.text.y = element_text(color = "white"),
                  plot.background = element_rect(fill = "#2b3e50", color = NA)
            )
    })
    
    # Datatable cas corona en CH
    output$coronaCasesTableDeaths <- renderDataTable({
        coronaCH[,c('Date', 'Positifs', 'Décès', 'Guéris')]
    },
    options = list(order = list(0, 'desc'))
    )
    
    #data table for selected canton stats
    output$tableau <- renderDataTable({
        coronaCanton[which(coronaCanton$Canton==input$Canton),c("Date", "Positifs", "PositifsPar100000", "Total_Décès", "Total Guéris")]
    },
    options = list(searching=FALSE,
                   lengthChange=0,
                   info=0,
                   paging=FALSE,
                   order = list(0, 'desc')) # sort by date
    )
    

    # datatable with cantonal data
    output$tableauDemographique <- renderDataTable({
        #tableauCantonal <- corona[which(corona$Canton==input$Canton), c("Canton", "Population", "Population > 65ans", "Lits", "Lits par habitant")]
        tableauCantonal <- coronaCanton[, c("Canton", "Population", "Population > 65ans", "Lits d'hôpitaux", "Lits par habitant")]
        unique(tableauCantonal)
        
    },
    # these options remove "how many items are displayed per page choice"
    options = list(lengthChange=0,
                   info=0),
    )
    
    # output to render HTML file on intro page (if not done like this there are formatting errors..)
    output$presentation1<-renderUI({
        includeHTML(htmlPresentation1)
    })
    
    # output to render HTML file on intro page 
    output$presentation2<-renderUI({
        includeHTML(htmlPresentation2)
    })
    
    # output to render HTML file on Suisse page
    output$htmlSuisse<-renderUI({
        includeHTML(htmlSuisse)
    })
    
    #output to render HTML file on Canton page
    output$htmlCanton<-renderUI({
        includeHTML(htmlCantons)
    })
        
    

    
}

shinyApp(ui = ui, server = server)