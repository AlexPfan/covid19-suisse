library(shiny)
library(ggplot2)
library(hrbrthemes)
library(sf)
library(tidyverse)
library(viridis)
library(RColorBrewer)

coronaCH <- read.csv("0.CoronaCH6.csv")
coronaCanton <- read.csv("0.CoronaCanton11.csv")

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
canton_geo <- read_sf("g2k20.shp")
# read country borders
country_geo <- read_sf("g2l20.shp")
# read lakes
lake_geo <- read_sf("g2s20.shp")

#Data work
coronaSpatial$KTNR <- as.numeric(coronaSpatial$KTNR)
dat_merged <- merge(canton_geo, coronaSpatial, by="KTNR")
dat_merged$CasPositifs22_3 <- as.numeric(dat_merged$CasPositifs22_3)
dat_merged$CasPositifsParTete22_3 <- as.numeric(dat_merged$CasPositifsParTete22_3)

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
names(coronaCanton)[names(coronaCanton) == "TotalCured"] <- "Total Guéris"
names(coronaCanton)[names(coronaCanton) == "SortisDeHôpital"] <- "Sortis de l'hôpital"

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
    
    theme = "bootstrap.css",
    
    # App title ----
    titlePanel("Situation du Coronavirus en Suisse"),
    
    sidebarLayout(
        
        sidebarPanel(
            helpText("Choisissez le canton que vous souhaitez visualiser sous forme de graphique ou tableau"),
            selectInput(inputId = "Canton", label =  "Canton",
                        choices =  unique(as.character(coronaCanton$Canton)),
                        selected = "AG", multiple = FALSE),
            
            h5("Dernière mise à jour des données: 17:42 23/03/2020")
        ),
        
        mainPanel(
            tabsetPanel(type = "pills",
                tabPanel("Introduction",
                         fluidRow(
                             includeHTML(htmlPresentation1),
                             column(12, plotOutput("coronaSpatialParTete1")),
                             includeHTML(htmlPresentation2)
                         )
                            
                         ),
                tabPanel("Suisse",
                         fluidRow(
                             includeHTML(htmlSuisse),
                             column(12, plotOutput("coronaSpatial")),
                             column(12, plotOutput("coronaSpatialParTete2")),
                             column(12, plotOutput("coronaCasesCH")),
                             column(12, plotOutput("coronaCasesGraphDeathsCH")),
                             column(12, dataTableOutput("coronaCasesTableDeaths"))
                         )),
                tabPanel("Canton", 
                         fluidRow(
                             includeHTML(htmlCantons),
                             column(12, plotOutput("coronaCasesCanton")),
                             column(12, plotOutput("coronaCasesAll"))
                         )
                ),
                tabPanel("Tableaux", dataTableOutput("tableau")),
                tabPanel("Statistiques Cantonales", dataTableOutput("tableauCantonal"))
            )
            
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Vue du canton choisi
    output$coronaCasesCanton <- renderPlot ({
        #corona$Date <- format(corona$Date, format="%d.%m")
        ggplot(subset(coronaCanton, coronaCanton$Canton==input$Canton), aes(x=Date, y=Positifs, colour = Canton, group = Canton)) +
            geom_line(color="#e61523", size=2) +
            theme_ipsum() +
            ggtitle(label = "Evolution des personnes positives au COVID-19", subtitle = paste("Canton: ", input$Canton, sep='')) +
            xlab("Date") + ylab("Nombre de personnes") + 
            theme(plot.title = element_text(color="black", size=15, face="bold", hjust=0.5),
                    plot.subtitle = element_text(color="red", size=14, face="bold.italic"),
                    axis.title.x = element_text(color="black", size=12, face="bold", hjust=0.5),
                    axis.title.y = element_text(color="black", size=12, face="bold", hjust=0.5),
                    axis.text.x = element_text(angle=45))
    })
    
    # Vue du ratio de tous les cas de corona par canton
    output$coronaCasesAll <- renderPlot({
        ggplot(subset(coronaCanton, coronaCanton$Canton==input$Canton), aes(x=Date, y=Pos_Pop_rate, colour = Canton, group = Canton)) +
            geom_line(color="#5b61bd", size=2) +
            theme_ipsum() +
            ggtitle("Evolution du ratio des personnes positives au COVID-19", subtitle = paste("Canton: ", input$Canton, sep='')) +
            xlab("Date") + ylab("Nombre de cas / population") + 
            theme(plot.title = element_text(color="black", size=15, face="bold", hjust=0.5),
                  plot.subtitle = element_text(color="blue", size=14, face="bold.italic"),
                  axis.title.x = element_text(color="black", size=12, face="bold", hjust=0.5),
                  axis.title.y = element_text(color="black", size=12, face="bold", hjust=0.5),
                    axis.text.x = element_text(angle=45))
    })
    
    #Spatial map "cas positifs"
    output$coronaSpatial <- renderPlot ({
        ggplot(
            data = dat_merged
        ) +
            geom_sf(
                mapping = aes(
                    fill = CasPositifs22_3,
                ),
                color = "white",
                size = 0.1
            ) +
            scale_fill_continuous(high = "#e53a0f", low = "#f4dcd5",
                                  name = "Cas positifs")+
            scale_size(guide = "legend"
            )+
            geom_sf(
                data = canton_geo,
                fill = "transparent",
                color = "white",
                size = 0.5
            ) +
            geom_sf(
                data = lake_geo,
                fill = "#D6F1FF",
                color = "transparent"
            ) +
            labs(x = NULL,
                 y = NULL,
                 title = "Nombre de personnes testées positives",
                 subtitle = "Aperçu par canton, état au 22 mars 2020") 
    })   
    
    #Spatial map "cas positifs par tete"
    output$coronaSpatialParTete1 <- output$coronaSpatialParTete2 <- renderPlot ({
        ggplot(
            data = dat_merged
        ) +
            geom_sf(
                mapping = aes(
                    fill = CasPositifsParTete22_3,
                ),
                color = "white",
                size = 0.1
            ) +
            scale_fill_continuous(high = "#e53a0f", low = "#f4dcd5",
                                  name = "Cas positifs par \n100'000 habitants")+
            scale_size(guide = "legend"
            )+
            geom_sf(
                data = canton_geo,
                fill = "transparent",
                color = "white",
                size = 0.5
            ) +
            geom_sf(
                data = lake_geo,
                fill = "#D6F1FF",
                color = "transparent"
            ) +
            labs(x = NULL,
                 y = NULL,
                 title = "Nombre de personnes testées positives par 100'000 habitants",
                 subtitle = "Aperçu par canton, état au 22 mars 2020") 
    }) 
    
    # Vue de tous les cas de corona en CH
    output$coronaCasesCH <- renderPlot({
        ggplot(subset(coronaCH, Canton == "CH"), aes(x=Date, y=Positifs, colour = Canton, group = Canton)) +
            geom_line(color="#588c3e", size=2) +
            theme_ipsum() +
            ggtitle("Evolution des personnes positives \n au COVID-19 en Suisse") +
            xlab("Date") + ylab("Nombre de personnes") + 
            theme(plot.title = element_text(color="black", size=15, face="bold", hjust=0.5),
                  axis.title.x = element_text(color="black", size=12, face="bold", hjust=0.5),
                  axis.title.y = element_text(color="black", size=12, face="bold", hjust=0.5),
                  axis.text.x = element_text(angle=45))
    })
    
    # Vue de tous les morts en CH
    output$coronaCasesGraphDeathsCH <- renderPlot({
        ggplot(subset(coronaCH), aes(x=Date, y=Décès)) +
            geom_line(color="#588c3e", size=2) +
            theme_ipsum() +
            ggtitle("Evolution des décès dûs \n au COVID-19 en Suisse") +
            xlab("Date") + ylab("Nombre de personnes") + 
            theme(plot.title = element_text(color="black", size=15, face="bold", hjust=0.5),
                  axis.title.x = element_text(color="black", size=12, face="bold", hjust=0.5),
                  axis.title.y = element_text(color="black", size=12, face="bold", hjust=0.5),
                  axis.text.x = element_text(angle=45))
    })
    
    output$coronaCasesTableDeaths <- renderDataTable({
        coronaCH[,c('Date', 'Positifs', 'Décès', 'Guéris')]
    })
    
    output$tableau <- renderDataTable({
        # ajouter cas confirmés quand plus de données
        coronaCanton$Date <- format(coronaCanton$Date, format="%d.%m.%y")
        coronaCanton[which(coronaCanton$Canton==input$Canton),c("Canton", "Date", "Positifs", "Hospitalisations", "Total Guéris", "Sortis de l'hôpital")]
    },
    options = list(searching=FALSE,
                   lengthChange=0,
                   info=0,
                   paging=FALSE),
    )
    

    output$tableauCantonal <- renderDataTable({
        #tableauCantonal <- corona[which(corona$Canton==input$Canton), c("Canton", "Population", "Population > 65ans", "Lits", "Lits par habitant")]
        tableauCantonal <- coronaCanton[, c("Canton", "Population", "Population > 65ans", "Lits d'hôpitaux", "Lits par habitant")]
        unique(tableauCantonal)
        
    },
    options = list(lengthChange=0,
                   info=0),
    )
    

    
}

shinyApp(ui = ui, server = server)