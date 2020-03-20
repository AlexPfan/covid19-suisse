library(shiny)
library(ggplot2)
library(hrbrthemes)

coronaCH <- read.csv("0.CoronaCH.csv")
coronaCanton <- read.csv("0.CoronaCanton.csv")

htmlPresentation <- "htmlPresentation.html"

# Renaming columns in CoronaCanton dataset
names(coronaCanton)[names(coronaCanton) == "date"] <- "Date"
names(coronaCanton)[names(coronaCanton) == "canton"] <- "Canton"
names(coronaCanton)[names(coronaCanton) == "population"] <- "Population"
names(coronaCanton)[names(coronaCanton) == "O65_perc"] <- "Population > 65ans"
names(coronaCanton)[names(coronaCanton) == "beds"] <- "Lits d'hôpitaux"
names(coronaCanton)[names(coronaCanton) == "bedsPerCapita"] <- "Lits par habitant"
names(coronaCanton)[names(coronaCanton) == "tested_pos"] <- "Positifs"
names(coronaCanton)[names(coronaCanton) == "TotalConfCases"] <- "Confirmés"

# Renaming columns in CoronaCH dataset
names(coronaCH)[names(coronaCH) == "date"] <- "Date"
names(coronaCH)[names(coronaCH) == "canton"] <- "Canton"
names(coronaCH)[names(coronaCH) == "population"] <- "Population"
names(coronaCH)[names(coronaCH) == "O65_perc"] <- "Population > 65ans"
names(coronaCH)[names(coronaCH) == "beds"] <- "Lits d'hôpitaux"
names(coronaCH)[names(coronaCH) == "bedsPerCapita"] <- "Lits par habitant"
names(coronaCH)[names(coronaCH) == "tested_pos"] <- "Positifs"
names(coronaCH)[names(coronaCH) == "TotalConfCases"] <- "Confirmés"

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
    titlePanel(
        h1("Situation du Coronavirus en Suisse", align="center")
    ),
    
    sidebarLayout(
        
        sidebarPanel(
            helpText("Choisissez le canton que vous souhaitez visualiser sous forme de graphique ou tableau"),
            selectInput(inputId = "Canton", label =  "Canton",
                        choices =  unique(as.character(coronaCanton$Canton)),
                        selected = "AG", multiple = FALSE),
            
            h5("Dernière mise à jour des données: 10:30 20/03/2020")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Introduction", includeHTML(htmlPresentation)),
                tabPanel("Suisse",
                         fluidRow(
                             h3("Vous trouverez ci-dessous quelques informations concernant l'évolution du COVID-19 pour toute la Suisse."),
                             helpText("4222 personnes positives et 43 décès."),
                             column(12, plotOutput("coronaCasesCH"))
                             
                         )),
                tabPanel("Canton", 
                         fluidRow(
                             h3("Vous trouverez ci-dessous quelques informations concernant l'évolution du COVID-19 par canton."),
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
    
    output$tableau <- renderDataTable({
        # ajouter cas confirmés quand plus de données
        coronaCanton$Date <- format(coronaCanton$Date, format="%d.%m.%y")
        coronaCanton[which(coronaCanton$Canton==input$Canton),c("Canton", "Date", "Positifs")]
    },
    options = list(searching=FALSE,
                   bLengthChange=0,
                   bInfo=0,
                   paging=FALSE),
    )
    

    output$tableauCantonal <- renderDataTable({
        #tableauCantonal <- corona[which(corona$Canton==input$Canton), c("Canton", "Population", "Population > 65ans", "Lits", "Lits par habitant")]
        tableauCantonal <- coronaCanton[, c("Canton", "Population", "Population > 65ans", "Lits d'hôpitaux", "Lits par habitant")]
        unique(tableauCantonal)
        
    },
    options = list(bLengthChange=0,
                   bInfo=0),
    )
    

    
}

shinyApp(ui = ui, server = server)