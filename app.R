################################################################################################
####                                                                                        ####
#### Projekt Videospiel Dashboard                                                           ####
#### Von: Minh Huynh (MatrNR 77135) und Thomas Glanz (MatrNR 76696)                         ####
####                                                                                        ####
################################################################################################

library(base)
library(shiny)
library(readr)
library(plotly)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(readxl)
library(gganimate)
library(DT)

vgsales <- read_csv("vgsales.csv")
consoles <- read_excel("consoles.xlsx")

ui <- dashboardPage(
  dashboardHeader(title = "Videospiele Verkaufszahlen Dashboard Minh & Thomas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        "Publisher & Spiele",
        tabName = "widgets",
        icon = icon("th")
      ),
      menuItem("Datensatz", tabName = "datensatz", icon = icon("table"))
    ),
    
    #style wird hier angepasst
    
    singleton(tags$head(tags$style(
      HTML(
        '

                #Navbar

                .skin-blue .main-header .logo {
                          background-color: light-blue;
                          }
                .skin-blue .main-header .logo:hover {
                          background-color: light-blue;
                          }
                .skin-blue .main-header .logo {
                          background-color: light-blue; color: white;
                          font-weight: bold;font-size: 24px;text-align: Right;
                          }
                .skin-blue .main-header .logo:hover {
                          background-color: light-blue;
                          }
                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          color: white;font-weight: bold;
                          }
                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: transparent;
                          color: white;font-weight: bold;font-size: 18px;
                          }
                .skin-blue .main-sidebar {
                         background-color: #323232;
                         }
                .skin-blue .main-header .navbar {
                          background-color: light-blue;
                          }
                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                          background-color: lightgrey;
                          }
                .box.box-solid.box-primary>.box-header {
                color:#fff;
                background:#323232
                }
                -webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}

                #Boxen und Sliderinput

                .box.box-solid.box-primary{
                border-bottom-color:#323232;
                border-left-color:#323232;
                border-right-color:#323232;
                border-top-color:#323232;
                }
                .irs-bar {background: light-blue; border-color: transparent;}
                .irs-bar-edge {background: light-blue; border-color :transparent;}
                .irs-single {background: light-blue;}
                .irs-slider {background: light-blue;}
                .irs-from {background: light-blue;}
                .irs-to {background: light-blue;}
      '
      )
    ))),
    
    sliderInput(
      "reichweite",
      "Zeitspanne:",
      min = 1980,
      max =  2015,
      value = c(1980, 2015)
    ),
    
    checkboxGroupInput(
      "auswahl",
      "Kategorie Auswahl:",
      choices = c(unique(vgsales$Genre)),
      selected = c(unique(vgsales$Genre))
    )
  ),
  
  #------------------------------------------------------------------------------
  
  dashboardBody(tabItems(
    #------------------------------------------------------------------------------
    # First tab content
    tabItem(tabName = "dashboard",
            
            fluidRow(
              column(
                10,
                
                infoBox(
                  "Erfasste Spiele",
                  count(vgsales),
                  icon = icon("bar-chart"),
                  color = "light-blue",
                  width = 3
                ),
                
                infoBox(
                  "Erfasse Jahre",
                  length(unique(vgsales$Year)),
                  icon = icon("calendar"),
                  color = "light-blue",
                  width = 3
                ),
                infoBox(
                  "Anzahl Publisher",
                  length(unique(vgsales$Publisher)),
                  icon = icon("building"),
                  color = "light-blue",
                  width = 3
                ),
                infoBox(
                  "Anzahl Platformen",
                  length(unique(vgsales$Platform)),
                  icon = icon("gamepad"),
                  color = "light-blue" ,
                  width = 3
                ),
                
                
                fluidRow(column(
                  12,
                  box(
                    title = "Spiele Verlauf",
                    status = "primary",
                    plotlyOutput("Jahreszahlen_Barplot"),
                    width = NULL
                  ),
                  
                  fluidRow(column(
                    4,
                    box(
                      title = "Verkaufszahlen gruppiert nach Märkten",
                      status = "primary",
                      plotlyOutput("VerkaufNachMarkt"),
                      width = NULL
                    ),
                  ),
                  
                  column(
                    4,
                    box(
                      title = "Systemtyp der Spiele",
                      status = "primary",
                      plotlyOutput("Konsole"),
                      width = NULL
                    ),
                    
                  ),
                  
                  column(
                    4,
                    box(
                      title = "Aufteilung aller Genre",
                      status = "primary",
                      plotlyOutput("pieChart", width = 400)
                    ),
                  ), ),
                ), ),
              ),
              
              column(
                2,
                
                infoBox(
                  "Verkaufszahlen Global",
                  sum(vgsales$Global_Sales) * 1000,
                  icon = icon("globe"),
                  color = "light-blue",
                  width = 50
                ),
                infoBox(
                  "Verkaufszahlen NA",
                  sum(vgsales$NA_Sales) * 1000,
                  color = "light-blue",
                  width = 50
                ),
                infoBox(
                  "Verkaufszahlen EU",
                  sum(vgsales$EU_Sales) * 1000,
                  color = "light-blue",
                  width = 50
                ),
                infoBox(
                  "Verkaufszahlen JP",
                  sum(vgsales$JP_Sales) * 1000,
                  color = "light-blue",
                  width = 50
                ),
                infoBox(
                  "Verkaufszahlen Rest",
                  sum(vgsales$Other_Sales) * 1000,
                  color = "light-blue",
                  width = 50
                ),
                
                br(),
                br(),
                br(),
                
                box(
                  title = "System entwickelt in",
                  status = "primary",
                  plotlyOutput("LandVergleich"),
                  width = NULL
                ),
                
              ),
            ), ),
    
    #------------------------------------------------------------------------------
    
    # Second tab content
    tabItem(
      tabName = "widgets",
      h2("Publisher & Spiele "),
      
      fluidRow(column(
        4,
        box(
          title = "Publisher mit den meisten Spielen",
          status = "primary",
          plotlyOutput("interactivePlot"),
          width = NULL,
        ),
      ),
      
      column(
        8,
        box(
          title = "Anzahl an Spiele veröffentlicht im Jahr",
          status = "primary",
          plotlyOutput("verkaufVerlauf"),
          width = NULL
          
        ),
      ),),
      fluidRow(column(
        4,
        
        
        box(
          title = "Input bins",
          status = "primary",
          width = NULL,
          sliderInput(
            "bins",
            "Number of bins:",
            min = 1,
            max = 50,
            value = 3
          ),
          
          radioButtons(
            "verkaufauswahl",
            h3("Controller"),
            choices = list(
              "Globale Verkaufszahlen" = "Global_Sales",
              "EU Verkaufszahlen" = "EU_Sales",
              "NA Verkaufszahlen" = "NA_Sales",
              "JPVerkaufszahlen" = "JP_Sales",
              "Andere Verkaufszahlen" = "Other_Sales"
            ),
            
          )
        )
      ),
      
      column(
        8,
        box(
          title = "Am meist verkaufte Spiele",
          status = "primary",
          plotlyOutput("barPlot"),
          width = NULL
        ),
        
      ),),
      
    ),
    
    #------------------------------------------------------------------------------
    
    tabItem(
      tabName = "datensatz",
      fluidRow(DT::dataTableOutput(outputId = "data_vgsales")),
      fluidRow(DT::dataTableOutput(outputId = "consoles"))
    )
  ), )
)


#------------------------------------------------------------------------------

server <- function(input, output) {
  #------------------------------------------------------------------------------
  #Seite1
  
  output$Jahreszahlen_Barplot <- renderPlotly({
    x <-
      subset(vgsales,
             Year >= input$reichweite [1] &
               Year <= input$reichweite[2])   #Für die Interaktivität, hierdurch wird das Jahr mit dem Slider eingestellt.
    
    plot_ly(x = x$Year,                       #Plotly Plot in einen Barchart
            y = x$Genre,
            type = "bar")
  })
  
  
  output$VerkaufNachMarkt <- renderPlotly({
    auswahl <- input$auswahl
    auswahl <- as.data.frame(auswahl)
    vgsales <-
      subset(vgsales, Genre == auswahl[1:length(auswahl), ])            #Für die Interaktivität, Genre auswählbar, standardmäßig sind alle ausgewählt
    
    verkauf <- vgsales[, 7:11]
    namen <- colnames(verkauf)                      
    y <-
      c(
        sum(verkauf$NA_Sales),
        sum(verkauf$EU_Sales),
        sum(verkauf$JP_Sales),
        sum (verkauf$Other_Sales),
        sum(verkauf$Global_Sales)
      )
    plot_ly (verkauf,
             x = reorder(namen, -y),
             y = y,
             type = 'bar')
    #name = 'NA Verkaufszahlen' )  'EU Verkaufszahlen' )    name = 'JP Verkaufszahlen'  name = 'Andere Verkaufszahlen'   name = 'Globale Verkaufszahlen') %>% layout(barmode = 'group' )
    
  })
  
  
  output$Konsole <- renderPlotly({                                      # Stacked Plot mit Plotly um die Consolen Typen darzustellen
    aaa <- subset(consoles, Type == 'Console', )
    a  <- length(aaa$Type)
    
    bbb <- subset(consoles, Type == 'Handheld', )
    b  <- length(bbb$Type)
    
    ccc <- subset(consoles, Type == 'PC', )
    c  <- length(ccc$Type)
    
    plot_ly(
      consoles,
      x = 'Konsolen',
      y = a ,
      type = 'bar',
      name = 'Konsolen'
    ) %>% add_trace(y = b, name = 'Handheld') %>% add_trace(y = c, name = 'PC') %>% layout(barmode = 'stack')
  })
  
  
  output$pieChart <- renderPlotly({                                    #piechart wird hier erstellt.

    genre <- vgsales$Genre
    
    list  <- list(vgsales$Genre)
    t     <-
      aggregate(vgsales$Genre, by = list(vgsales$Genre), "length")
    
    plot_ly(vgsales,
            labels = ~ genre,
            values = t ,
            type = 'pie') %>%
      layout(
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    
  })
  
  
  output$LandVergleich <- renderPlotly({                          #Plotly barchart für Herstellerland Vergleich
    aaa <- subset(consoles, Country == 'Japan', )
    a  <- length(aaa$Country)
    
    bbb <- subset(consoles, Country == 'USA', )
    b  <- length(bbb$Country)
    
    plot_ly(
      consoles,
      x = 'Land',
      y = a ,
      type = 'bar',
      name = 'Japan'
    ) %>% add_trace(y = b, name = 'USA')
  })
  
  #------------------------------------------------------------------------------
  #Seite2
  
  output$interactivePlot <- renderPlotly({                                         #Plot zum Darstellen von den Publisher mit den meisten Spielen, sortiert und interaktiv
    t <-
      vgsales %>% group_by(Publisher) %>% summarise(n()) %>% arrange(desc(`n()`))
    tz <- t[1:input$bins, ]
    
    
    plot1 <-
      ggplot(data = tz, aes(x = reorder(Publisher, -`n()`) , y = `n()`)) + geom_bar(stat = "identity", fill = "lightblue") + labs(y = "Herausgebrachte Spiele", x = "Publisher Name") + coord_flip() + theme_classic()
    ggplotly(plot1)
    
  })
  
                                                                                     #Plot um Anzahl der Spiele pro Jahr zu zeigen als Verlauf
  output$verkaufVerlauf <- renderPlotly({
    vgsales <-
      subset(
        vgsales,
        vgsales$Year >= input$reichweite[1] &
          vgsales$Year <= input$reichweite[2],
        select = Name:Global_Sales
      )
    
    x <- unique(vgsales$Year)
    t <- vgsales %>% group_by(Year)
    
    tt <- t %>% summarise(n = n())
    tt <- data.frame(tt)
    plot1 <-
      ggplot(data = tt, aes(x = Year, y = n , group = 1)) + labs(y = "Anzahl Spiele") + geom_line(color = "#6066CA") + geom_point(color = "#6066CA") + theme_classic() + transition_reveal(Year)
    ggplotly(plot1)
  })
                                                                          #die meist verkaufte Spiele werden hier gezeigt, interaktiv und reaktiv
  
  output$barPlot <- renderPlotly({
    #show(input$auswahl)
    
    test1 <- as.data.frame(input$auswahl)
    #show(test1[1:nrow(test1),] )
    typeof(test1)
    
    top_Sales <-
      subset(vgsales,
             vgsales$Year > input$reichweite[1] &
               vgsales$Year < input$reichweite[2],
      )
    top_Sales <- top_Sales[1:input$bins, ]
    
    
    verkaufszahlen <- input$verkaufauswahl
    
    
    dist1 <- switch(
      verkaufszahlen,
      
      Global_Sales = top_Sales$Global_Sales,
      EU_Sales = top_Sales$EU_Sales,
      NA_Sales = top_Sales$NA_Sales,
      JP_Sales = top_Sales$JP_Sales,
      Other_Sales = top_Sales$Other_Sales,
    )
    plot <-
      ggplot(data = top_Sales, aes(x = reorder(Name, -dist1), y = dist1,)) + geom_bar(stat = "identity", fill = "lightblue") + labs(x = "Spielname", y = "Verkaufszahlen in Mio.") + theme_classic()
    ggplotly(plot)
  })
  
  #------------------------------------------------------------------------------
  #Seite 3
                                                                                  #datensätze werden als Tabellen ausgegeben
  output$data_vgsales = DT::renderDataTable({
    vgsales
  })
  
  output$consoles <- DT::renderDataTable({
    consoles
  })
  
  #------------------------------------------------------------------------------
}


shinyApp(ui, server)