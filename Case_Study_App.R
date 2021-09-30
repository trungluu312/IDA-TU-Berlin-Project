## Download needed packages

## Loading in needed packages
library(shiny)
library(leaflet)
library(leafpop)
library(bslib)
library(dplyr)
library(readr)
library(ggplot2)
library(openxlsx)

## Create a function to calculate distance between municipalities and Berlin
distance_calculation <- function(long, lat, berlin_long = 13.40495, berlin_lat = 52.52000){
    R = 6378.137; ##/ Radius of earth in KM
    dLat = lat * pi / 180 - berlin_lat * pi / 180;
    dLon = long * pi / 180 - berlin_long * pi / 180;
    a = sin(dLat/2) * sin(dLat/2) + cos(berlin_lat * pi / 180) * cos(lat * pi / 180) *
        sin(dLon/2) * sin(dLon/2);
    c = 2 * atan2(sqrt(a), sqrt(1-a));
    d = R * c;
    return(d)
}



ui <-fluidPage(
    ## Change theme for the app
    theme = bs_theme(bootswatch = "superhero"),
    
    ## add image to the head
    tags$img(src='https://www.qw.tu-berlin.de/fileadmin/_processed_/8/8d/csm_QW_ohne_Text_print_a4670877cd.jpg', height = 125, width =200),
    
    ##Header
    titlePanel("Municipalities around Berlin with affected vehicles"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("range", 
                        label = "Choose a radius:",
                        min = 0, max = 100, value = 50)
                ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Map", leafletOutput("map")),
                        tabPanel("Chart", plotOutput("chart")),
                        tabPanel("Download dataset", downloadButton("downloadData", "Download .csv"),downloadButton("downloadDataxlsx", "Download .xlsx")),
                        tabPanel("Underlying dataset for shiny App", tableOutput('table')))
                        
            )
    )
)
# Define server
server <- function(input, output) {
        rangeInput <- reactive(input$range)
        load("./Final_dataset_group_25.RData")
        geo_daten_gemeinden <- read_csv2("./Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv")
        ## Loading in geo data and select only municipalities in radius of 80km from Berlin
        gemeinde_in_80km <- geo_daten_gemeinden %>%
            mutate(Distance = distance_calculation(Laengengrad, Breitengrad))%>%
            select(Gemeinden = Gemeinde, Distance)%>%
            filter(Distance <= 80)
        
        
        shiny_app_1 <- berlin_all%>% filter(Fehlerhaft == TRUE) %>% group_by(Gemeinden) %>% summarise(count_vehicles = n())
        shiny_app_2 <- berlin_all%>% filter(Fehlerhaft == FALSE) %>% group_by(Gemeinden) %>% summarise(count_unaffected_vehicles = n())
        
        shiny_app <- full_join(shiny_app_1, shiny_app_2)
        shiny_app[is.na(shiny_app)] <- 0
        
        shiny_app <- inner_join(shiny_app, geo_daten_gemeinden, by=c("Gemeinden"="Gemeinde"))
        shiny_app <- shiny_app%>%select(Gemeinden, count_vehicles, count_unaffected_vehicles, Laengengrad, Breitengrad)
        shiny_app <- inner_join(shiny_app, gemeinde_in_80km, by = "Gemeinden")

        
        map_data_react <- reactive({
            
            shiny_app %>% dplyr::filter(Distance < input$range)
        })
    
        output$map <- renderLeaflet({
            map_data <- map_data_react()
            
            plotter <- function(ID){
                
                dataFiltered <- map_data %>% filter(Gemeinden == ID)
                
                data_table_1 <- tibble(
                    x = c("Affected diesel-engined vehicles","Registered vehicles"),
                    y = c(sum(dataFiltered$count_vehicles), sum(dataFiltered$count_unaffected_vehicles)))
                
                p<- ggplot(data_table_1, aes(x,y)) + geom_col(fill="#2a5590", colour="black") + geom_text(aes(label = y), vjust = -0.5) +
                    labs(x = "") + labs(y = "Number of vehicles") + labs(title = dataFiltered$Gemeinden) + 
                    theme(text = element_text(size=8))
                return(p)
            }
            q = lapply(map_data$Gemeinden, function(i) {
                plotter(i)
            })
            
            map_data %>% leaflet() %>% 
            setView(lng = 13.40495, lat = 52.52000, zoom = 8) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%    
                
            addCircles(lng = 13.40495, lat =52.52, 
                        opacity = 0.2,
                        radius = rangeInput()*1000, 
                        color = "#006400", fill = FALSE, group='markers')%>%
                
            addCircleMarkers(~Laengengrad,
                             ~Breitengrad,
                             #popup = ~as.character(count_vehicles),
                             popup = popupGraph(q),
                             radius = 3)
        })
        
        output$chart <- renderPlot({
            
            map_data <- map_data_react()
            
            data_table <- tibble(
                x = c("Affected diesel-engined vehicles","Registered vehicles"),
                y = c(sum(map_data$count_vehicles), sum(map_data$count_unaffected_vehicles))
            )
            
            data_table %>%
                ggplot(aes(x,y)) + geom_col(fill="#2a5590", colour="black") + geom_text(aes(label = y), vjust = -0.5) +
                labs(x = "") + labs(y = "Number of vehicles") + labs(title = "Relation between affected and unaffected vehicles.")
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("berlindataset", ".csv", sep="")
            },
            content = function(file) {
                write.csv(berlin_all %>% arrange(Gemeinden), file)
            }
        )
        
        output$downloadDataxlsx <- downloadHandler(
            filename = function() {
                paste("berlindataset", ".xlsx", sep="")
            },
            content = function(file) {
                write.xlsx(berlin_all %>% arrange(Gemeinden), file)
            }
        )
        
        output$table <- renderTable({map_data_react()}) 
        
}
shinyApp(ui = ui, server = server)
