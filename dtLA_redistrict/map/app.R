##this is for shiny map


library(shinydashboard)     # Dashboard version of shiny
library(shinydashboardPlus) # Dashboard extras (mainly right sidebar)
library(shiny)              # Base shiny
library(shinyWidgets)       # For cool buttons, sliders, checkboxes, etc.
library(leaflet)            # For interactive maps
library(htmltools)          # Custom HTML control
library(RColorBrewer)       # Color palette management
library(shinycssloaders)    # For loading spinner/animation
library(shinyjs)            # For running custom Java Script code in Shiny evironment
library(shinyBS)          # For tooltips, popovers, etc.
library(tigris)
library(shinythemes)
library(readr)
library(sf)
library(gridExtra)
library(plotly)
library(stringr)
library(dplyr)
library(tidyverse)
library(classInt)
library(ggalt)
#install.packages("bsplus")


#set working directory

source('global.R')

#cat(ls(), "\n")
cat(file=stderr(), "done the preamble", "\n")
ui <- bootstrapPage(
  #tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = FALSE,
             "Central City Association of Los Angeles", id="nav",
             tabPanel("Redistricting",
                      div(class="outer",
                          tags$head(includeCSS("style.css")),
                          leafletOutput("main_map", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 100, left = 55, width = 500, fixed=TRUE,
                                        draggable = T, height = 'auto',
                                        #img(src="logo.png",height=37,width=110,align = "left"),
                                        pickerInput("select_layer", label = h4("Layer Selection"), inline = F, 
                                                    selected = "Nothing",width='100%',
                                                    choices = sort(c(unique(as.character(census$var_name)))),
                                                    choicesOpt = list(
                                                      style = rep(("color:black; font-size: 110%;"), 56)),
                                                    options = list(liveSearch = TRUE)),
                                        selectizeInput("select_tract",label = "Tract Selection", multiple = TRUE,
                                                       selected = NULL,
                                                       choices = sort(as.character(la_census$census), decreasing = F),width='100%',
                                                       options = list(maxItems = 9999,placeholder = "Select Tracts", 'plugins' = list('remove_button'))
                                        ),
                                        span(h4(htmlOutput("la_change"), align = "left"), style="color:#270180"),
                                        span(h6(htmlOutput("author"), align = "left")),
                                        span(h6(htmlOutput("source"), align = "left"))
                                        
                          ),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 100, right = 55, width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        span(h4(htmlOutput("pro"), align = "left"), style="color:#270180"))
                                        
                                        
                          ),
                          absolutePanel(id = "logo", class = "card", bottom = 270, right = 20, width = 120, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.ccala.org/', tags$img(src='logo.png',height='110',width='110')))
                      
                          
                      )
             )
  )


server <- function(input, output, session) {
  # Pre-define map function to be called later
  make_leaflet_map <- function() { 
    map1 <- leaflet() %>%
      
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom = 3), group = "Plain basemap") %>%
      addProviderTiles("OpenStreetMap",options = providerTileOptions(minZoom = 3), group = "Street Basemap")  %>%
      # 
      addMapPane("line", zIndex=420) %>%
      # 
      addMapPane("Council", zIndex = 408)%>%
      addMapPane("Neighborhood", zIndex = 405)%>%
      addMapPane("Downtown", zIndex = 402)%>%
      addMapPane("Tract", zIndex = 401) %>%
      addMapPane("Base Tract", zIndex = 400) %>%# Pane z-Index for polygons to stay underneath the markers
      #
      addLayersControl(
        baseGroups = c("Plain Basemap","Street Basemap"), 
        overlayGroups = c('Tract','Council','Neighborhood','Downtown','Base Tract'),
        options = layersControlOptions(collapsed = F, autoZIndex = TRUE),
        position = "bottomright")  %>%
      hideGroup(c("Neighborhood",'Council')) %>%
      addPolylines(data = dtla,
                   weight=5,
                   color = 'royalblue',
                   group = 'Downtown',
                   opacity = 0.8,
                   options = pathOptions(pane = "Downtown"))%>%
      addPolygons(data = council,
                  fillColor ='#00000',
                  fillOpacity = 0,
                   weight = 4,
                   color = 'green',
                   group = 'Council',
                   opacity = 0.8,
                   popup = ~paste('District',district, sep = ' '),
                   options = pathOptions(pane = "Council"))%>%
      addPolygons(data = neigh,
                  fillColor ='#00000',
                  fillOpacity = 0,
                   weight = 4,
                   color = 'orange',
                   group = 'Neighborhood',
                   opacity = 0.8,
                   popup = ~name,
                   options = pathOptions(pane = "Neighborhood"))%>%
      addPolygons(data = la_census,
                  fillColor ='grey',
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(stroke = 4, weight = 3,
                                                      color = 'black'),
                  weight = 2,
                  color = 'grey',
                  group = 'Base Tract',
                  opacity = 0.8,
                  popup = ~census,
                  layerId = ~GEOID,
                  options = pathOptions(pane = "Base Tract"))
    

    
    
    return(map1)
  }
  # Rendering the main map
  output$main_map <- renderLeaflet({make_leaflet_map()})
  
  #update the dataset
  # update_dataset <- reactive({
  #   var <- as.character(census$my_cen_var_names[census$var_name==input$select_layer])
  #   data_set <- dt_census %>% select("GEOID","NAME",'census',var)
  #   return(data_set)})
  #update the census
  update_dataset_census <- reactive({
    var <- as.character(census$my_cen_var_names[census$var_name==input$select_layer])
    data_set_census <- la_census %>% select("GEOID","NAME",'census',var)%>%
      filter(census%in%c(input$select_tract) )
    return(data_set_census)})
  # fill colors
  quantcolors <- reactive({
    if(input$select_layer == 'Nothing'){
      return(colorBin("purple", domain = 1))}
    if(input$select_layer == 'Total Population'){
      return(colorQuantile("Purples",la_census$total_population, n=6))}
    if(input$select_layer == '# Limited English'){
      return(colorQuantile("Purples", la_census$limited_english_totpop, n=6))}
    if(input$select_layer == '# African American Alone'){
      return(colorQuantile("Purples", la_census$aa_pop, n=6))}
    if(input$select_layer == '# API'){
      return(colorBin("Purples", la_census$api_pop, bins=3))}
    if(input$select_layer == '# Asian Alone'){
      return(colorQuantile("Purples", la_census$asian_pop, n=6))}
    if(input$select_layer == '# Foreign Born'){
      return(colorQuantile("Purples", la_census$foreign_born, n=6))}
    if(input$select_layer == '# Latinx Alone'){
      return(colorQuantile("Purples", la_census$hispanic_pop, n=5))}
    if(input$select_layer == '# Renter'){
      return(colorQuantile("Purples", la_census$renter_number, n=6))}
    if(input$select_layer == '# Under 100% Poverty Level'){
      return(colorQuantile("Purples", la_census$poverty_number, n=6))}
    if(input$select_layer == 'Median Income'){
      return(colorQuantile("Purples", la_census$medi_income, n=6))}
    if(input$select_layer == '# White Alone'){
      return(colorQuantile("Purples", la_census$white_only, n=6))}
  })
  
  #### layer click #### 
  # Geography click
  data_of_click <- reactiveValues(clickedMarker = NULL, clickedGeography = NULL)
  right_lay_g <- c()
  observeEvent(input$main_map_shape_click, {
    data_of_click$clickedGeography <- input$main_map_shape_click
    
    #print(data_of_click$clickedGeography)
    
    cat(file=stderr(), "Observed Geography Click", "\n")
    
    if(!is.null(data_of_click$clickedGeography$id)){
      right_lay = la_census %>% filter(GEOID==data_of_click$clickedGeography$id)
      cs <- c(right_lay['census'])
      right_lay_g <- c(right_lay_g,cs)
      #print(right_lay_g$census)
      st=input$select_tract
      updateSelectizeInput(session,"select_tract",
                        selected=c(st,right_lay_g))
      }
    
  })
  
  
  output$pro <- renderText({
    if(!is.null(data_of_click$clickedGeography$id)){
      if(nchar(data_of_click$clickedGeography$id)==11){
        right_lay = la_census %>% filter(GEOID==data_of_click$clickedGeography$id)}
      else{
        right_lay = la_census %>% filter(NAME==data_of_click$clickedGeography$id)
      }
      HTML(paste(paste0('Census Tract: ',right_lay$census),
                 paste0('Total Population: ',right_lay$total_population),
                 #paste0('# Limited English: ',right_lay$limited_english_totpop), 
                 paste0('# White Alone%: ',round(right_lay$white_only/right_lay$total_population*100,2)),
                 paste0('# African American Alone%: ',round(right_lay$aa_pop/right_lay$total_population*100,2)),
                 paste0('# API%: ',round(right_lay$api_pop/right_lay$total_population*100,2)),
                 paste0('# Asian Alone%: ',round(right_lay$asian_pop/right_lay$total_population*100,2)),
                 paste0('# Latinx Alone%: ',round(right_lay$hispanic_pop/right_lay$total_population*100,2)),
                 #paste0('# Under 100% Poverty Level: ',right_lay$poverty_number),
                 #paste0('# Renter: ',right_lay$renter_number),
                 paste0('Median Income: ',right_lay$medi_income),sep = '<br/>'))

             
    }
    else{
      paste0("")
    }
    })
  
  output$author <- renderText({
      paste0("Powered by Yaoyao Cai and Cheng Ren")
    
  })
  output$source <- renderText({
    paste0("Source: ACS 2018")
    
  })
  output$la_change <- renderText({
    if(!is.null(input$select_tract)){
      right_lay = la_census %>% filter(census%in%c(input$select_tract))
      HTML(paste(paste0('Current Boundary Information (Sum)'),
                 paste0('Total Population: ',sum(right_lay$total_population,na.rm=T)),
                 #paste0('# Limited English: ',sum(dt_census$limited_english_totpop,right_lay$limited_english_totpop,na.rm=T)), 
                 paste0('% White Alone: ',round(sum(right_lay$white_only,na.rm=T)/sum(right_lay$total_population,na.rm=T)*100,2)),
                 paste0('% African American Alone: ',round(sum(right_lay$aa_pop,na.rm=T)/sum(right_lay$total_population,na.rm=T)*100,2)),
                 paste0('% API: ',round(sum(right_lay$api_pop,na.rm=T)/sum(right_lay$total_population,na.rm=T)*100,2)),
                 paste0('% Asian Alone: ',round(sum(right_lay$asian_pop,na.rm=T)/sum(right_lay$total_population,na.rm=T)*100,2)),
                 paste0('% Latinx Alone: ',round(sum(right_lay$hispanic_pop,na.rm=T)/sum(right_lay$total_population,na.rm=T)*100,2)),
                 #paste0('# Under 100% Poverty Level: ',sum(dt_census$poverty_number,right_lay$poverty_number,na.rm=T)),
                 #paste0('# Renter: ',sum(dt_census$renter_number,right_lay$renter_number,na.rm=T)),
                 paste0('Median Income(mean): ',round(sum(right_lay$medi_income,na.rm=T)/(nrow(right_lay)),2)),
                 paste0('# Select Census: ',nrow(right_lay)),sep = '<br/>'))
      
      
    }
    else{
      HTML(paste(paste0('Current Boundary Information (Sum)'),
                 paste0('Total Population: ',0),
                 #paste0('# Limited English: ',sum(dt_census$limited_english_totpop,na.rm=T)), 
                 paste0('% White Alone: ',0),
                 paste0('% African American Alone: ',0),
                 paste0('% API: ',0),
                 paste0('% Asian Alone: ',0),
                 paste0('% Latinx Alone: ',0),
                 #paste0('# Under 100% Poverty Level: ',sum(dt_census$poverty_number,na.rm=T)),
                 #paste0('# Renter: ',sum(dt_census$renter_number,na.rm=T)),
                 paste0('Median Income(mean): ',0),
                 paste0('# Select Census: ',0), sep = '<br/>'))
    }
  })
  
  observeEvent(list(input$select_layer,
                    input$select_tract),{
                      leafletProxy("main_map") %>%
                        #clearGroup('Donwtown') %>%
                        clearGroup("Tract") %>%
                        #clearGroup("Base Tract") %>%
                        # addPolygons(data = update_dataset(),
                        #             fillColor = ~quantcolors()(
                        #               if(input$select_layer == 'Total Population'){total_population}
                        #               else if(input$select_layer == '# Limited English'){limited_english_totpop}
                        #               else if(input$select_layer == '# African American Alone'){aa_pop}
                        #               else if(input$select_layer == '# API'){api_pop}
                        #               else if(input$select_layer == '# Asian Alone'){asian_pop}
                        #               else if(input$select_layer == '# Foreign Born'){foreign_born}
                        #               else if(input$select_layer == '# Latix Alone'){hispanic_pop}
                        #               else if(input$select_layer == '# Renter'){renter_number}
                        #               else if(input$select_layer == '# Under 100% Poverty Level'){poverty_number}
                        #               else if(input$select_layer == 'Median Income'){medi_income}
                        #               else if(input$select_layer == '# White Alone'){white_only}),
                        #             group='Tract',
                        #             fillOpacity = 0.7,
                        #             color = "grey",
                        #             weight = 2,
                        #             layerId = ~GEOID,
                        #             popup = ~census,
                        #             options = pathOptions(pane = "Tract"))%>%
                        addPolygons(data = update_dataset_census(),
                                   fillColor = ~quantcolors()(
                                     if(input$select_layer == 'Total Population'){total_population}
                                     else if(input$select_layer == 'Nothing'){Nothing}
                                     #else if(input$select_layer == '# Limited English'){limited_english_totpop}
                                     else if(input$select_layer == '# African American Alone'){aa_pop}
                                     else if(input$select_layer == '# API'){api_pop}
                                     else if(input$select_layer == '# Asian Alone'){asian_pop}
                                     #else if(input$select_layer == '# Foreign Born'){foreign_born}
                                     else if(input$select_layer == '# Latinx Alone'){hispanic_pop}
                                     #else if(input$select_layer == '# Renter'){renter_number}
                                     #else if(input$select_layer == '# Under 100% Poverty Level'){poverty_number}
                                     else if(input$select_layer == 'Median Income'){medi_income}
                                     else if(input$select_layer == '# White Alone'){white_only}),
                                   group='Tract',
                                   fillOpacity = 0.7,
                                   color = "grey",
                                   weight = 2,
                                   highlightOptions = highlightOptions(stroke = 4, weight = 3,
                                                                       color = 'black'),
                                   layerId = ~NAME,
                                   popup = ~census,
                                   options = pathOptions(pane = "Tract"))
                        
                    }
  )
  
}
cat(file=stderr(), "After Rendering Map", "\n")
  
shinyApp(ui, server)
  