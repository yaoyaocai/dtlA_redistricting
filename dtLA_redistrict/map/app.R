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
                                        top = 100, left = 55, width = 470, fixed=TRUE,
                                        draggable = FALSE, height = 'auto',
                                        #img(src="logo.png",height=37,width=110,align = "left"),
                                        pickerInput("select_layer", label = h4("Layer Selection"), inline = F, 
                                                    selected = "Total Population",width='100%',
                                                    choices = sort(c(unique(as.character(census$var_name)))),
                                                    choicesOpt = list(
                                                      style = rep(("color:black; font-size: 110%;"), 56)),
                                                    options = list(liveSearch = TRUE)),
                                        selectizeInput("select_tract",label = "Tract Selection", multiple = T,
                                                       choices = sort(as.character(exp_census$census), decreasing = F),width='100%',
                                                       options = list(placeholder = "Select Tracts", 'plugins' = list('remove_button'))
                                        ),
                                        span(h4(htmlOutput("la_change"), align = "left"), style="color:#270180"),
                                        span(h6(htmlOutput("author"), align = "left"))
                                        
                          ),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 100, right = 55, width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        span(h4(htmlOutput("pro"), align = "left"), style="color:#270180"))
                                        
                                        
                          ),
                          absolutePanel(id = "logo", class = "card", bottom = 210, right = 20, width = 120, fixed=TRUE, draggable = FALSE, height = "auto",
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
      addMapPane("Council", zIndex = 410)%>%
      addMapPane("Downtown", zIndex = 405)%>%
      addMapPane("Tract", zIndex = 400) %>%   # Pane z-Index for polygons to stay underneath the markers
      #
      addLayersControl(
        baseGroups = c("Street Basemap","Plain Basemap"), 
        overlayGroups = c('Tract','Downtown','Council'),
        options = layersControlOptions(collapsed = F, autoZIndex = TRUE),
        position = "bottomright")  %>%
      addPolylines(data = dtla,
                   weight=5,
                   color = 'purple',
                   group = 'Downtown',
                   opacity = 0.8,
                   options = pathOptions(pane = "Downtown"))%>%
      addPolylines(data = council,
                   weight = 6,
                   color = 'green',
                   group = 'Council',
                   opacity = 0.8,
                   options = pathOptions(pane = "Council"))
    
    
    return(map1)
  }
  # Rendering the main map
  output$main_map <- renderLeaflet({make_leaflet_map()})
  
  #update the dataset
  update_dataset <- reactive({
    var <- as.character(census$my_cen_var_names[census$var_name==input$select_layer])
    data_set <- dt_census %>% select("GEOID","NAME",'census',var)
    return(data_set)})
  #update the census
  update_dataset_census <- reactive({
    var <- as.character(census$my_cen_var_names[census$var_name==input$select_layer])
    data_set_census <- exp_census %>% select("GEOID","NAME",'census',var)%>%
      filter(census%in%c(input$select_tract) )
    return(data_set_census)})
  # fill colors
  quantcolors <- reactive({
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
    if(input$select_layer == '# Latix Alone'){
      return(colorQuantile("Purples", la_census$hispanic_pop, n=6))}
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
  observeEvent(input$main_map_shape_click, {
    data_of_click$clickedGeography <- input$main_map_shape_click
    
    #print(data_of_click$clickedGeography)
    
    cat(file=stderr(), "Observed Geography Click", "\n")
    
    
  })
  
  output$pro <- renderText({
    if(!is.null(data_of_click$clickedGeography$id)){
      right_lay = la_census %>% filter(GEOID==data_of_click$clickedGeography$id)
      HTML(paste(paste0('Census Tract: ',right_lay$census),
                 paste0('Total Population: ',right_lay$total_population),
                 paste0('# Limited English: ',right_lay$limited_english_totpop), 
                 paste0('# White Alone: ',right_lay$white_only),
                 paste0('# African American Alone: ',right_lay$aa_pop),
                 paste0('# API: ',right_lay$api_pop),
                 paste0('# Asian Alone: ',right_lay$asian_pop),
                 paste0('# Latinx Alone: ',right_lay$hispanic_pop),
                 paste0('# Under 100% Poverty Level: ',right_lay$poverty_number),
                 paste0('# Renter: ',right_lay$renter_number),
                 paste0('Median Income: ',right_lay$medi_income),sep = '<br/>'))

             
    }
    else{
      paste0("")
    }
    })
  
  output$author <- renderText({
      paste0("Powered by Yaoyao Cai and Cheng Ren")
    
  })
  output$la_change <- renderText({
    if(!is.null(input$select_tract)){
      right_lay = la_census %>% filter(census%in%c(input$select_tract))
      HTML(paste(paste0('Current Boundary Information (Sum)'),
                 paste0('Total Population: ',sum(dt_census$total_population,right_lay$total_population,na.rm=T)),
                 paste0('# Limited English: ',sum(dt_census$limited_english_totpop,right_lay$limited_english_totpop,na.rm=T)), 
                 paste0('# White Alone: ',sum(dt_census$white_only,right_lay$white_only,na.rm=T)),
                 paste0('# African American Alone: ',sum(dt_census$aa_pop,right_lay$aa_pop,na.rm=T)),
                 paste0('# API: ',sum(dt_census$api_pop,right_lay$api_pop,na.rm=T)),
                 paste0('# Asian Alone: ',sum(dt_census$asian_pop,right_lay$asian_pop,na.rm=T)),
                 paste0('# Latinx Alone: ',sum(dt_census$hispanic_pop,right_lay$hispanic_pop,na.rm=T)),
                 paste0('# Under 100% Poverty Level: ',sum(dt_census$poverty_number,right_lay$poverty_number,na.rm=T)),
                 paste0('# Renter: ',sum(dt_census$renter_number,right_lay$renter_number,na.rm=T)),
                 paste0('Median Income(mean): ',round(sum(dt_census$medi_income,right_lay$medi_income,na.rm=T)/(nrow(dt_census)+nrow(right_lay)),2)),sep = '<br/>'))
      
      
    }
    else{
      HTML(paste(paste0('Current Boundary Information (Sum)'),
                 paste0('Total Population: ',sum(dt_census$total_population,na.rm=T)),
                 paste0('# Limited English: ',sum(dt_census$limited_english_totpop,na.rm=T)), 
                 paste0('# White Alone: ',sum(dt_census$white_only,na.rm=T)),
                 paste0('# African American Alone: ',sum(dt_census$aa_pop,na.rm=T)),
                 paste0('# API: ',sum(dt_census$api_pop,na.rm=T)),
                 paste0('# Asian Alone: ',sum(dt_census$asian_pop,na.rm=T)),
                 paste0('# Latinx Alone: ',sum(dt_census$hispanic_pop,na.rm=T)),
                 paste0('# Under 100% Poverty Level: ',sum(dt_census$poverty_number,na.rm=T)),
                 paste0('# Renter: ',sum(dt_census$renter_number,na.rm=T)),
                 paste0('Median Income(mean): ',round(mean(dt_census$medi_income,na.rm=T),2)),sep = '<br/>'))
    }
  })
  
  observeEvent(list(input$select_layer,
                    input$select_tract),{
                      leafletProxy("main_map") %>%
                        clearGroup('Donwtown') %>%
                        clearGroup("Tract") %>%
                        addPolygons(data = update_dataset(),
                                    fillColor = ~quantcolors()(
                                      if(input$select_layer == 'Total Population'){total_population}
                                      else if(input$select_layer == '# Limited English'){limited_english_totpop}
                                      else if(input$select_layer == '# African American Alone'){aa_pop}
                                      else if(input$select_layer == '# API'){api_pop}
                                      else if(input$select_layer == '# Asian Alone'){asian_pop}
                                      else if(input$select_layer == '# Foreign Born'){foreign_born}
                                      else if(input$select_layer == '# Latix Alone'){hispanic_pop}
                                      else if(input$select_layer == '# Renter'){renter_number}
                                      else if(input$select_layer == '# Under 100% Poverty Level'){poverty_number}
                                      else if(input$select_layer == 'Median Income'){medi_income}
                                      else if(input$select_layer == '# White Alone'){white_only}),
                                    group='Tract',
                                    fillOpacity = 0.7,
                                    color = "grey",
                                    weight = 2,
                                    layerId = ~GEOID,
                                    popup = ~census,
                                    options = pathOptions(pane = "Tract"))%>%
                        addPolygons(data = update_dataset_census(),
                                   fillColor = ~quantcolors()(
                                     if(input$select_layer == 'Total Population'){total_population}
                                     else if(input$select_layer == '# Limited English'){limited_english_totpop}
                                     else if(input$select_layer == '# African American Alone'){aa_pop}
                                     else if(input$select_layer == '# API'){api_pop}
                                     else if(input$select_layer == '# Asian Alone'){asian_pop}
                                     else if(input$select_layer == '# Foreign Born'){foreign_born}
                                     else if(input$select_layer == '# Latix Alone'){hispanic_pop}
                                     else if(input$select_layer == '# Renter'){renter_number}
                                     else if(input$select_layer == '# Under 100% Poverty Level'){poverty_number}
                                     else if(input$select_layer == 'Median Income'){medi_income}
                                     else if(input$select_layer == '# White Alone'){white_only}),
                                   group='Tract',
                                   fillOpacity = 0.7,
                                   color = "grey",
                                   weight = 2,
                                   layerId = ~GEOID,
                                   popup = ~census,
                                   options = pathOptions(pane = "Tract"))
                        
                    }
  )
  
}
cat(file=stderr(), "After Rendering Map", "\n")
  
shinyApp(ui, server)
  