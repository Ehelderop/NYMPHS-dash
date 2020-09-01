library(shinyWidgets)
library(leaflet)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(useful)
library(scatterplot3d)

#format for sidebar titles
css <- "#id+div div a {color: black;}"

ui = dashboardPage(
  #running head of page
  dashboardHeader(title = "NYMPHS dash"),
  
  #sidebar items
  dashboardSidebar(width='450px',
                   tags$head(tags$style(css)),
                   sidebarMenu(id = 'sidebar',
                               menuItem('Data Upload', icon=icon('table'), tabName = 'dashboard'),
                               menuSubItem('Pollen',tabName='upload1'),
                               div(
                                 fileInput('file1','Choose pollen mix CSV',
                                           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                               ),
                               menuSubItem('(Optional) Sites',tabName='upload2'),
                               div(
                                 fileInput('file2','(Optional) candidate site CSV',
                                           accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
                               ),
                               
                               menuItem('Common pollen species', icon=icon('line-chart'), tabName = 'Pollens'),
                               div(multiInput(inputId = 'id', label = "Select pollens:",
                                              choices = c('Plant1','Plant2','Plant3','Plant4','Plant5',
                                                          'Plant6','Plant7','Plant8','Plant9','Plant10'),
                                              selected=NULL, width='400px',
                                              options = list(enable_search=F))
                               ),
                               
                               menuItem('Site types', icon=icon('globe'), tabName='Sites'),
                               div(radioButtons('siteOpts', label=h5("Select site types"), c("Cities" = "cities",
                                                                                             "States" = "states",
                                                                                             "Regions" = "regions",
                                                                                             "Countries" = "countries"))
                               ),
                               
                               actionButton('gofnd', 'Run GOFIND')
                   )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard', class="active",
              fluidRow(
                box(
                  title = "Texas candidate sites",
                  status = 'primary',
                  solidHeader=T,
                  collapsible = T,
                  leafletOutput('t_map')
                ),
                
                box(
                  title = "Site network",
                  status='warning',
                  solidHeader=T,
                  collapsible=T,
                  plotOutput('plot1')
                ),
              ),
              
              fluidRow(
                box(
                  title="Site probability ranking",
                  collapsible=T,
                  tableOutput('table1')
                ),
                
                box(
                  title='Controls',
                  sliderInput('slider','Number of sites:',1,30,15)
                )
              )
      )
    )
  )
)

server = function(input,output) {
  set.seed(122)
  
  df = data.frame(replicate(4, sample(10:35,200,rep=T)))
  colnames(df) = c('ID','x','y','z')
  df$z = sample(1:3,200,rep=T)
  
  output$plot1 <- renderPlot({
    inpCo = length(seq_len(input$slider))
    df = df[sample(nrow(df),inpCo),]
    colors = c('steelblue','green','black')
    colors = colors[as.numeric(df$z)]
    sp3d = scatterplot3d(df$x,df$y,df$z, angle=15,
                         main='Test 3D',
                         xlab = 'Longitude',
                         ylab = 'Latitude',
                         zlab = 'Level',
                         pch=16,
                         cex.symbols = 2,
                         color=colors,
                         grid = T,
                         box = F)
    
    sp3d$points3d(x=df$x,y=df$y,z=df$z,type='l',col='grey',lwd=1)
  })
  
  output$t_map = renderLeaflet({
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.Mapnik",
                       options = providerTileOptions(minZoom=5, maxZoom=11)) %>% #default openstreetmaps
      addMarkers(lng = -95.37, lat = 29.76, popup="Houston") %>%
      addMarkers(lng = -98.49, lat = 29.42, popup="San Antonio") %>%
      addMarkers(lng = -106.49, lat = 31.76, popup="El Paso") %>%
      setView(lng = -99, lat = 32, zoom=5) %>%
      setMaxBounds(lng1 = -110, lat1 = 34, lng2 = -93, lat2 = 28)
  })
  
  df2 = as.data.frame(matrix(0,
                             nrow = 5,
                             ncol = 3))
  colnames(df2) = c('Site','Probability','p_value')
  df2$Site = c('San Antonio','Houston','Dallas','El Paso','Austin')
  df2$Probability = c('0.88','0.4','0.02','0.01','0.01')
  df2$p_value = c('<0.01','0.04','0.22','0.3','0.41')
  
  observeEvent(input$gofnd, {
    output$table1 = renderTable(df2)
  })
  
}

shinyApp(ui,server)