##################################
# ui.R file                      #
##################################

library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics-bioNPS.js")),
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "green",
      
    dashboardHeader(title="Biodiversity in Zimbabwe National Parks", titleWidth = 300),
    
    dashboardSidebar(width = 300,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<a href='https://www.zimparks.org.zw/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='Zimparks-logo.png' width = '186'></a>",
          "<br>",
          "<p style = 'text-align: center;'><small><a href='https://www.zimparks.org.zw/' target='_blank'>Zimbabwe National Parks</a></small></p>",
          "<br>"
        )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Parks Map", tabName = "map", icon = icon("thumbtack")),
        menuItem("Species Tables", tabName = "table", icon = icon("table")),
        menuItem("Species Tree", tabName = "tree", icon = icon("random", lib = "glyphicon")),
        menuItem("Species Charts", tabName = "charts", icon = icon("stats", lib = "glyphicon")),
        menuItem("Species Choropleth Map", tabName = "choropleth", icon = icon("map marked alt")),
        
        menuItem("About Us", tabName = "releases", icon = icon("tasks")),
        HTML(paste0(
          "<br><br><br><br><br><br><br><br><br>",
          "<table style='margin-left:auto; margin-right:auto;'>",
            "<tr>",
              "<td style='padding: 5px;'><a href='https://https://www.facebook.com/Ministry-of-Health-and-Child-Care-148565931913609' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://https://www.youtube.com/watch?v=cHoKpXBRYcY' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://https://twitter.com/MoHCCZim' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
              "</tr>",
          "</table>",
          "<br>"))
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
          
          # home section
         # HTML(paste0(
           #"<br>",
          #  "<a href='https://www.stanbicbank.co.zw/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='homepic.png' width = '1500'></a>",
          #  "<br>"
         #))
        includeMarkdown("www/home.md")
          
        ),
        
        tabItem(tabName = "map",
        
          # parks map section
          leafletOutput("parksMap") %>% withSpinner(color = "green")
                
        ),
        
        tabItem(
          # species data section
          tabName = "table", dataTableOutput("speciesDataTable") %>% withSpinner(color = "green")
          
        ),
        
        tabItem(tabName = "tree", 
              
          # collapsible species tree section
          includeMarkdown("www/tree.md"),
          column(3, uiOutput("parkSelectComboTree")),
          column(3, uiOutput("categorySelectComboTree")),
          collapsibleTreeOutput('tree', height='700px') %>% withSpinner(color = "green")
          
        ),
      
        tabItem(tabName = "charts",
          
          # ggplot2 species charts section
          includeMarkdown("www/charts.md"),
          fluidRow(column(3, uiOutput("categorySelectComboChart"))),
          column(6, plotOutput("ggplot2Group1") %>% withSpinner(color = "green")),
          column(6, plotOutput("ggplot2Group2") %>% withSpinner(color = "green"))
          
        ), 
        
        tabItem(tabName = "choropleth",
          
          # choropleth species map section
          includeMarkdown("www/choropleth.md"),
          fluidRow(
            column(3, uiOutput("statesSelectCombo")),
            column(3, uiOutput("categorySelectComboChoro"))
          ),
          fluidRow(
            column(3,tableOutput('stateCategoryList') %>% withSpinner(color = "green")),
            column(9,leafletOutput("choroplethCategoriesPerState") %>% withSpinner(color = "green"))
          )
          
        ),
        
        tabItem(tabName = "releases", includeMarkdown("www/releases.md"))
      
      )
    ) # end dashboardBody
  
  )# end dashboardPage

))