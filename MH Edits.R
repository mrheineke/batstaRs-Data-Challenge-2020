#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(cowplot)
library(forcats)
library(scales)

### Data Import and Cleaning
#Download filtered data to desktop as .csv. This is only PropertyType = C/I
setwd("~/Desktop")
LA_Data <- read.csv("Assessor_Parcels_Data_-_2006_thru_2019.csv")

##MH: removing non-essential columns to ease formatting
LA_Data <- LA_Data %>% select(GeneralUseType, LandBaseYear, SpecificUseType, netTaxableValue, RollYear)

# Make factors
LA_Data$GeneralUseType <- as.factor(LA_Data$GeneralUseType) #change General Use Type from character to factor (so that we can sort by these categories later)
LA_Data$SpecificUseType <- as.factor(LA_Data$SpecificUseType) #change Specific USe Type from character to factor
LA_Data$SpecificUseType <-  fct_explicit_na(LA_Data$SpecificUseType, na_level = "Missing") #recode NAs as Missings to make sorting easier

#the original data set had 39 different categories, which was too many for our purposes so we made consilidated broader categories
LA_Data$SpecificUseType <- fct_collapse(LA_Data$SpecificUseType, 
                                        Sports_and_Recreation = c("Race Track", "Athletic and Amusement Facility", "Bowling Alley", "Golf Course", "Skating Rink", "Water Recreation", "Club, Lodge Hall, Fraternal Organization"),
                                        Retail = c("Shopping Center (Regional)", "Department Store", "Shopping Center (Neighborhood, Community)", "Store Combination", "Store", "Nursery or Greenhouse", "Non-Auto Service and Repair Shop, Paint Shop, or Laundry", "Commercial"),
                                        Food_Processing_and_Distribution = c("Food Processing Plant", "Supermarket", "Restaurant, Cocktail Lounge"),
                                        Entertainment = c("Motion Picture, Radio and Television Industry", "Theater"),
                                        Manufacturing = c("Heavy Manufacturing", "Wholesale and Manufacturing Outlet", "Light Manufacturing", "Service Station", "Lumber Yard", "Auto, Recreation Equipment, Construction Equipment Sales and Service", "Industrial"),
                                        Professional_Buildings_and_Offices = c("Office Building", "Bank, Savings and Loan", "Professional Building"),
                                        Mineral_Processing = "Mineral Processing",
                                        Lodging = "Hotel and Motel",
                                        Parking = c("Parking Lot (Commercial Use Property)", "Parking Lot (Industrial Use Property)"),
                                        Storage = c("Warehousing, Distribution, Storage", "Open Storage"),
                                        Other = c("Camp", "(Missing)", "(unavailable)", "Missing", "Animal Kennel", "", " ") #MH: added blank option
)

#remove extraneous rows that have LandBaseYear as 0 
LA_Data <- subset(LA_Data, LandBaseYear !=0)
#Subset the data to be the most recent land assessment list
LA_Data_Current <- LA_Data %>% 
  filter(RollYear == 2019)

#list the years in descending order so that when we call the dropdown menu it's in a sensible order
LA_Data_Current <- LA_Data_Current[order(-LA_Data_Current$LandBaseYear),]

#Now, that the data is cleaned up, we are going to begin writing the ShinyApp, which will produce an interactive graph
#It requires two separate entities: the User Interface (UI) and the Server
## UI is the first step in building the app
##Our UI contains three items: the Title, a Side Panel, and the Barplot


# Use a fluid Bootstrap layout
##fluid bootstrap layout auto decides how big to make each part of the page based on the resolution of each image, 
##this makes it easier than having to define the size of each individual component 

ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Net Taxable Value of Commercial and Industrial Properties in LA County"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("LandBaseYear", "Year:", #the first argument is what the category is called in the data, the second is what you want it to print on the page as
                  choices=unique(LA_Data_Current$LandBaseYear)), #instead of listing each year out individually, it pulls the unique values from the LandBaseYear and makes it an option
      hr(),
      helpText("Data from LA County Assessor updated last in 2019.") #not required, but further explains what the data is
    ),
    
    # Create a spot for the barplot, this will be the larger part of the page
    mainPanel(
      plotOutput("BarPlot")  
    )
    
  )
)

## Server
## The Server is where you create the information that feeds into the UI  
# Define a server for the Shiny app
server <- function(input, output) {
  
  #filter the data to only contain the Base Year the User chose
  #here we use a temporary reactive function to filter the data to only include the Year the User is interested in 
  filtered_data <- reactive({ 
    filter(LA_Data_Current, LandBaseYear == input$LandBaseYear)
  })
  
  # Fill in the spot we created for a plot
  output$BarPlot <- renderPlot({
    filtered_data() %>% 
      group_by(SpecificUseType) %>% #group the netTaxableValue by the Specific Use Type 
      ggplot(data = filtered_data(),mapping = aes(reorder(SpecificUseType, -netTaxableValue), netTaxableValue)) +
      geom_bar(stat = 'identity') +
      theme_classic() +
      labs(y = "Mean Net Taxable Value", x = "", title = "Mean Net Taxable Value Across Specific Use Types in LA County" ) +
      theme(axis.text.y = element_text(size = 10, angle = 30)) +
      scale_y_continuous(labels = comma) +
      coord_flip() 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


### A second Shiny App option

## UI
ui2 <- fluidPage(    
  plotOutput("plot", click = "plot_click"),
  tableOutput("Data")
)

## Server

# Define a server for the Shiny app
server2 <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(data = LA_Data_Current, aes(x=LandBaseYear, y = netTaxableValue, fill = SpecificUseType)) + 
      geom_bar(stat = 'identity', position = "stack") + 
      xlim(1975,2020) +
      labs(x = "Land Assessment Year", y = "Net Taxable Value (USD)", fill = "Specific Use Type", title = "Commercial and Industrial Property Value in LA County") +
      theme_cowplot() +
      theme(legend.title = element_text(size = 5), 
            legend.text = element_text(size = 5)) +
      guides(color = guide_legend(override.aes = list(size = 1))) })
  
  output$Data <- renderTable({
    nearPoints(LA_Data_Current, input$plot_click, xvar = LA_Data_Current$SpecificUseType, yvar=sum(LA_Data_Current$netTaxableValue))
  })
}

# Run the application 
shinyApp(ui = ui2, server = server2)
