# Load necessary libraries
library(shiny)
library(sqldf)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(grid)


midwest2 <- df
midwestNew <- sqldf(c(
  "Alter table midwest2 ADD MW_states varchar(15)",
  
  "Select * from midwest2"
))


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Midwest State Demography Distribution"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Stat",
        label = h4("Select Midwest State for Demographic details"),
        choices = list("IL", "MI", "IN", "WI", "OH"),
        selected = "IL"
      )
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw plots
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Query to get population by race
    popQuery <- sqldf(paste0(
      "select sum(popwhite),sum(popblack),sum(popamerindian), sum(popasian), sum(popother), MW_states   
         from midwestNew where MW_states ='", input$Stat, "';"
    ))
    
    # Query to get poverty and college education
    popQuery2 <- sqldf(paste0(
      "select percbelowpoverty, percollege, MW_states   
         from midwestNew where MW_states ='", input$Stat, "';"
    ))
    
    # Query to get average percentage by race
    popQuery3 <- sqldf(paste0(
      "select avg(percwhite), avg(percblack), avg(percamerindan), avg(percasian),
                    avg(percother), MW_states   
                     from midwestNew where MW_states ='", input$Stat, "';"
    ))
    
    # Melt data for plotting
    m <- melt(popQuery, id.vars = "MW_states", variable.name = "Race", value.name = "Population_by_Race")
    n <- melt(popQuery3, id.vars = "MW_states", variable.name = "Race", value.name = "Percentage_by_Population")
    
    # Create plots
    w <- ggplot(data = m, aes(x = MW_states, y = Population_by_Race, fill = Race)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Population by Race", x = "State", y = "Population") +
      scale_fill_manual(values = c("popwhite" = "blue", "popblack" = "red", "popamerindan" = "green", "popasian" = "pink", "popother" = "orange"))
    
    p <- ggplot(data = popQuery2, aes(y = percbelowpoverty, x = percollege, color = MW_states)) + 
      geom_point() + ggtitle("College Education Vs Total Poverty") +
      xlab("Percent College Educated") + ylab("Percentage of Total poverty")
    
    z <- ggplot(data = n, aes(x = "", y = Percentage_by_Population, fill = Race)) + 
      geom_bar(stat = "identity", color = 'black') + 
      geom_text(aes(label = scales::percent(Percentage_by_Population)), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") + 
      guides(fill = guide_legend(override.aes = list(colour = NA))) +
      labs(title = "Percentage Distribution of Races")
    
    # Print the plots
    pushViewport(viewport(layout = grid.layout(3, 1), width = 0.75, height = 1))
    print(w, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(z, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  })
}

# Run the app
shinyApp(ui, server)
