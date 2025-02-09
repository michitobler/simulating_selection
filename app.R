library(shiny)
library(learnPopGen)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Selection Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("start_freq", "Starting Allele Frequency (p0)", min = 0, max = 1, value = 0.5),
      numericInput("fitness_AA", "Fitness of homozygous AA", value = 1, min = 0, max = 2),
      numericInput("fitness_Aa", "Fitness of heterozygous Aa", value = 0.9, min = 0, max = 2),
      numericInput("fitness_aa", "Fitness of homozygous aa", value = 0.9, min = 0, max = 2),
      numericInput("time", "Time (Generations)", value = 50, min = 1, max = 1000),
      actionButton("run_sim", "Run Simulation")
    ),
    
    mainPanel(
      plotOutput("pop_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$run_sim, {
    # Parameters from the input
    p0 <- input$start_freq
    w <- c(input$fitness_AA, input$fitness_Aa, input$fitness_aa)
    time <- input$time
    
    # Run the selection simulation
    res <- selection(p0 = p0, w = w, time = time)
    
    # Create a dataframe for plotting
    r.table <- data.frame(Time = 1:time, p = res$p)
    
    # Plot the results
    output$pop_plot <- renderPlot({
      ggplot(r.table, aes(x = Time, y = p)) +
        geom_line(color = "blue") +
        labs(x = "Time", y = "Allele Frequency (p)") +
        theme_classic()+
        ggtitle(paste("p0 =", p0, 
                      ", w_AA =", w[1], 
                      ", w_Aa =", w[2], 
                      ", w_aa =", w[3]))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
