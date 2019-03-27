#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source('mgmt call report Dustin.R')

# Define UI for application that draws a histogram
ui <- navbarPage("RH Call Reporting",
                 tabPanel("All Calls",
                          mainPanel(
                            fluidRow(     
                              column(width = 6,plotOutput(outputId = "AllThisW.LastWPlot")), 
                                     column(width = 6,plotOutput(outputId = "All4w.4wPlot"))), 
                                            fluidRow(     
                                              column(width = 6,plotOutput(outputId = "All13W.13wPlot")), 
                                              column(width = 6,plotOutput(outputId = "AllQ.QPlot")))
                          )
                 ),
                 tabPanel("Calls by Region",
                          selectInput(inputId = "Region", 
                                      label = "Region:",
                                      choices =  levels(as.factor(RegQ.Q$Region)), 
                                      selected = "Northeast"),
                          mainPanel(
                            fluidRow(     
                              column(width = 6, plotOutput(outputId = "RegThisW.LastWPlot")), 
                                     column(width = 6,plotOutput(outputId = "Reg4w.4wPlot"))), 
                            fluidRow(     
                              column(width = 6,plotOutput(outputId = "Reg13W.13wPlot")), 
                                     column(width = 6,plotOutput(outputId = "RegQ.QPlot")))
                          )
                 ),
                 tabPanel("Calls by Territory",
                          selectInput(inputId = "Territory", 
                                      label = "Territory:",
                                      choices = levels(as.factor(TerrQ.Q$`Territory Name`)),
                                        selected = "Albany"),
                          mainPanel(
                            fluidRow(
                              column(width = 6,plotOutput(outputId = "TerrThisW.LastWPlot")), 
                              column(width = 6,plotOutput(outputId = "Terr4w.4wPlot"))), 
                            fluidRow(     column(width = 6,plotOutput(outputId = "Terr13W.13wPlot")), 
                                            column(width = 6,plotOutput(outputId = "TerrQ.QPlot")))
                          )
                 )
                 
                 
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$AllThisW.LastWPlot <- renderPlot({ggplot(Allw.w, aes(y=CallCount, x=Prod, color=ThisWeekVLastWeek, fill=ThisWeekVLastWeek)) +
    geom_bar( stat="identity", position=position_dodge()) +    
    facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                                position = position_dodge(1), size=3.5)+labs(title="This Week v Last Week") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
  })
  
  output$All4w.4wPlot <- renderPlot({ggplot(All4w.4w, aes(y=CallCount, x=Prod, color=ThisFourWeekVLastFourWeek, fill=ThisFourWeekVLastFourWeek)) + 
    geom_bar( stat="identity", position=position_dodge()) +    
    facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                                position = position_dodge(1), size=3.5)+labs(title="This 4 Weeks v Last 4 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
output$All13W.13wPlot <- renderPlot({ggplot(All13W.13w, aes(y=CallCount, x=Prod, color=ThisThirteenWeekVLastThirteenWeek, fill=ThisThirteenWeekVLastThirteenWeek)) + 
    geom_bar( stat="identity", position=position_dodge()) +    
    facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                                position = position_dodge(1), size=3.5)+labs(title="This 13 Weeks v Last 12 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
output$AllQ.QPlot <- renderPlot({ggplot(AllQ.Q, aes(y=CallCount, x=Prod, color=ThisQVLastQ, fill=ThisQVLastQ)) + 
    geom_bar( stat="identity", position=position_dodge()) +    
    facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                                position = position_dodge(1), size=3.5)  +labs(title="This QTD v Last QTD")   +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})

output$RegThisW.LastWPlot <- renderPlot({ggplot(subset(RegW.W, Region == input$Region), aes(y=CallCount, x=Prod, color=ThisWeekVLastWeek, fill=ThisWeekVLastWeek)) +
  geom_bar( stat="identity", position=position_dodge()) +    
  facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                              position = position_dodge(1), size=3.5)+labs(title="This Week v Last Week") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
output$Reg4w.4wPlot <- renderPlot({ggplot(subset(Reg4w.4w, Region == input$Region), aes(y=CallCount, x=Prod, color=ThisFourWeekVLastFourWeek, fill=ThisFourWeekVLastFourWeek)) + 
  geom_bar( stat="identity", position=position_dodge()) +    
  facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                              position = position_dodge(1), size=3.5)+labs(title="This 4 Weeks v Last 4 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
output$Reg13W.13wPlot <- renderPlot({ggplot(subset(Reg13W.13w, Region== input$Region), aes(y=CallCount, x=Prod, color=ThisThirteenWeekVLastThirteenWeek, fill=ThisThirteenWeekVLastThirteenWeek)) + 
  geom_bar( stat="identity", position=position_dodge()) +    
  facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                              position = position_dodge(1), size=3.5)+labs(title="This 13 Weeks v Last 12 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
output$RegQ.QPlot <- renderPlot({ggplot(subset(RegQ.Q, Region == input$Region), aes(y=CallCount, x=Prod, color=ThisQVLastQ, fill=ThisQVLastQ)) + 
  geom_bar( stat="identity", position=position_dodge()) +    
  facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                              position = position_dodge(1), size=3.5)+labs(title="This QTD v Last QTD") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})


output$TerrThisW.LastWPlot <- renderPlot({ggplot(subset(TerrW.w, `Territory Name`== input$Territory), aes(y=CallCount, x=Prod, color=ThisWeekVLastWeek, fill=ThisWeekVLastWeek)) +
  geom_bar( stat="identity", position=position_dodge()) +    
  facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                              position = position_dodge(1), size=3.5)+labs(title="This Week v Last Week") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
output$Terr4w.4wPlot <- renderPlot({ggplot(subset(Terr4w.4w, `Territory Name` == input$Territory), aes(y=CallCount, x=Prod, color=ThisFourWeekVLastFourWeek, fill=ThisFourWeekVLastFourWeek)) + 
  geom_bar( stat="identity", position=position_dodge()) +    
  facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                              position = position_dodge(1), size=3.5)+labs(title="This 4 Weeks v Last 4 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
output$Terr13W.13wPlot <- renderPlot({ggplot(subset(Terr13W.13w, `Territory Name` == input$Territory), aes(y=CallCount, x=Prod, color=ThisThirteenWeekVLastThirteenWeek, fill=ThisThirteenWeekVLastThirteenWeek)) + 
  geom_bar( stat="identity", position=position_dodge()) +    
  facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                              position = position_dodge(1), size=3.5)+labs(title="This 13 Weeks v Last 12 Weeks") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
output$TerrQ.QPlot <- renderPlot({ggplot(subset(TerrQ.Q, `Territory Name` == input$Territory), aes(y=CallCount, x=Prod, color=ThisQVLastQ, fill=ThisQVLastQ)) + 
  geom_bar( stat="identity", position=position_dodge()) +    
  facet_wrap(~Type)+ coord_flip() + geom_text(aes(label=CallCount), vjust=0, color = "black",
                                              position = position_dodge(1), size=3.5)+labs(title="This QTD v Last QTD") +theme(plot.title = element_text(hjust = 0.5),legend.position="bottom", legend.title=element_blank())
})
}
# Run the application 
shinyApp(ui = ui, server = server)

