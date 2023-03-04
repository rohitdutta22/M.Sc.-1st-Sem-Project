#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(imager)
library(ggplot2)
install.packages("patchwork")      ## Installing library  patchwork
library(patchwork)
install.packages("shinythemes")   ## Installing library shinythemes
library(shinythemes)
load("Final_DATA_GRP3.Rdata")
load("col_hex.Rdata")
load("color_names.Rdata")
attach(final_data_grp3)

# Define UI for application that draws a histogram
ui <- navbarPage( theme = shinytheme("flatly"),
  
  # Application title
  titlePanel(h1("Color Variation Analysis")),
  tabPanel(h3("Home"),
           sidebarLayout(
             sidebarPanel(
               selectInput("name", "Select Painter",
                           choices = unique(final_data_grp3$Painter)),
               
               
               
               selectInput("year", "Select year",
                           choices = NULL),
               
               selectInput("painting", "Choose Painting",
                           choices = NULL),
               checkboxGroupInput("colors", "Choose Colors",
                                  choices = (colnames(final_data_grp3))[11:26])
             ),
             mainPanel(
               h3(textOutput("top")),
               plotOutput("imgrplot"),
               h3(textOutput("mid")),
               plotOutput("densityplot"),
               h3(textOutput("low")),
               plotOutput("boxplot"),
               h3(textOutput("low1")),
               plotOutput("colplot")
             )
           )
  ),
  
  tabPanel(h3("Boxplots and Violin Plots"),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("name_multi", "Select Painter(s) for Comparison",
                                  choices = unique(final_data_grp3$Painter))
             ),
             mainPanel(
               h3(textOutput("low3")),
               plotOutput("medplot"),
               h3(textOutput("low2")),
               plotOutput("qdplot")
             )
           )
           
  ),
  
  tabPanel(h3("Color Proportions"),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("name_multi_2", "Select Painter(s) for Comparison",
                                  choices = unique(final_data_grp3$Painter)),
               checkboxGroupInput("colors1_2", "Choose Colors for comparison between Painters",
                                  choices = (colnames(final_data_grp3))[11:26])
             ),
             mainPanel(
               h3(textOutput("low4")),
               plotOutput("gencolplot")
             )
           )
    
  ),
  
  tabPanel(h3("Trend of Color Proportions"),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("name_multi_3", "Select Painter(s) for Comparison",
                                  choices = unique(final_data_grp3$Painter)),
               checkboxGroupInput("colors1_3", "Choose Colors for comparison between Painters",
                                  choices = (colnames(final_data_grp3))[11:26])
             ),
             mainPanel(
               h3(textOutput("low5")),
               plotOutput("yearplot")
             )
           )
           
  ),
  
  tabPanel(h3("Density Plot of Shades"),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("name_multi_4", "Select Painter(s) for Comparison",
                                  choices = unique(final_data_grp3$Painter)),
             ),
             mainPanel(
               h3(textOutput("low6")),
               plotOutput("shadeplot")
             )
           )
           
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  name <- reactive({
    filter(final_data_grp3, Painter == input$name)
  })
  observeEvent(name(), {
    choices <- unique(name()$Year)
    updateSelectInput(inputId = "year", choices = choices) 
  })
  
  year <- reactive({
    req(input$year)
    filter(name(), Year == input$year)
  })
  observeEvent(year(), {
    choices <- year()$Painting_Names
    updateSelectInput(inputId = "painting", choices = choices)
  })
  
  output$top <- renderText({
    paste("Painting Name: ",input$painting)
  })
  output$imgrplot <- renderPlot({
    a <- plot(load.image(final_data_grp3$Links[which(final_data_grp3$Painting_Names == input$painting)]))
    a
  })
  
  output$mid <- renderText({
    paste("Density Plot of Pixel Values of Three Color Channels Red, Green and Blue")
  })
  output$densityplot <- renderPlot({
    
    col.mat <- as.array((load.image(final_data_grp3$Links[which(final_data_grp3$Painting_Names == input$painting)]))[,,1,])
    
    plot(density(col.mat[,,1]), main = "",
         xlab = "Intensity (Pixel Values of the Color Channels)", ylab = "Density",col = 2,type = "l" ,lwd = 2, cex.lab = 1.6, cex.axis = 1.5,
         ylim = c(0,max(density(col.mat[,,1])$y) + 4),
         xlim = c(-0.05,1.05))
    lines(density(col.mat[,,2]),
          col = 3, lwd = 2)
    lines(density(col.mat[,,3]),
          col = 4, lwd = 2)
  })
  
  output$low <- renderText({
    paste("Boxplot showing the Variation in the Pixel Values of Three Color Channels Red, Green and Blue")
  })
  
  output$boxplot <- renderPlot({
    col.mat <- as.array((load.image(final_data_grp3$Links[which(final_data_grp3$Painting_Names == input$painting)]))[,,1,])
    
    pixels = c(as.vector(col.mat[,,1]) , as.vector(col.mat[,,2]) , as.vector(col.mat[,,3]))
    
    category = c(rep("Red",length(as.vector(col.mat[,,1]))) , rep("Green",length(as.vector(col.mat[,,2]))) , rep("Blue",length(as.vector(col.mat[,,3]))))
    
    p_c_data = data.frame(Category = category , Pixels = pixels )
    
    ggplot(p_c_data, aes(x = Category, y = Pixels, fill = category)) +
      geom_boxplot(width = 0.35,alpha = 0.4, show.legend = F, outlier.Color = "black", outlier.fill = "white") +
      scale_color_manual(values = c("blue","green","red"))+
      scale_fill_manual(values = c("blue","green","red"))+
      labs(y = "RGB Values", x = "")+
      theme(axis.title.x = element_text(face = 'italic'), axis.text.x = element_text(size = 20))+
      theme(axis.title.y = element_text(face = 'italic', angle = 90),axis.title.y.left = element_text(size = 20),
            axis.text.y = element_text(size = 15))
    
  })
  
  output$low1 <- renderText({
    paste("Color Proportions of Different Colors")
  })
  
  output$colplot <- renderPlot({
    dat <- final_data_grp3[which(final_data_grp3$Painting_Names == input$painting),]
    
    props <- array(0)
    hex_code <- array(0)
    for(i in 1:length(input$colors)){
      props[i] <- dat[1,which(colnames(dat) == (input$colors)[i])]
      hex_code[i] <- col.hex[which(col.names == (input$colors)[i])]
    }
    df.temp <- data.frame(input$colors,props,hex_code)
    colnames(df.temp) <- c("multicol","proportions","hex")
    df.temp = df.temp[order(df.temp$multicol),]
    
    ggplot(df.temp, aes(x = multicol, y = proportions, size = (2*proportions)))+
      geom_point(aes(color = multicol))+
      labs(x = "", y = "Values of Color Proportions", color = "Colors", subtitle  = "Size of each point corresponds to twice of the color proportions")+
      scale_color_manual(values = df.temp$hex)+
      theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), legend.title = element_text(size = 16),
            legend.text = element_text(size = 14), plot.subtitle = element_text(size = 15))+
      ylim(0,1)
    
  })
  
  output$low2 <- renderText({
    paste("Plot Showing the Distribution of Quartile Deviations (QD) and their Dispersion of Three Color Channels Red, Green and Blue, Considering All Images of a Painter")
  })
  
  output$qdplot = renderPlot({
    
    p = list()
    
    for(i in 1:length(input$name_multi)){
      dat <- filter(final_data_grp3, final_data_grp3$Painter == input$name_multi[i])
      all_qd = c(as.vector(dat$Red_QD), as.vector(dat$Green_QD), as.vector(dat$Blue_QD))
      
      category = c(rep("Red",length(dat$Red_QD)) , rep("Green",length(dat$Green_QD)) , rep("Blue",length(dat$Blue_QD)))
      
      qd_data = data.frame(Category = category , QD = all_qd )
      
      p[[i]] <-  ggplot(qd_data, aes(x = Category, y = QD, fill = Category)) +
        geom_violin( alpha = 0.1, lwd = 1.2 , trim=FALSE, aes(color = Category, fill = Category), show.legend = F) +
        geom_boxplot(width=0.35, alpha = 0.5, show.legend = F) +
        scale_color_manual(values = c("blue","green","red"))+
        scale_fill_manual(values = c("blue","green","red"))+
        labs(x= (input$name_multi)[i], y = "Values of QD")+
        theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 15),
              axis.text.x = element_text(size = 15),axis.title.x = element_text(size = 20))+
        ylim(-0.2,0.55)
      
    }
    if(length(input$name_multi) == 1){
      p[[1]]
    }else if(length(input$name_multi) == 2){
      p[[1]] + p[[2]]
    }else if(length(input$name_multi) == 3){
      p[[1]] + p[[2]] + p[[3]]
    }else{
      p[[1]] + p[[2]] + p[[3]] + p[[4]]
    }
    
  })
  
  output$low3 <- renderText({
    paste("Plot Showing the Distribution of Medians and their Dispersion of Three Color Channels Red, Green and Blue, Considering All Images of a Painter")
  })
  
  output$medplot = renderPlot({
    
    p = list()
    
    for(i in 1:length(input$name_multi)){
      dat <- filter(final_data_grp3, final_data_grp3$Painter == input$name_multi[i])
      all_qd = c(as.vector(dat$Red_Median), as.vector(dat$Green_Median), as.vector(dat$Blue_Median))
      
      category = c(rep("Red",length(dat$Red_Median)) , rep("Green",length(dat$Green_Median)) , rep("Blue",length(dat$Blue_Median)))
      
      med_data = data.frame(Category = category , med = all_qd )
      
      p[[i]] <-  ggplot(med_data, aes(x = Category, y = med)) +
        geom_violin(alpha = 0.3, lwd = 1.2, trim=FALSE, aes(color = Category, fill = Category), show.legend = F) +
        geom_boxplot(width=0.35, alpha = 0.5) +
        scale_color_manual(values = c("blue","green","red"))+
        scale_fill_manual(values = c("blue","green","red"))+
        labs(x= (input$name_multi)[i], y = "Values of Medians")+
        theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 15),
              axis.text.x = element_text(size = 15),axis.title.x = element_text(size = 20))+
        ylim(-0.1,1.2)
    }
    if(length(input$name_multi) == 1){
      p[[1]]
    }else if(length(input$name_multi) == 2){
      p[[1]] + p[[2]]
    }else if(length(input$name_multi) == 3){
      p[[1]] + p[[2]] + p[[3]]
    }else{
      p[[1]] + p[[2]] + p[[3]] + p[[4]]
    }
    
  })
  
  
  output$low4 <- renderText({
    paste("Average Color Proportions of Different Colors")
  })
  
  output$gencolplot = renderPlot({
    
    p = list()
    
    for(i in 1:length(input$name_multi_2)){
      dat <- filter(final_data_grp3, final_data_grp3$Painter == input$name_multi_2[i])
      avg.color.prop <- apply(as.matrix(dat[,11:26]),2,mean)
      color_names <- colnames(dat)[11:26]
      temp.df <- data.frame(color_names,avg.color.prop)
      
      props <- array(0)
      hex_code <- array(0)
      for(j in 1:length(input$colors1_2)){
        props[j] <- temp.df[which(temp.df$color_names == (input$colors1_2)[j]),2]
        hex_code[j] <- col.hex[which(col.names == (input$colors1_2)[j])]
      }
      
      df.temp <- data.frame(input$colors1_2,props,hex_code)
      colnames(df.temp) <- c("multicol","proportions","hex")
      df.temp = df.temp[order(df.temp$multicol),]
      
      
      
      p[[i]] <-  ggplot(df.temp, aes(x = multicol, y = proportions, size = (2*proportions)))+
        geom_point(aes(color = multicol))+
        scale_color_manual(values = df.temp$hex)+
        labs(x= (input$name_multi_2)[i], y = "Color Proportions", color = "Colors", subtitle  = "Size of each point corresponds to twice of the color proportions")+
        theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 15),
              axis.text.x = element_text(size = 15), legend.title = element_text(size = 15),
              legend.text = element_text(size = 13), plot.subtitle = element_text(size = 16),
              axis.title.x = element_text(size = 20))+
        ylim(0,1)
    }
    if(length(input$name_multi_2) == 1){
      p[[1]]
    }else if(length(input$name_multi_2) == 2){
      p[[1]] + p[[2]]
    }else if(length(input$name_multi_2) == 3){
      p[[1]] + p[[2]] + p[[3]]
    }else{
      (p[[1]] + p[[2]] ) / ( p[[3]] + p[[4]] )
    }
    
  })
  
  output$low5 <- renderText({
    paste("Color Proportions of Different Colors over the Years")
  })
  
  output$yearplot = renderPlot({
    p = list()
    for(i in 1:length(input$name_multi_3)){
      dat <- filter(final_data_grp3, final_data_grp3$Painter == input$name_multi_3[i])
      
      
      df1 = dat[,c(3, 11:26)]
      df_mean = aggregate(df1[,-1], by = list(df1[,1]), FUN = mean)
      
      df2 = data.frame(df_mean[,1],stack(df_mean[,-1]))
      df3 = df2[(df2$ind %in% input$colors1_3),]
      colnames(df3) <- c("years","values","ind")
      temp.col <- unique(df3$ind)
      
      hex.code = array(0)
      for(j in 1:length(input$colors1_3)){
        hex.code[j] <- col.hex[which(col.names == temp.col[j])]
      }
      
      year_seq <- seq(min(df3$years), max(df3$years), 4) 
      p[[i]] <- ggplot(df3, aes(years, values))+
        geom_line(aes(col = ind), size = 1.1)+
        scale_x_continuous(breaks = year_seq)+
        scale_color_manual(values = hex.code)+
        labs(x= (input$name_multi_3)[i], y = "Color Proportions", color = "Colors")+
        theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 13),
              axis.text.x = element_text(size = 15), legend.title = element_text(size = 14),
              legend.text = element_text(size = 13), plot.subtitle = element_text(size = 15),
              axis.title.x = element_text(size = 20))+
        ylim(0,1)
    }
    if(length(input$name_multi_3) == 1){
      p[[1]]
    }
    else if(length(input$name_multi_3) == 2){
      p[[1]]  /  p[[2]]
    }else if(length(input$name_multi_3) == 3){
      p[[1]] / p[[2]] / p[[3]]
    }else{
      p[[1]] / p[[2]] / p[[3]] / p[[4]]
    }
    
  })

      
  output$low6 <- renderText({
    paste("Density Curves of Dark, Medium and Light Color Shades Considering All the Paintings of a Painter")
  })
  
  output$shadeplot = renderPlot({
    
    p = list()
    
    for(i in 1:length(input$name_multi_4)){
      dat <- filter(final_data_grp3, final_data_grp3$Painter == input$name_multi_4[i])
      dat.new <- data.frame(values =c(dat$dark_shade_prop, dat$medium_shade_prop, dat$light_shade_prop),
                            Shades = rep(c("dark","medium","light"), each = 714))
      
      p[[i]] <- ggplot(dat.new)+
        stat_density(aes(x=values, col = Shades),
                     geom="line",position="identity", lwd = 1.1)+
        scale_color_manual(values = c("black","gray","orange"), 
                           labels = c("Dark Shade","Light Shade","Medium Shade"))+
        labs(x= "Value of Proportion", y = "Density", title = input$name_multi_4[i])+
        theme_minimal()+
        theme(axis.title.y = element_text(size = 15), axis.text.y = element_text(size = 13),
              axis.text.x = element_text(size = 15), legend.title = element_text(size = 14),
              legend.text = element_text(size = 13), plot.subtitle = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              title = element_text(size = 15))+
        ylim(0,13)+
        xlim(-0.15,1.1)
      
      
    }
    if(length(input$name_multi_4) == 1){
      p[[1]]
    }else if(length(input$name_multi_4) == 2){
      p[[1]] + p[[2]]
    }else if(length(input$name_multi_4) == 3){
      p[[1]] + p[[2]] + p[[3]]
    }else{
      (p[[1]] + p[[2]]) / (p[[3]] + p[[4]])
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
