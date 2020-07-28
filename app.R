library(shiny)
library(ggplot2)
library(ggthemes)
library(shinyWidgets)



dataset <- tmp
subState2 <-  subset(states2, region %in% tolower("Alabama"))


ui <- fluidPage(
  
  fluidRow(


      column(width = 4,
             selectInput('state', 'STATE', stSub$State),
             # Input: Select the random distribution type ----
             radioButtons("fut", "Present or Future?",
                          c("Present" = "present",
                            "Future" = "future")),
             
        textOutput(outputId = "spInfo"),
        
        sliderTextInput(inputId = "timeP", 
                        label = "Time period:", 
                        choices = c("2040-2069", "2070-2099")),

        sliderTextInput(inputId = "scenario",
                        label = "Scenarios:", 
                        choices = c("rcp45", "rcp85")), 
        
        textOutput(outputId = "deltaA") 
        
        #includeHTML("~/Documents/R/include.html")
        
        
        
  
  ), 
  
  column(width = 8, 
         plotOutput("rasterPlot"))
  
  ),

  fluidRow(
    
    column(width = 8, 
           plotOutput("plot2")), 

    column(width = 4,
           
           includeHTML("~/Documents/R/include.html"))
    
  )
)



server <- function(input, output){
  
  output$rasterPlot <- renderPlot({
    
    sv <- which(stSub$State == input$state)
    print(sv)
    load(file = paste0(stPath, "/Datafiles/", input$state, ".Rdata"))
    dataset <- tmp
    head(dataset)
    
     MINS <- c(min(dataset[[1]]$Abundance), min(dataset[[2]]$Abundance), 
               min(dataset[[3]]$Abundance), min(dataset[[4]]$Abundance), 
               min(dataset[[5]]$Abundance))
     
     MAXS <- c(max(dataset[[1]]$Abundance), max(dataset[[2]]$Abundance), 
               max(dataset[[3]]$Abundance), max(dataset[[4]]$Abundance), 
               max(dataset[[5]]$Abundance))
     
     scale <- c(MINS[which(MINS == min(MINS))[1]], MAXS[which(MAXS == max(MAXS))[1]])
     
     mapx <- c(min(subState2$long), max(subState2$long))
     mapy <- c(min(subState2$lat), max(subState2$lat))
    

    scen <- sapply(names(dataset), function(x){
      cut <- gregexpr("_", x)[[1]][1]
      substr(x, 1, cut - 1)})
    names(scen) <- NULL
    scen <- unique(scen)
    
    timeP <- sapply(names(dataset), function(x){
      cut <- gregexpr("_",x)[[1]][1]
      substr(x,  cut + 1,nchar(x))})
    names(timeP) <- NULL
    timeP <- unique(timeP)
    
    
    #num <- which(scen == input$scenario & timeP == input$timeP )
    test <- paste0(input$scenario, "_", input$timeP)
    num <- which(names(dataset) == test)

    if(input$fut == "present"){
     
      ggplot() +
        geom_raster(data = dataset[[1]], aes(x = x, y = y, fill =Abundance))  + 
        scale_fill_viridis_c(limits = scale) +
        coord_sf()+
        theme_bw() + xlab("Longitude") + ylab("Latitude")  +
        theme(panel.background = element_rect(fill = "white")
              , plot.title = element_text(size = 10.5, vjust =-1, face = "italic"),
              plot.subtitle = element_text(size = 8, face = "italic")) 
      
    }else{
       
       
       ggplot() +
         geom_raster(data = dataset[[num]], aes(x = x, y = y, fill =Abundance))  + 
         scale_fill_viridis_c(limits = scale) +
         coord_sf()+
         theme_bw() + xlab("Longitude") + ylab("Latitude")  +
         theme(panel.background = element_rect(fill = "white")
               , plot.title = element_text(size = 10.5, vjust =-1, face = "italic"),
               plot.subtitle = element_text(size = 8, face = "italic")) 
       
     }
    
     
    
 
  })
  
  output$spInfo <- renderText({
    sv <- which(stSub$State == input$state)
    species <- stSub$Scientific_name[sv]
    common <- stSub$Common_name[sv]
    
    paste0("SPECIES DISPLAYED: ", species, " (also known as ", common,")")})
  
  
  output$deltaA <- renderText({
    
    sv <- which(stSub$State == input$state)
    load(file = paste0(stPath, "/Datafiles/", input$state, ".Rdata"))
    dataset <- tmp
    scen <- sapply(names(dataset), function(x){
      cut <- gregexpr("_", x)[[1]][1]
      substr(x, 1, cut - 1)})
    names(scen) <- NULL
    scen <- unique(scen)
    
    timeP <- sapply(names(dataset), function(x){
      cut <- gregexpr("_",x)[[1]][1]
      substr(x,  cut + 1,nchar(x))})
    names(timeP) <- NULL
    timeP <- unique(timeP)
    
    
    test <- paste0(input$scenario, "_", input$timeP)
    num <- which(names(dataset) == test)
    
    
    presentA <- sum(dataset[["hist_2018"]]$Abundance, na.rm = T)
    
    futureA <- sum(dataset[[num]]$Abundance, na.rm = T)
    if(input$fut == "present"){
      changeA <- 0
    }else{
    changeA <- round(((futureA - presentA)/presentA)*100, digits = 2)}
    
    paste0("Change in abundance relative to present ", changeA, " %")
    
  })
  
  output$plot1 <- renderPlot({
    
    plot(rnorm(100, 0, 1), rnorm(100, 0, 1))
  })
  
  library(scatterplot3d)
  z <- rnorm(100,0,1)
  x <- cos(z)
  y <- sin(z)
  
  output$plot2 <- renderPlot({
  
    sv <- which(stSub$State == input$state)
    spCode <- stSub$SPSymbol[sv]
    
    xy <- read.csv(paste0(stPath, "/xydata_out1.csv"))
    sp3 <- xy[, c(spCode, "tmin", "PPET")]
  
    
    scatterplot3d(sp3[,1], sp3$tmin, sp3$PPET, 
                  col.axis = "grey", highlight.3d = TRUE, 
                  col.grid = "grey", main = "Species niche", pch = 20,
                  xlab = "Abundance (m2/ha)", ylab = "PPET (mm)", zlab = "Tmin (deg)")
    

    
  })
}

shinyApp(ui = ui, server = server)


