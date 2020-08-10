#install.packages("shinyFiles")
library(shiny)
library(shinyFiles)
library(jpeg)
ui <- fluidPage(
  
  includeCSS("bootstrap.min.css"),
  tags$h1('Image Compression'),
  tags$p('This app takes only JPG image.You can set variance for compressing image using slider.'),
  sidebarLayout(
    sidebarPanel(
      fileInput('selfie', "Choose File",accept=c('image/png', 'image/jpeg','image/jpg')),
      tags$hr(),
      sliderInput("PCAdim",label="Select value",value=25,min=1,max=100),
      actionButton("exec",label="Compress"),
      tags$hr(),
      downloadButton("dl", label = "Download")
      
                
                
           
      
              
               ),
    
    mainPanel(
      imageOutput('img'),
      imageOutput('compressimg')
           )
  
  
) 
)



server <- function(input, output) {
#take image
  re1 <- reactive({gsub("\\\\", "/", input$selfie$datapath)})
  #observe({ 
   # filejpg<-input$file1$name
    
    #print(filejpg) })
  
  inFile <- reactive({
    #gsub("\\\\", "/", input$selfie$datapath)
    
    inFile <- input$selfie
  })
  PCAdim <- reactive({
    PCAdim <- input$PCAdim
  })
  
  img.array <- eventReactive(input$exec, {
    ReduceDimsjpg(inFile()$datapath[1], PCAdim())
  })
  
  ReduceDimsjpg <- function(jpg.file, pc.num) {
    
    rgb.array <- jpeg::readJPEG(jpg.file)
    rgb.list <- SplitImage(rgb.array)
    
    rgb.list <- lapply(rgb.list, prcomp, center=FALSE)
    rgb.proj <- lapply(rgb.list, ProjectImage, pc.num)
    
    restored.img <- abind::abind(rgb.proj, along=3)
  }
  
  SplitImage <- function(rgb.array) {
    
    rgb.list <- list()
    for (i in 1:dim(rgb.array)[3]) {
      rgb.list[[i]] <- rgb.array[, , i]
    }
    return(rgb.list)
  }
  
  ProjectImage <- function(prcomp.obj, pc.num) {
    # project image onto n principal components
    
    img.proj <- prcomp.obj$x[, c(1:pc.num)] %*% t(prcomp.obj$rotation[, c(1:pc.num)])
    img.proj <- ifelse(img.proj>1, 1, img.proj)
    img.proj <- ifelse(img.proj<0, 0, img.proj)
    
    
    
    
    
    
    return(img.proj)
  }
  
  output$img <- renderImage({
    req(input$selfie)
    list(src = re1())},deleteFile = FALSE)
  
  
  
  
  output$compressimg <- renderImage({
    outfile <- tempfile(fileext='.jpg')
    #gsub("\\\\", "/",outfile)
    #print(outfile)
    jpeg::writeJPEG(img.array(), target = outfile)
    list(src = outfile, contentType = 'image/jpg')}, deleteFile = TRUE
  )
  
  
  
 
#download compress file
  
  output$dl <- downloadHandler(
     filename = function() {
     paste('image-', Sys.Date(), '.jpg', sep='')
   },
   content = function(file) {
     jpeg::writeJPEG(img.array(), target = file)
   }
 )
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)