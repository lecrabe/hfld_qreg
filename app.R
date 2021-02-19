############# Packages
options(stringsAsFactors=FALSE)
options(shiny.launch.browser=T)

library(quantreg)
library(shiny)
library(ggplot2)

source("packages.R",echo = TRUE)

############# Define UI 
ui <- fluidPage(
  uiOutput("select_file"),
  uiOutput("select_values"),
  plotOutput('plot'),
  textOutput('val')
)

############# Define server logic ----
server <- function(input, output,session) {
  
  ############# Create local volumes information
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  media <- list.files("/media", full.names = T)
  names(media) = basename(media)
  volumes <- c(media)
  aoi_vol <- setNames(paste0(normalizePath("~"),"/hfld_qreg/"),"hfld")
  
  volumes <- c(aoi_vol,
               'Home' = Sys.getenv("HOME"),
               volumes
  )
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  
  print(volumes)
  
  ############# 
  output$select_file <- renderUI({
    #req(input$aoi_type == "custom")
    
    shinyFilesButton(id = 'aoi_file',
                     label = "Data file", #TO TRY TO IMPLEMENT
                     title = "Browse", #htmlOutput('select_a_file'),
                     multiple = FALSE)
  })
  
  ##################################################################################################################################
  ############### Select input file 
  shinyFileChoose(
    input,
    'aoi_file',
    filetype = c('csv'),
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  ############# 
  file_path <- reactive({
    validate(need(input$aoi_file, "Missing input: select the AOI file"))
    df <- parseFilePaths(volumes, input$aoi_file)
    file_path <- as.character(df[, "datapath"])
    nofile <- as.character("No file selected")
    if (is.null(file_path)) {
      cat(nofile)
    } else{
      cat(file_path)
    }
    file_path
  })
  
  ############# 
  df <- reactive({
    req(file_path())
    print(paste0( "test",file_path()))
    df <- read.csv(as.character(file_path()))
    df
  })
  
  
  ############# 
  output$val <- renderText({
    req(file_path())
    print(file_path())
  })
  
  ############# 
  output$select_values <- renderUI({
    validate(need(input$aoi_file, "Missing input: select the AOI file"))
    req(df())
    df <- df()
    sliderInput('values',
                'Select start and end',
                min = min(df$year),
                max=max(df$year),
                step = 1,
                value=c(min(df$year),max(df$year)),
                sep=""
    )
  })
  
  yr_str <- reactive({as.numeric(input$values[1])})
  yr_end <- reactive({as.numeric(input$values[2])})
  
  ############# 
  df1 <- reactive({
    req(df())
    df <- df()
    df[df$year >= yr_str() & df$year <= yr_end(),]
  })
  
  ############# 
  rqfit <- reactive({
    req(df1())
    rq(total ~ year, data = df1())
  })
  

  
  ############# 
  output$plot <- renderPlot({
    req(rqfit())
    rqfit <- rqfit()
    df1   <- df1()
    
    ggplot(df1, aes(x=year, y=total)) + 
      geom_bar(position="stack", stat="identity")+
      geom_abline(intercept = rqfit[[1]][1],slope = rqfit[[1]][2], color="red")
  })
  
}

############# Run the app ----
shinyApp(ui = ui, server = server)