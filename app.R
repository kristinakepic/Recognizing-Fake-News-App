library(shiny)
library(shinydashboard)
library(shinythemes)
library(qdap)
library(randomForest)
library(caret)
library(stats)


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Real or Fake Article Detector"),

    # Sidebar with a the result
    sidebarLayout(
        sidebarPanel(
          textOutput("pred")
        ),

        # main panel to input title and text into
        mainPanel(
          textInput("artTitle", "Article Title", value = "", width = 450, placeholder = NULL),
          textAreaInput(
            "artText",
            "Article Text",
            value = "",
            width = 450,
            height = 450,
            cols = NULL,
            rows = NULL,
            placeholder = NULL,
            resize = NULL
          ),
          actionButton("Submit", "Submit")
        )
    )
)

# Define server logic
server <- function(input, output) {
  
  output$pred <- eventReactive(input$Submit, {
    str <- toString(input$artTitle)
    split <- strsplit(str," ")
    headCount <- sapply(split , length)
    str <- toString(input$artText)
    split <- strsplit(str," ")
    textCount <- sapply(split , length)
    if(grepl("\\.", str))
    {
      Ascii <- iconv(str, "latin1","ASCII","")
      temp <- data.frame(text=Ascii)
      temp2 <- sentSplit(temp, 'text')
      readabilitydf <- automated_readability_index(temp2$text,)
      formalitydf <- formality(temp2$text,)
      readAnalysis <- readabilitydf$Readability
      formAnalysis <- formalitydf$formality
      r <- readAnalysis[1,5]
      s <- readAnalysis[1,3]
      c <- readAnalysis[1,4]
      f <- formAnalysis$formality
    }
    else
    {
      r<- 0
      f<- 0
      s<- 0
      c<- nchar(str)
    }
    df = data.frame(
      headCount = numeric(),
      col7 = numeric(),
      Readability = numeric(),
      Formality = numeric(),
      SentCount = numeric(),
      charCount = numeric()
    )
    df[1, ] <- list(textCount,headCount, r, f, s, c)
    set.seed(7)
    NewsML <- readRDS(file= "NewsML.rds")
    pred <- predict(NewsML, newdata=df, test = "class")
    print(pred)
    zero <- pred[1,1]
    if(zero <= .5)
    {
      result <- "This news article is most likely real"
    }
    else
    {
       result <- "This news article is most likely fake"
    }
    result
    
    })

  
        
    }


# Run the application 
shinyApp(ui = ui, server = server)
