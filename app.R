# DiseaseDetection expert system
# Web application for users to detect disease based on provided symptoms

library(shiny)
library(stringr)

source('ImportData.R')

predictionData <- ParseExpertDataAllFiles()
availableSymptoms <- GetAvailableSymptoms()
coefficients <- ParseCoefficients()

# Parse input to list of occuring symptoms
GetOccurringSymptoms <- function (input, symptopms) {
  return (
    Filter(function (symptom) {
      return(input[[symptom]] == 'yes')
    }, symptopms)
  )
}

# Generate select input for provided symptom id
YesNoSelect <- function (inputId) {
  readableLabel <- str_to_title(str_replace_all(inputId, '\\.', ' '))
  default <- ifelse(inputId %in% c('asthenia', 'non.productive.cough', 'pain.chest', 'rale', 'shortness.of.breath'), 'yes', 'no')
  return(
    selectInput(inputId, h5(readableLabel),
                choices = list("Yes" = 'yes',
                               "No" = 'no'), selected = default))
}

# Generate inputs for all symptoms
GenerateInputs <- function (symptopms) {
  colsNum <- 4
  rowsNum <- ceiling(length(symptopms) / colsNum)

  return(lapply(1:rowsNum, function (rowNum) {
    fluidRow(
      lapply(1:colsNum, function (colNum) {
        index <- (rowNum - 1) * colsNum + colNum
        if (index > length(symptopms)) return(NULL)
        return(column(12 / colsNum, YesNoSelect(symptopms[index])))
      })
    )
  }))
}

GetDiseaseIndex <- function(disease) {
  diseaseRow <- coefficients$Disease;
  return(match(disease, diseaseRow))
}

GetSymptoDiseaseWeight <- function(symptom, disease) {
  diseaseIndex <- GetDiseaseIndex(disease)
  weight <- coefficients[[symptom]][diseaseIndex]
  if (is.na(weight)) return(0)
  return(as.numeric(sub(",", ".", weight)))
}


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Disease detection - Expert System"),

  fluidRow(
    # Select symptoms
    column(6,
      h3("Select your symptoms"),
         GenerateInputs(availableSymptoms),
         br(),
         actionButton("submit", "Submit")
    ),
    column(6,
      h3("Plot with predictions"),
           textOutput("result")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  observeEvent(input$submit, {
    occurringSymptoms <- GetOccurringSymptoms(input, availableSymptoms)
    diseases <- NULL

    # Get predicted diseases
    for (disease in names(predictionData)) {
      pData <- predictionData[[disease]]
      symptomTable <- GenerateSymptomTable(occurringSymptoms, pData$decisionTable)
      res <- pData$predict(symptomTable, pData$rules, pData$cutValues)
      if (res == 1) {
        wI <- 0
        wJ <- 0

        for (symptom in availableSymptoms) {
          weight <- GetSymptoDiseaseWeight(symptom, disease)
          if (weight > 0) {
            if (symptom %in% occurringSymptoms) wI <- wI + weight
            else wJ <- wJ + weight
          }
        }

        p2 <- (wI / (wI + wJ)) * 100

        if (!is.nan(p2) && p2 > 10) {
          info <- paste(disease, res, ' ', p2, '%')
          diseases <- append(diseases, info)
          print(info)
        }
      }
    }

    output$result <- renderText({ paste(diseases, sep = ', ') })

  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)