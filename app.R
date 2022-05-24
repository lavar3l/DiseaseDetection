# DiseaseDetection expert system
# Web application for users to detect disease based on provided symptoms

library(shiny)
library(stringr)
library(plotly)
library(shinyWidgets)

source('ImportData.R')

predictionData <- ParseExpertDataAllFiles()
availableSymptoms <- GetAvailableSymptoms()
coefficients <- ParseCoefficients()
risks <- ParseRisks()
riskFactors <- ParseRiskFactors()
riskFactorNames <- GetRiskFactorNames()

# Parse input to list of occuring symptoms
GetOccurringSymptoms <- function (input, symptopms) {
  return (
    Filter(function (symptom) {
      return(input[[symptom]] == 'yes')
    }, symptopms)
  )
}

ToHumanReadable <- function(string) {
  return(str_to_title(str_replace_all(string, '\\.', ' ')))
}

# Generate select input for provided symptom id
YesNoSelect <- function (inputId) {
  readableLabel <- ToHumanReadable(inputId)
  return(
    selectInput(inputId, h5(readableLabel),
                choices = list("Yes" = 'yes',
                               "No" = 'no'), selected = 'no'))
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

# Generate mutli select risk factors
GenerateRiskFactorsMultiSelect <- function () {
  return(
    multiInput(
      inputId = "riskFactors",
      choiceValues = riskFactorNames,
      choiceNames = lapply(riskFactorNames, FUN = ToHumanReadable),
      label = "",
      width = "100%",
      options = list(
        enable_search = FALSE
      )
    )
  )
}

# Convert from char to numeric
ConvertToNumeric <- function(dec) {
  return(as.numeric(sub(",", ".", dec)))
}

# Get specified sympton and disease weight
GetSymptomDiseaseWeight <- function(symptom, disease) {
  diseaseRow <- coefficients$Disease;
  diseaseIndex <- match(disease, diseaseRow)
  weight <- coefficients[[symptom]][diseaseIndex]
  if (is.na(weight)) return(0)
  return(ConvertToNumeric(weight))
}

# Get min and max disease risk
GetDiseaseRisks <- function(disease) {
  diseaseRow <- risks$X;
  diseaseIndex <- match(disease, diseaseRow)
  return(list(
    min = ConvertToNumeric(risks$min[diseaseIndex]),
    max = ConvertToNumeric(risks$max[diseaseIndex])
  ))
}

# Get disease risk factors average
GetDiseaseRiskFactorsAvg <- function(disease, occuringRiskFactors) {
  if (length(occuringRiskFactors) == 0) {
    return(0)
  }

  diseaseRow <- riskFactors$Risk.factor;
  diseaseIndex <- match(disease, diseaseRow)

  sum <- 0

  for (factor in occuringRiskFactors) {
    sum <- sum + ConvertToNumeric(riskFactors[[factor]][diseaseIndex])
  }

  return(sum / (length(riskFactors) - 1))
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
         h3("Select your risk factors"),
         GenerateRiskFactorsMultiSelect(),
       br(),
       actionButton("submit", "Submit"),
    ),
    column(6,
      h3("Plot with predictions"),
         textOutput("noDiseases"),
         plotlyOutput("chart")
    )
  ),
  br()
)

# Define server logic required to draw a chart of predicted diseases ----
server <- function(input, output) {
    observeEvent(input$submit, {
      # Convert input to vector of occuring symptopms
      occurringSymptoms <- GetOccurringSymptoms(input, availableSymptoms)
      occurringRiskFactors <- input$riskFactors

      # Init vectors
      predictedDiseases <- NULL
      predictedDiseasesP1 <- NULL
      predictedDiseasesP2 <- NULL

      # Get predicted diseases
      for (disease in names(predictionData)) {
        pData <- predictionData[[disease]]
        symptomTable <- GenerateSymptomTable(occurringSymptoms, pData$decisionTable)
        res <- pData$predict(symptomTable, pData$rules, pData$cutValues)
        if (res == 1) {
          wI <- 0
          wJ <- 0

          for (symptom in availableSymptoms) {
            weight <- GetSymptomDiseaseWeight(symptom, disease)
            if (weight > 0) {
              if (symptom %in% occurringSymptoms) wI <- wI + weight
              else wJ <- wJ + weight
            }
          }

          risks <- GetDiseaseRisks(disease)
          p1 <-  max(min(((2 * wI - wJ - risks$min) / (risks$max - risks$min)) * 100, 100), 0)
          p2 <- max(min(((wI / (wI + wJ)) + GetDiseaseRiskFactorsAvg(disease, occuringRiskFactors=occurringRiskFactors)) * 100, 100), 0)

          # Omit disease when fuzzy function result is low
          if (is.na(p1) || is.na(p2) || is.nan(p1) || is.nan(p2) || p1 < 10 || p2 < 10)
            next

          predictedDiseases <- append(predictedDiseases, disease)
          predictedDiseasesP1 <- append(predictedDiseasesP1, p1)
          predictedDiseasesP2 <- append(predictedDiseasesP2, p2)
        }
      }

      if (length(predictedDiseases) > 0) {
        print(predictedDiseases)
        output$noDiseases <- renderText({ '' })

        data <- data.frame(predictedDiseases, predictedDiseasesP1, predictedDiseasesP2)

        # Show chart with predicted diseases
        output$chart <- renderPlotly({
          plot_ly(
            data,
            x = ~predictedDiseases,
            y = ~predictedDiseasesP1,
            name = 'Fuzzy function 1',
            type = 'bar'
          ) %>% add_trace(y = ~predictedDiseasesP2, name = 'Fuzzy function 2') %>%
            layout(xaxis = list(title = 'Predicted diseases'), yaxis = list(title = 'Percent of probability'), barmode = 'group')
        })
      } else {
        output$noDiseases <- renderText({ 'No diseases predicted' })
        output$chart <- renderPlotly({ NULL })
      }
    })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)