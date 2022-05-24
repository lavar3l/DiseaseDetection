# DiseaseDetection expert system
# Script for parsing data provided by an expert

# ----------------------------- Global parameters ------------------------------
gInputDirectory <- './Data'
gOutputDirectory <- './Output'

# ------------------------------ Import libraries ------------------------------ 
library(RoughSets)
library(data.table)
library(dplyr)

# --------------------------------- Functions ----------------------------------

ReadDecisionTableFromCsv <- function(fileName) {
  # Read data from CSV file separated by ';' (Excel format)
  dataFrame <- read.csv(file = fileName, sep = ';')
  
  # Convert data frame to decision frame
  decisionTable <- SF.asDecisionTable(dataset = dataFrame, 
                                      decision.attr = ncol(dataFrame), 
                                      indx.nominal = c(ncol(dataFrame)))
  
  return(decisionTable)
}

GenerateDiscernibilityMatrix <- function(decisionTable) {
  # Generate discernibility matrix based on decision table
  controlList <- list(type.relation = c('crisp'), 
                      type.aggregation = c('crisp'),
                      t.implicator = 'lukasiewicz', 
                      type.LU = 'implicator.tnorm')
  
  discernibilityMatrix <- BC.discernibility.mat.RST(decisionTable)
  
  return(discernibilityMatrix)
}

GenerateReductObject <- function(decisionTable.discretized) {
  # Generate reducts based on discretized discernibility matrix
  reducts <- FS.quickreduct.RST(decisionTable.discretized)
  
  return(reducts)
}

GetSingleReduct <- function(reductsObject) {
  # Get single reduct from reducts object and convert it to list of its
  # member symptoms
  reductElements <- as.list(reductsObject$reduct)
  reduct <- as.list(names(reductElements))
  
  return(reduct)
}

ExportReductToCsv <- function(fileName, reduct) {
  # Export reduct (list of symptoms) to CSV file (Excel format)
  dt <- data.frame(reduct)
  write.table(dt, 
              file = fileName, 
              append = FALSE, 
              col.names = FALSE, 
              row.names = FALSE, 
              quote = FALSE, 
              sep = ';')
}

ParseExpertDataSingleFile <- function(fileName) {
  # Parse data provided by expert, generate reducts and export them to CSV file
  
  # Create decision table based on input from CSV file
  decisionTable <- ReadDecisionTableFromCsv(paste(gInputDirectory, fileName, 
                                                                    sep = '/'))
  
  # Generate discernibility matrix
  discernibilityMatrix <- GenerateDiscernibilityMatrix(decisionTable)
  
  # Discretize decision table
  cutValues <- D.discretization.RST(decisionTable, 
                                    type.method = 'unsupervised.quantiles')
  decisionTable.discretized <- SF.applyDecTable(decisionTable, cutValues)
  
  # Generate reducts
  reducts <- GenerateReductObject(decisionTable.discretized)
  
  # Generate rules for predicting
  rules <- RI.indiscernibilityBasedRules.RST(decisionTable.discretized, reducts)

  # Prepare prediction function
  predict.disease <- function(symptomTable, rules, cutValues) {
    symptomTable.discretized <- SF.applyDecTable(symptomTable, cutValues)
    return(predict(rules, symptomTable.discretized))
  }
  
  # Prepare reduct for export
  reduct <- GetSingleReduct(reducts)
  
  # Export reduct to CSV as a list of symptoms (for debug purposes only)
  # ExportReductToCsv(paste(gOutputDirectory, fileName, sep = '/'), reduct)
  
  # Return generated data
  return(list(
    decisionTable = decisionTable,
    rules = rules,
    cutValues = cutValues,
    predict = predict.disease
  ))
}

GenerateSymptomTable <- function(symptoms, decisionTable) {
  symptomTable <- decisionTable[1,]
  symptomTable[] <- 0
  allowed <- colnames(symptomTable)
  for(symptom in symptoms)
    if(symptom %in% allowed) symptomTable[symptom] <- 1
  return(symptomTable)
}

GetAvailableSymptoms <- function() {
  fileList <- list.files(path = gInputDirectory,
                         pattern = '*.csv',
                         full.names = FALSE)

  file <- read.csv(file = paste(gInputDirectory, fileList[1], sep = '/'), 
                                                                      sep = ';')

  symptoms <- names(file)
  symptoms <- symptoms[-length(symptoms)]
  return(symptoms)
}

ParseCoefficientFile <- function(fileName) {
  directory <- paste(gInputDirectory, 'Coefficients', sep = '/')
  return(read.csv(paste(directory, fileName, sep = '/'), sep = ';'))
}

ParseCoefficients <- function() {
  return(ParseCoefficientFile('data.csv'))
}

ParseRisks <- function() {
  return(ParseCoefficientFile('risk.csv'))
}

ParseRiskFactors <- function() {
  return(ParseCoefficientFile('riskFactors.csv'))
}

GetRiskFactorNames <- function () {
  names <- names(riskFactors)
  return(names[names != 'Risk.factor'])
}

ParseExpertDataAllFiles <- function() {
  # Parse data provided by expert for all files from provided input directory,
  # generate reducts and export them to CSV files in provided output directory.
  
  # Get all CSV files from input directory
  fileList <- list.files(path = gInputDirectory, 
                         pattern = '*.csv', 
                         full.names = FALSE)

  # Result list
  result <- list()

  # Parse all files
  for(fileName in fileList) {
    predictionData <- ParseExpertDataSingleFile(fileName)
    diseaseName <- str_split(fileName, '\\.')[[1]][1]
    result[[diseaseName]] <- predictionData
  }

  return(result)
}

# ------------------------------- Test script ----------------------------------
# symptoms1 <- c('asthenia', 'non.productive.cough', 'pain.chest', 'rale', 'shortness.of.breath')
# symptoms2 <- c('constipation', 'fever', 'nausea', 'pain.back', 'throat.sore', 'vomiting')
# ParseExpertDataAllFiles(symptoms1)
# ParseExpertDataAllFiles(symptoms2)
