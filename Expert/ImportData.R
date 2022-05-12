# DiseaseDetection system expert
# Script for parsing data provided by an expert

# ----------------------------- Global parameters ------------------------------
gInputDirectory <- "./Data"
gOutputDirectory <- "./Output"

# ------------------------------ Import libraries ------------------------------ 
library(RoughSets)
library(data.table)

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
  controlList <- list(type.relation = c("crisp"), 
                      type.aggregation = c("crisp"),
                      t.implicator = "lukasiewicz", 
                      type.LU = "implicator.tnorm")
  
  discernibilityMatrix <- BC.discernibility.mat.RST(decisionTable)
  
  return(discernibilityMatrix)
}

GenerateAllReducts <- function(discernibilityMatrix) {
  # Generate reducts based on discernibility matrix
  reducts <- FS.all.reducts.computation(discernibilityMatrix)
  
  return(reducts)
}

GetSingleReduct <- function(reducts) {
  # Get single reduct from reducts object and convert it to list of its
  # member symptoms
  reductObject <- tail(reducts$decision.reduct, n = 1)
  reductElements <- as.list(reductObject$reduct$reduct)
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
              sep = ";")
}

ParseExpertDataSingleFile <- function(fileName) {
  # Parse data provided by expert, generate reducts and export them to CSV file
  
  # Create decision table based on input from CSV file
  decisionTable <- ReadDecisionTableFromCsv(paste(gInputDirectory, fileName, 
                                                                    sep = "/"))
  
  # Generate discernibility matrix
  discernibilityMatrix <- GenerateDiscernibilityMatrix(decisionTable)
  
  # Generate reducts
  reducts <- GenerateAllReducts(discernibilityMatrix)
  
  # Prepare reduct for export
  reduct <- GetSingleReduct(reducts)
  
  # Export reduct to CSV as a list of symptoms
  ExportReductToCsv(paste(gOutputDirectory, fileName, sep = "/"), reduct)
}

ParseExpertDataAllFiles <- function() {
  # Parse data provided by expert for all files from provided input directory,
  # generate reducts and export them to CSV files in provided output directory.
  
  # Get all CSV files from input directory
  fileList <- list.files(path = gInputDirectory, 
                         pattern = "*.csv", 
                         full.names = FALSE)
  
  # Parse all files
  for(fileName in fileList) {
    print(paste("Parsing:", fileName, sep = " "))
    ParseExpertDataSingleFile(fileName)
  }
}

# ---------------------------------- Script ------------------------------------
ParseExpertDataAllFiles()