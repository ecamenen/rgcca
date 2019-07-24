# Author: Etienne CAMENEN
# Date: 2019
# Contact: arthur.tenenhaus@l2s.centralesupelec.fr
# Key-words: omics, RGCCA, multi-block
# EDAM operation: analysis, correlation, visualisation
#
# Abstract: A user-friendly multi-blocks analysis (Regularized Generalized Canonical Correlation Analysis, RGCCA)
# with all default settings predefined. Produce four figures to help clinicians to identify fingerprint:
# the samples and the variables projected on the two first component of the multi-block analysis, the histograms
# of the most explicative variables and the explained variance for each blocks.

rm(list=ls())

setGlobVar <- function(){
  assign("one_block",
         c(`Principal Component Analysis` = "PCA"),
         .GlobalEnv)
  assign("two_blocks",
         c(`Canon ical Correlation Analysis` = 'CCA', `Interbattery Factor Analysis` = "IFA", `Partial Least Squares Regression` = 'PLS',  `Redundancy analysis` = 'RA'),
         .GlobalEnv)
  assign("multiple_blocks",
         c(`Regularized Generalized CCA (RGCCA)` = 'RGCCA', `Sparse Generalized CCA (SGCCA)` = 'SGCCA', `SUM of CORrelations method` = 'SUMCOR', `Sum of SQuared CORrelations method` = 'SSQCOR',
                       `Sum of ABSolute value CORrelations method` = 'SABSCOR',`SUM of COVariances method` = 'SUMCOV',`Sum of SQuared COVariances method` = 'SSQCOV',
                       `Sum of ABSolute value COVariances method` = 'SABSCOV', `MAXBET` = 'MAXBET', `MAXBETB` = 'MAXBET-B'),
         .GlobalEnv)
  assign("multiple_blocks_super",
         c(`Generalized CCA (GCCA)` = 'GCCA', `Hierarchical PCA` = 'HPCA', `Multiple Factor Analysis` = 'MFA'),
         .GlobalEnv)
  assign("analyse_methods",
         list(one_block, two_blocks, multiple_blocks, multiple_blocks_super),
         .GlobalEnv)

  # Maximum size allowed : 30 MB
  options(shiny.maxRequestSize = 30*1024^2)
}

setGlobVar()
shiny::runApp(appDir = strsplit(rstudioapi::getActiveDocumentContext()$path, "/app.R")[[1]],
              display.mode = "normal")
