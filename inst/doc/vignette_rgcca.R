## ----setup, include = FALSE, echo = FALSE, warning = FALSE, message = FALSE----

# Importing libraries
librairies = c("RGCCA", "ggplot2", "scales", "rgccaLauncher", "knitr", "pander")
for (l in librairies) {
  if (!(l %in% installed.packages()[, "Package"]))
    install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
  library(l, character.only = TRUE)
}


# Load the data
data("Russett")

#Creates the blocks
agriculture = Russett[, 1:3]
industry = Russett[, 4:5]
politic = Russett[, 6:11]

# Creates optional files
response = factor( apply(Russett[, 9:11], 1, which.max),
                   labels = colnames(Russett)[9:11] )
connection = matrix(c(0, 0, 0, 1,
                      0, 0, 0, 1,
                      0, 0, 0, 1,
                      1, 1, 1, 0),
                    4, 4)

# Save the Russett files
files = c("agriculture", "industry", "politic", "response", "connection")

# Creates the files (in .tsv format for example)
if(!file.exists("data"))
  dir.create("data")

# Without row and column names for connection files
sapply(1:length(files), function (x) {

  bool = ( files[x] != "connection")

  write.table(
    x = get(files[x]),
    file = paste("data/", files[x], ".tsv", sep=""),
    row.names = bool,
    col.names = bool,
    sep = "\t")
})

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "images/"
)


## ----blocks--------------------------------------------------------------
# A boolean giving the presence (TRUE) / absence (FALSE) of a superblock

blocks = setBlocks(file = "data/agriculture.tsv,data/industry.tsv,data/politic.tsv")
blocks[["Superblock"]] = Reduce(cbind, blocks)

## ----connection,  echo = TRUE, inlcude = TRUE----------------------------
# Optional parameters
RESPONSE = "data/response.tsv"
CONNECTION = "data/connection.tsv"
SUPERBLOCK =  TRUE
# Uncomment the parameters below to try without default settings
# RESPONSE <- CONNECTION <- NULL

response = setResponse(blocks = blocks, 
                       file = RESPONSE)
connection = setConnection(blocks = blocks, 
                           file = CONNECTION)

## ----views, echo=FALSE, results = "asis"---------------------------------

for (x in 1:length(files)) {
  
  tab = get(files[x])
  if (x > 3) colnames(tab) = rep(NULL, NCOL(tab))
  
  pander::pandoc.table(head(tab),
                       caption = files[x])
}

## ----rgcca---------------------------------------------------------------
# Use two components in RGCCA to plot in a bidimensionnal space
NB_COMP = c(2, 2, 3, 3)

sgcca.res = rgcca.analyze(blocks = blocks,
                  connection = connection,
                  ncomp = NB_COMP)

# Renames the elements of the list according to the block names
names(sgcca.res$a) = names(blocks)

## ------------------------------------------------------------------------
COMP1 = 2
COMP2 = 3
NB_MARK = 100

## ------------------------------------------------------------------------
plotSamplesSpace(rgcca = sgcca.res, 
                 resp = response)

## ------------------------------------------------------------------------
plotVariablesSpace(rgcca = sgcca.res,
                   block = blocks,
                   superblock = SUPERBLOCK)

## ------------------------------------------------------------------------
plotFingerprint(rgcca = sgcca.res,
                superblock = SUPERBLOCK,
                n_mark = NB_MARK,
                type = "weigth")

## ------------------------------------------------------------------------
plotAVE(rgcca = sgcca.res)


## ------------------------------------------------------------------------
plotSamplesSpace(rgcca = sgcca.res, 
                 resp = response, 
                 comp_x = COMP1, 
                 comp_y = COMP2, 
                 i_block = 3)

## ------------------------------------------------------------------------
plotVariablesSpace(rgcca = sgcca.res,
                   block = blocks,
                   comp_x = COMP1,
                   comp_y = COMP2,
                   i_block = 3)

## ------------------------------------------------------------------------
plotFingerprint(rgcca = sgcca.res,
                comp = COMP1,
                n_mark = NB_MARK, 
                i_block = 3,
                type = "weigth")

## ---- echo = FALSE-------------------------------------------------------
# Remove the temp/ folder
unlink("data", recursive = TRUE)

