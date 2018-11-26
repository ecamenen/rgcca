library(RGCCA)
library(rgccaLauncher)
data("Russett")

# Load the data
agriculture = Russett[, 1:3]
industry = Russett[, 4:5]
politic = Russett[, 6:11]
response = factor( apply(Russett[, 9:11], 1, which.max),
                   labels = colnames(Russett)[9:11] )
connection = matrix(c(0, 0, 0, 1,
                      0, 0, 0, 1,
                      0, 0, 0, 1,
                      1, 1, 1, 0),
                    4, 4)

files = c("agriculture", "industry", "politic", "response", "connection")

# Creates the files
if(!file.exists("temp"))
  dir.create("temp")

sapply(1:length(files), function (x) {

  bool = ( files[x] != "connection")

  write.table(
    x = get(files[x]),
    file = paste("temp/", files[x], ".tsv", sep=""),
    row.names = bool,
    col.names = bool,
    sep = "\t")
})


test_that("blocks are equals to the origin", {

  blocks = setBlocks (TRUE, "temp/agriculture.tsv,temp/industry.tsv,temp/politic.tsv", "agric,ind,polit")
  for (i in 1:3)
    expect_equal(blocks[[i]], as.matrix( get(files[i])) )
})


test_that("response is equals to the origin", {

  blocks = setBlocks (TRUE, "temp/agriculture.tsv,temp/industry.tsv,temp/politic.tsv", "agric,ind,polit")
  response_obs = setResponse(blocks, "temp/response.tsv")
  expect_equal(as.vector(response_obs), as.vector(response))
  expect_equal(row.names(as.vector(response_obs)), row.names(as.vector(response)))
})

test_that("connection is equals to the origin", {

  blocks = setBlocks (TRUE, "temp/agriculture.tsv,temp/industry.tsv,temp/politic.tsv", "agric,ind,polit")
  connection_obs = setConnection (blocks, "temp/connection.tsv")
  expect_equal( matrix(connection_obs, 4, 4), connection)
})

# Remove the temp/ folder
unlink("temp", recursive = TRUE)
