library(RGCCA)
data("Russett")
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
