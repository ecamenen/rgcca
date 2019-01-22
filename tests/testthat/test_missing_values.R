#getData = function() load("~/Documents/Stage Emmanuel/BLOCS.RData")
set.seed(31)

getData = function(){
  m1 = createMatrix(35, 28)
  m2 = createMatrix(48, 18)
  m3 = createMatrix(47, 38)
  return (list(m1, m2, m3))
}

getDim = function(list_m)
  lapply(list_m, dim )

getRownames = function(list_m)
  lapply(list_m, row.names)

createMatrix = function(nrow, ncol){
  #output: matrix with  random values and random row.names in integer format

  d = as.data.frame(matrix(runif(nrow * ncol), nrow, ncol))
  #give a random row number as row names (in a subset 1.5 times more bigger than NROW)
  row.names(d) = sample(NROW(d)*1.5, NROW(d))
  return (d)
}

getDiffRow = function(list_m, i){
  # Get the row that they are missing (according to their names) in a df in comparison with a list of othe matrix
  # list_m: list of dataframe
  # i: df to compare with the other
  # Output : vector of character with the missing rownames

  missing_row = c()

  for ( j in 1:length(list_m) )
    # add the result of missing rows with each pairwise comparison with another matrix
    missing_row = c(missing_row, setdiff(row.names(list_m[[j]]), row.names(list_m[[i]])) )

  return (unique(missing_row))
}

addNARow = function (df, r){
  # Add rows with NA values to a dataframe
  # df: dataframe
  # r: vector of rowname to add
  # Output : df with new rows filled by NA

  if (length(r) != 0){
    # create NA vectors
    added_row = matrix(NA, length(r),NCOL(df))
    # set names for binding to the df
    row.names(added_row) = r
    colnames(added_row) = colnames(df)
    # bind by rows
    df = rbind(df, added_row)
  }else
    return (df)
}

filledRowInDiff = function (list_m){
  # Add NA rows to each missing row (based on their rownames) according to a pairwise comparaison among a list of df
  # list_m : list of dataframe
  # Output: a list of datrame with the same number of row (with the same name), missing rows contain NA

  for ( i in 1:length(list_m) ){
    row_diff = getDiffRow(list_m, i)
    list_m[[i]] = addNARow(list_m[[i]], row_diff )
  }

  return (list_m)
}


### TESTS ###

test_that( "test_commonRow", {
  BLOCS = getData()

  test = unique( intersect(row.names(BLOCS[[1]]), row.names(BLOCS[[2]])) == commonRow(BLOCS[1:2]) )
  expect_true (  length(test) == 1 && test )
})


test_that( "test_keepCommonRow" ,{
  BLOCS = getData()

  dim_list_before = sapply(1:length(BLOCS), function (x) dim(BLOCS[[x]])[1] )
  # tests if any block if their row number is equals to the maximum number of rows
  expect_true ( any ( sapply(1:length(BLOCS), function (x) dim(BLOCS[[x]])[1] == max(dim_list_before)) == F) )

  BLOCS = keepCommonRow(BLOCS)

  # tests if all blocks have the same row.names (and at the same time, the same row number)
  expect_true ( all ( sapply(1:length(BLOCS), function (x) row.names(BLOCS[[x]]) == row.names(BLOCS[[1]])) == T) )
})


test_that( "test_diffRow", {
  BLOCS = getData()

  expect_true ( all.equal( getDiffRow(BLOCS[c(1,2)], 1), setdiff(row.names(BLOCS[[2]]), row.names(BLOCS[[1]])) ) )
})

test_that( "test_addNARow", {
  BLOCS = getData()

  # get row in difference
  row_diff = getDiffRow(BLOCS, 1)
  # add them to a df
  actual = addNARow(BLOCS[[1]], row_diff )
  # test if all rows get by getDiffRow have been added to the df with NA
  expect_true (  all ( is.na ( actual[row_diff,] ) ) )
})

test_that( "test_filledRowInDiff", {
  BLOCS = getData()

  dim_list_before = sapply(1:length(BLOCS), function (x) dim(BLOCS[[x]])[1] )
  # tests if any block if their row number is equals to the maximum number of rows
  expect_true (  any ( sapply(1:length(BLOCS), function (x) dim(BLOCS[[x]])[1] == max(dim_list_before)) == F) )

  BLOCS = filledRowInDiff(BLOCS)

  dim_list_after = sapply(1:length(BLOCS), function (x) dim(BLOCS[[x]])[1] )
  expect_true ( all ( sapply(1:length(BLOCS), function (x) dim(BLOCS[[x]])[1] == max(dim_list_after)) == T) )
})
