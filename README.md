# RGCCA Command-line and Shiny 

##### Version: 1.0

##### Author: Etienne CAMENEN

##### Key-words: 
omics, RGCCA, multi-block

##### EDAM operation: 
analysis, correlation, visualisation

##### Contact: 
arthur.tenenhaus@l2s.centralesupelec.fr

##### Short description
Performs multi-variate analysis (PCA, CCA, PLS, RGCCA) and projects the variables and samples into a bi-dimensional space.

---

## Description
A user-friendly multi-blocks analysis (Regularized Generalized Canonical Correlation Analysis, RGCCA) 
with all default settings predefined [1, 2]. Produce figures to help clinicians to identify biomarkers: 
samples and variables projected on the two first component of the multi-block analysis, list of top biomarkers
and explained variance in the model.
 
## RGCCA 
We consider J data matrices X1 ,..., XJ. Each n × pj data matrix Xj = [ xj1, ..., xjpj ] is called a block and represents a set
of pj variables observed on n individuals. The number and the nature of the variables may differ from one block to another,
but the individuals must be the same across blocks. We assume that all variables are centered. The objective of RGCCA is to find,
for each block, a weighted composite of variables (called block component) yj = Xjaj, j = 1 ,..., J (where a j is a column-vector with pj
elements) summarizing the relevant information between and within the blocks. The block components are obtained such that (i) block
components explain well their own block and/or (ii) block components that are assumed to be connected are highly correlated.
As a component-based method, RGCCA can provide users with graphical representations to visualize the sources
of variability within blocks and the amount of correlation between blocks.

## Input files 
(see data/ folder for a working example at [https://github.com/BrainAndSpineInstitute/rgcca_Rpackage/tree/master/data])
- ```blocks``` (.tsv, .csv, .txt or .xls, xlsx) : file(s) containing variables to analyse together.
The samples should be in lines and labelled and variables in columns with a header. With an Excel format, each block 
must be in a separated sheet. For other format, each blocks must be in a separated file.
- ```connection``` (.tsv, .csv, .txt or .xls, xlsx) : file without header, containing a symmetric matrix
of non-negative elements describing the network of connections between blocks that the user wants to take into account.
Its dimension should be NB_BLOCKS + 1) * (NB_BLOCKS + 1). + 1 corresponds for the use of a supplementary block 
(the "superblock"), a concatenation of all the blocks helpful to interpret the results. By default, the connection
matrix is build with values for the last line (and column) except for the diagonal (i.e., the superblock is fully
connected with the other blocks) and 0 values for the other cells (the blocks are not connected together). 
To go further than this null hypothesis, a priori information could be used to tune the matrix (e.g., add 1 value 
for a connection between two block).
- ```response``` (.tsv, .csv, .txt or .xls, xlsx) : an only column of of either a qualitative, or a quantitative variable 
or multiple columns containing a disjunctive table.

## Output files 
- ```corcircle``` (.pdf, .png, .tiff, .bmp or .jpeg) : samples projected in a space composed by the first two components of the analysis (with the percent of explained variance). By selecting a response, samples are colored according to this criterion.
![variables_space](https://raw.githubusercontent.com/BrainAndSpineInstitute/rgcca_Rpackage/master/img/variables_space.png)
- ```samples_space``` (.pdf, .png, .tiff, .bmp or .jpeg) : circle of correlation of variables with the first two components of the analysis (with the percent of  explained variance). The dotted circle corresponds to a 0.5 correlation and the full one corresponds to a 1 correlation.
![samples_space](https://raw.githubusercontent.com/BrainAndSpineInstitute/rgcca_Rpackage/master/img/samples_space.png)
- ```fingerprint``` (.pdf, .png, .tiff, .bmp or .jpeg) : 100 best biomarkers for a set of blocks according to the weight of these variables in the analysis (eigen value for PCA, canonical variable for CCA, component for PLS and RGCCA).
![best_biomarkers](https://raw.githubusercontent.com/BrainAndSpineInstitute/rgcca_Rpackage/master/img/best_biomarkers.png)
- ```ave``` (.pdf, .png, .tiff, .bmp or .jpeg) : average variance explained (in %) in the model for each block ranked decreasingly.
![ave](inst/shiny/img/ave.png)


## Installation
- Softwares : R, [Rstudio](https://www.rstudio.com/products/rstudio/download/#download)
- R libraries : see the [DESCRIPTION](https://github.com/BrainAndSpineInstitute/rgcca_Rpackage/blob/develop/DESCRIPTION)
 file

### Linux

```
sudo apt-get install -y --no-install-recommends git && \
    apt-get install -y r-base r-cran-ggplot2 r-cran-scales r-cran-optparse r-cran-shiny r-cran-shiny-js r-cran-plotly r-cran-igraph && \
    R -e 'install.packages(c("RGCCA", "parallel", "ggrepel", "visNetwork"))' && \
    git clone https://github.com/BrainAndSpineInstitute/rgcca_Rpackage && \
	cd rgcca_Rpackage
```

On Ubuntu, if dependencies errors appear for igraph and plotly, try :
```
sudo apt-get install -y libcurl4-openssl-dev libssl-dev liblapack-dev && \
    apt-get update
```

### Windows
Please, find the software on [Github](https://github.com/BrainAndSpineInstitute/rgcca_Rpackage/tree/develop). Click on the green button in the upper right corner ```Clone and Download``` and then ```Download the ZIP```. Extract the file.


## Execution
If the Linux dependencies installation step was not executed previously (for Windows users), their automatic 
installation could take several minutes during the first execution. If dependencies compatibility errors appear, the required (and suggested) librairies to import are listed in the [DESCRIPTION](https://github.com/BrainAndSpineInstitute/rgcca_Rpackage/blob/develop/DESCRIPTION) file.


### Vignette
- Required : pander R-package
- Suggested : LateX

On linux :
```	
R -e 'install.packages("pandoc")' && \
    sudo apt-get install -y texlive-latex-base texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra texlive-science
```

Please, find the Rmarkdown working example at ```vignettes/vignette_rgcca.Rmd```.

### Shiny interface
Open ```inst/shiny/app.R``` with RStudio to execute Shiny. Execute the code without clicking the ```Run App button```.  Click [here](https://github.com/BrainAndSpineInstitute/rgcca_Rpackage/blob/develop/inst/shiny/tutorialShiny.md) to read
 the tutorial.

### Command line

For direct usage (Example from Russet data from RGCCA package [3]) :

```
Rscript R/launcher.R -d data/agriculture.tsv,data/industry.tsv,data/politic.tsv
```

With parameters :

```
Rscript R/launcher.R --datasets <list_block_files> [--help] [--names <list_block_names] [--connection <connection_file>] [--response <response_file>] [--scheme <scheme_type>] [--output1 <variables_space_fig_name>] [--output3 <samples_space_fig_name>] [--output3 <biomarkers_fig_name>] [--header] [--separator <separator_type>]
```

#### Files parameters
By default, on tabulated files with a header without response groups. The names of the blocks are the filename 
(or the name of the sheets for an Excel file) without the extension.

- ```-d (--datasets)``` (STRING) The list of the paths for each block file separated by comma (without space between).
 Ex : data/X_agric.tsv,data/X_ind.tsv,data/X_polit.tsv or data/blocks.xlsx
- ```-c (--connection)``` (STRING) The path of the file used as a connection matrix. 
- ```-r (--response)``` (STRING) To color samples by group in ```samples_space```, a response file could be added.
- ```--names``` (STRING) The list of the names for each block file separated by comma (without space between).
- ```-H (--header)```DO NOT consider the first row as header of the columns.
- ```--separator``` (INTEGER) Specify the character used to separate the column (1: tabulation, 2: semicolon).
- ```--output1``` (STRING) The path of the output file for the samples space. Ex : sample_space.pdf
- ```--output2``` (STRING) The path of the output file for the corcircle space. Ex : corcircle.pdf
- ```--output3``` (STRING) The path of the output file for the biomarkers. Ex : fingerprint.pdf
- ```--output4``` (STRING) The path of the output file for the variance explained in the model. Ex : ave.pdf

#### Analyse parameters
By default, the analysis : scales the blocks, initiates the algorithm with Singular Value Decomposition, 
uses a superblock with a factorial scheme function, a biased estimator of the variance, a tau equals to one and
two components for each block.

- ```--scale``` DO NOT standardize each block to zero mean and unit variances and then divide them by the square root of its number of variables.
- ```--bias``` Use an unbiased estimator of the variance.
- ```--superblock``` DO NOT use a superblock, a concatenation of all the blocks to better interpret the results.
- ```--ncomp``` (INTEGER) The number of components to use in the analysis for each block (should be greater than 1 and 
lower than the minimum number of variable among the blocks). Could also be a list separated by comma. Ex: 2,2,3,2.
- ```--tau``` (FLOAT) Tau parameter in RGCCA. A tau near 0 maximize the covariance between blocks whereas a tau near 1 maximize
 the correlation between the blocks. Could also be a list separated by comma. Ex: 0,1,0.75,1.
- ```-g (--scheme)``` (INTEGER) Scheme function among 1: Horst, 2: Factorial, 3: Centroid, 4: x^4 (by default, factorial scheme).
The identity (horst scheme) maximizes the sum of covariances between block components. The absolute value (centroid scheme)
maximizes of the sum of the absolute values of the covariances. The square function (factorial scheme) maximizes the sum
of squared covariances, or, more generally, for any even integer m, g(x)=x^m (m-scheme), maximizes the power of m of the
sum of covariances.
- ```--init``` (INTEGER) The mode of initialization of the algorithm (1: Singular Value Decompostion , 2: random).
 
#### Graphical parameters
By default, the x-axis and y-axis are respectively the first and the second components, the number of top biomarkers is 100 and the superblock is used in graphics.

- ```--compx``` (INTEGER) The component used in the X-axis in biplots and the one used in histograms (should not be greater than the ```--ncomp``` parameter). 
- ```--compy``` (INTEGER) The component used in the Y-axis in biplots (should not be greater than the ```--ncomp``` parameter).
- ```--nmark``` (INTEGER) The maximum number of top potential biomarkers (for ```fingerprint``` file).
- ```--block``` (INTEGER) The block shown in the graphics (0: the superblock or, if not, the last, 1: the first one, 2: the 2nd, etc.).

## References
1. Tenenhaus M, Tenenhaus A, Groenen PJF, (2017) Regularized generalized canonical correlation analysis: A framework for sequential multiblock component methods, Psychometrika, vol. 82, no. 3, 737–777
2. Tenenhaus  A. and Guillemot V. (2017): RGCCA Package. http://cran.project.org/web/packages/RGCCA/index.html
3. Tenenhaus A, Tenenhaus M (2011) Regularized generalized canonical correlation analysis, vol. 76, pp. 257-284, Psychometrika.