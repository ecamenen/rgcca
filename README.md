# multiOmics Toolbox

#### Version: 1.0

#### Author: Etienne CAMENEN

#### Key-words: 
omics, RGCCA, multi-block

#### EDAM operation: 
analysis, correlation, visualisation

#### Contact: 
iconics@icm-institute.org

#### Institute: 
- ICM - Institut du Cerveau et de la Moelle épinière (Paris, FRANCE)
- Institut Français de Bioinformatique (IFB)
- Centre national de la recherche scientifique (CNRS)


#### Short description
Performs multi-variate analysis (PCA, CCA, PLS, RGCCA) and projects the variables and samples into a bi-dimensional space.

## Description
A user-friendly multi-blocks analysis (Regularized Generalized Canonical Correlation Analysis, RGCCA) with all default settings predefined. Produce two figures to help clinicians to identify biomarkers: samples and variables projected on the two first component of the multi-block analysis.

### Input files (see data/ folder for examples)
- ```blocks``` (.tsv or .txt) : tabulated files containing variables to analyse together. The samples should be in lines and labelled and variables in columns with an header.
- ```connection``` (.tsv or .txt) : tabulated files without header, containing a symmetric matrix with either 0 or 1 for a connection between each blocks.
- ```response``` (.tsv or .txt) : an only column of a qualitative variable.

### Output files 
- ```variables_space``` (.pdf or .png) : samples projected in a space composed by the first two component of the analysis (with the percent of explained variance). By selecting a response, samples are colored according to this criterion.
![variables_space](/img/variables_space.png)
- ```samples_space``` (.pdf or .png) : circle of correlation of variables with the first two component of the analysis (with the percent of explained variance). The dotted circle corresponds to a 0.5 correlation and the full one corresponds to a 1 correlation. 
![samples_space](/img/samples_space.png)
- ```best_biomarkers``` (.pdf or .png) : 10 best biomarkers for a set of blocks according to the weight of these variables in the analysis (eigen value for PCA, canonical variable for CCA, component for PLS and RGCCA).
![best_biomarkers](/img/best_biomarkers.png)

## Usage Instructions
For direct usage :

```
Rscript galaxy_rgcca.R -d <datasets>
```

With optional parameters :

```
Rscript galaxy_rgcca.R --datasets <datasets> [--help] [--connection <connection_file>] [--response <response_file>] [--scheme <scheme_type>] [--output1 <variables_space_fig_name>] [--output3 <samples_space_fig_name>] [--output3 <biomarkers_fig_name>]
```

- ```-d (--datasets)``` (STRING) The list of the path for each block file separated by comma (without space between). Ex: data/X_agric.tsv,data/X_ind.tsv,data/X_polit.tsv
- ```-c (--connection)``` (STRING) The path of the file used as a connection matrix. Its dimension should be (NB_BLOCKS + 1) * (NB_BLOCKS + 1). + 1 corresponds for the use of a supplementary block (the "superblock"), a concatenation of all the blocks helpful to interpret the results. By default, the connection matrix is build with 1 values for the last line (and column) except for the diagonal (i.e., the superblock is fully connected with the other blocks) and 0 values for the other cells (the blocks are not connected together). To go further than this null hypothesis, a priori information could be used to tune the matrix (e.g., add 1 value for a connection between two block).  
- ```-r (--response)``` (STRING) For a supervised mode, a response file could be added (by default, no response variable).
- ```-g (--scheme)``` (INTEGER) Scheme function among 1: Horst, 2: Factorial, 3: Centroid, 4: x^4 (by default, factorial scheme).
- ```-s (--separator)``` (INTEGER) Specify the character used to separate the column in the fingerprint dataset (1: tabulation, 2: semicolon) (by default, tabulation).
- ```-o1 (--output1)``` (STRING) The path of the output file for the samples space.
- ```-o2 (--output2)``` (STRING) The path of the output file for the variables space.
- ```-o3 (--output3)``` (STRING) The path of the output file for the biomarkers.

## References
- Tenenhaus M, Tenenhaus A, Groenen PJF, (2017) Regularized generalized canonical correlation analysis: A framework for sequential multiblock component methods, Psychometrika, vol. 82, no. 3, 737–777
- Tenenhaus  A. and Guillemot V. (2017): RGCCA Package. http://cran.project.org/web/packages/RGCCA/index.html
- Tenenhaus A, Tenenhaus M (2011) Regularized generalized canonical correlation analysis, vol. 76, pp. 257-284, Psychometrika.