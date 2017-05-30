## Cascade Incidence Project ##

This project uses a simple HIV transmission model to estimate the contribution of the population at each step of the HIV diagnosis and care cascade to new infections. The project involves collaboration between the [_The Kirby Institute_](https://kirby.unsw.edu.au/) and the [_Centre for Social Research in Health (CSRH)_](https://csrh.arts.unsw.edu.au/), UNSW Sydney, Australia. 
Funding for the project was provided as part of [_the COUNT study_](https://csrh.arts.unsw.edu.au/research/projects/the-count-study/). While the analyses has primarily focused on transmission from gay and bisexual men (GBM) nationally, the code has been designed to be as flexible as possible to facilitate future analyses. 

All calculations are conducted using R (currently version 3.3.2) with associated packages (main packages: dplyr 0.5.0; tidyr 0.6.1; ggplot2 2.2.1; cowplot 0.7.0; gridExtra 2.2.1).

**Repository owner:** Richard T. Gray

**ORCID ID:** orcid.org/0000-0002-2885-0483

**Affiliation:** [_The Kirby Institute_](https://kirby.unsw.edu.au/), UNSW Sydney, Sydney NSW 2052, Australia

### Contributors ###

The following list of people were the main contributors to the development of the analysis methodology. 

- Dr Richard T. Gray: Principle investigator for the project at the Kirby Institute. Model developer and coder and maintainer of this repository [1].
- Assoc. Prof. Martin Holt: [2]
- Prof. David P. Wilson: [3]

**Affiliations**:

- [1] [_The Kirby Institute_](https://kirby.unsw.edu.au/), UNSW Sydney, Sydney NSW 2052, Australia
- [2] [_Centre for Social Research in Health (CSRH)_](https://csrh.arts.unsw.edu.au/), UNSW Sydney, Australia
- [3] [_Burnet Institute_](https://www.burnet.edu.au/), Melbourne VIC 3004, Australia 

### Aims ###

The main aim of this project was to determine the contribution of undiagnosed HIV to new infections among gay and bisexual men (GBM) over a ten period in Australia. To do this we produced annual estimates for each step of the HIV care and treatment cascade and the number of new HIV infections for GBM in Australia over 2004-2015 using relevant national data. Using Bayesian melding we then fitted a quantitative model to the cascade and incidence estimates in order to infer relative infectivity parameters associated with being undiagnosed, diagnosed and not on ART, or on ART (virally suppressed or not). 

### Project organization ###

All the project files are stored in the main directory and 5 main sub-directories. This main README file describes the overall project. Additional README files are provided in specific directories to describe further aspects as necessary. 

The project code is written in R version 3.3.2 as R or R markdown scripts with Rstudio version 1.0.44. The primary packages to run this code are dplyr 0.5.0, tidyr 0.6.1, ggplot2 2.2.1, and markdown 0.7.7. All model inputs and outputs stored as `.csv` or `.rda` files. 

_Main directory scripts_

The following R markdown scripts are used to generate estimates of the HIV cascade, run the analysis, and produce outputs. The numbering is to indicate the order the scripts need to be run. Each of the scripts require user inputs to specify project details and continues a detailed description. The files are numbered from 0 to specify the order they need to be run to produce all the results. Scripts starting with the same number can be run independently of each other. 

- **0-GbmCascade.Rmd**: This script is used to estimate the number of people in each step of the HIV cascade for GBM in Australia. The user has to specify the start and end years for the analysis and prepare inputs files with annual estimates for the number of GBM living with diagnosed HIV, the percentage of undiagnosed GBM living with HIV, and the proportion of GBM diagnosed with HIV on ART and with suppressed virus (stored in the data directory described below). All user inputs are set in the _script details_ R markdown chunk. Outputs results and figures are stored in the `output` and `output/Cascade_figures` directories, respectively. 

- **1-CascadeIncidence.Rmd**: This script is used to explore various approaches of estimating the proportion of new HIV infections in the overall population due to due to each stage of the HIV diagnosis and care cascade. This script is now **deprecated** due to methodological problems with the approach. 

- **2-CascadeIncidenceBayes.Rmd**: This script is the primary analysis script for the project. Based on user specifications it loads the cascade estimates produced by 0-GbmCascade and new infections estimates (in the data directory described below) and runs the Bayesian melding methodology. User inputs are set in the _loaddata_ and _bayessims_ R markdown chunks. Outputs from the script are stored in the `output` directory. 

- **3-CascadeIncidenceBayesCompare.Rmd**: This script is used to compare the results from different runs of `2-CascadeIncidenceBayes.Rmd` which have different prior or simulation specifications. The script produces a number of figures stored in the `output/Compare_figures` directory. 

- **3-CascadeIncidenceResults.Rmd**: This script generates all the results and figures for a user specified output from `2-CascadeIncidenceBayes.Rmd`. The specific output used is set at the top of the _Load results_ R markdown chunk. Other user inputs are specified in the _Script options_ R markdown chunk. All the results and figures are saved in the inputted results directory in the `output` directory. 

Note as these are scripts, not functions, care should be taken to ensure the project specifications are correct before running a script especially after updates have been pulled from the repository.

#### code ####

Contains all the specific R functions used in the analysis and by the main directory scripts. The main set of functions is stored in `BayesFunctions.R`. Details of each function are provided as comments within the function file. 

#### data ####

Contains all the input data files used by the `1-CascadeIncidence.Rmd` script for calculating the HIV cascade for GBM. These files are stored as `.csv` files in separate directories for the years 2014 and 2015. Associated output files from the ECDC HIV Modelling Tool used in the analysis are also stored in the repository. 

#### docs (local) ####

Contains manuscript, report, and presentation files based on cascade results and related documents. These files are stored locally and are not available in the online repository.

#### misc (local) ####

Contains miscellaneous files such references, correspondence, general information, examples, working documents, and general kibble  of relevance to the project. These files are stored locally and are not available in the online repository. 

#### outputs ####

Contains output files storing the results produced by the main directory scripts. The results are stored in multiple directories as `.csv` and `.rda` files. The GBM HIV cascade estimates produced by `0-GbmCascade.Rmd` are stored in the `output/` directory as `.csv` files with associated figures saved in `output/Cascade_figures`. Each run of `2-CascadeIncidenceBayes.Rmd` produces a separate directory of results given a name based on the time when the calculations were initiated or a summary name (which replaces the initial time name). Within the each results directory the associated simulation results are stored as `.rda` files. The associated summary results and figures produced by `3-CascadeIncidenceResults.Rmd` are also stored in this directory as `.csv` files and within a `figures` directory. All the `.csv` files associated with the final cascade results and the cascade incidence analyses used in the project paper are stored in the repository. A README file within the directory describes the output files and directories files in detail. 

### Publications ###

The following publication is associated with this project and used the code in this repository to generate all of the results and figures. 

- Richard T. Gray, David P. Wilson, Rebecca Guy, Mark Stoove, Margaret Hellard, Garrett Prestage, Toby Lea, John de Wit, Martin Holt. Undiagnosed HIV infections among gay and bisexual men increasingly contribute to new infections in Australia. _In preparation_
 

