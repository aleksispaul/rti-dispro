# rti-dispro project
This is a repo for a collaborative analysis of the impact that RTI to SLD legislation has had on disproportionate placement of SOC in special education. With the exception of Wisconsin data requested from the Wisconsin Department of Public Instruction (DPI), data were pulled from the U.S. Department of Education section 618 reporting on the Dept of Ed. website in August, 2020.

## folder file structure
Folders are organized by "input" and "output" of datasets. Original datasets are placed in the input section for historical reference and, with the exception of the Wisconsin 618 data (which was provided as a PDF, and processed using Tabula and excel prior to importing into R), were not edited outside of the syntax provided in the R syntax files.

Output datasets are those that were cleaned/adjusted in R and exported to the output folder. These are the datasets used for analysis in each successive iteration of syntax.

## syntax files
Syntax files are chronological with code that builds from one to the next. The beginning of each syntax file loads the most recent output datasets for use in analysis in that syntax file. Multiple syntax files were created to increase readability of each process. That is, a person does not need to re-run all of the data cleaning in the first syntax file to arrive at a cleaned and merged dataset of 618 data from 2005 to 2018. Rather, they can start at syntax file 02_* and access the exported results from the first syntax file. 
