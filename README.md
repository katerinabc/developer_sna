# Microdynamics in Software developer teams

## Progress
Tech interface is done. 
communication realized is done.
communication required is done.
ERGM model: in progress
TERGM model: not started

## Data

3 datasets are used in this study.

### Dataset 1: micro-tasks
The dataset consists of 2-mode network data linkig developer to software files. The edge is the type of work a developer has done on the file (a = added, m = modified). This dataset is also called *microtask*. This data is used to calculate the *realized communication.*

### Dataset 2: Task dependencies
1-mode network file showing the connections between software files (tehcnical dependencies between software files). This dataset is used to measure *required communication*.

### Dataset 3: Developer attributes
Attribute information about developers (e.g., job role, work-location, type of contract).

*Data files are not included in the github folder.*

*The version number has been changed from what is in the file. Some versions are consolidated. This is done in the file `data_import.R`. 

## Variables

### Communication realized
The number of times two developers "communicated" with each other. This is operationalized as the number of times developers made changes to two connected files in the same time period. If file A is depending on file B, and file B is changed in period 1, then file A also needs to be adapted. 

The file is created in the script *comm_realized.R*. Detailed instructions are included in that script. In brief, the steps are:
1. Using the modified 2-mode network (owner - software file; `df_modified.csv`), we created a 1-mode network showing how frequent two developers communicated with each other. This was created as an edgelist to keep time information. The package `networkDynamic` was used for that

### Communication required
The number of times two developers need to "communicate" with each other, given the dependence between two files. 

In the file *task dependencies*, each file has an ID. To replace the IDs with the owner's name, the software file IDs are matched with the software file ID in the dataset micro-task (`df_modified.csv`). Using NetworkDynamic this edgelist is then transformed into a network object (edgelist with time information)

### Ownership of files
To decide who is responsible to keep a file up-to-date, a file is assigned to a developer (see script `ownership_developer_sna.R`). 
- A developer owns a file if s/he has made the first changes to the file. 
- If a developer is not present during a version, then ownership is transferred to the first developer who makes a change to the file in the respective version.
- Developers can not regain ownership when they rejoin the software development.

There remains an error with some folders having no owner. This is an issue for version 4. The frequency of wrong assignments of folder owner (assigning a folder owner who is not a member of the project) is low for the other version 

 |   ver |  n  | freq |
 | ---- | ----| ----|
  |3   |  1   |0.77|
  |4   | 102  |78.5 |
  |5   |   12 | 9.23|
  |6   |   15 |11.5 |


The result of the script `onwership_developer_sna.R` is `df_modified.csv`. This file is used for calculating communication realized. 

### Familiarity
The number of times two developers are part of the same version. 

## Valued ERGM

*My main concern is that the models converge only most of the time.*

## Model set up
A geometric reference distirbution is taken as the valued data represents count data.
- Null model: only modeling sum (valued ERGM equivalent of density)
- Model 1: Null model + independept network. It measures the impact of required communication on realized communication. Edge frequency is taken into account. The sum of edge weights is used.
- Model 2: Model 1 + influence of familiarity on realized communication. Edge frequency is taken into account. The sum of edge weights is used.
- Model 3: Model 2 + influence of job title taken into account. Job title included as "the higher the job title, the greater chance that developers communicate with that person (nodefactor).  *does not converge. Error message: NA/NaN/Inf in foreign function call (arg 1)*
- Model 4: Model 3 + Job location added as homophily term. (not run, see model 3)


|model       | variable               | log odds | odds    | significance |
| ----        |         ---------     | :---:    | :---:    | ----|           
|null model  | sum                    | -0.135    | 87%   | *** |
|model 1     | sum                    | -0.053   |       | *** |
|            | nonzero                |  -3.612   |       | *** |
|            |comm required           | 0.004     |  0.4% | * |
|model 2     | sum                    | -0.06     |       |*** |
|            | nonzero                | -3.59     |       |***|
|            | comm required          |0.003      |  0.3% |not significant |
|            | Familiarity            |0.002      |0.2%   |* |




|model | AIC | BIC |
| ----|:---:|:--:|
|null model | -1835 | -1831|
|model  1    | -2581 -2571|
|model 2| -2577 | -2563 |







