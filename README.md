# Microdynamics in Software developer teams

## Progress
Tech interface is done. 
communication realized is done.
communication required is not done
Mirroring is not done. 


25/11/19: Communication realized: work-in-progress. I need to get my head around the network dynamic package. 
16/01/20: Comm realizes is done. The networks for branch 1 and 2 are calculated. similarity between them is 0.7
14/10/20: Mahdi needs 3 csv files: layers of developers (communication realized), layers of dependencies (task depenndeices), ownership matrix (connecting developers to tasks)

## Data

The dataset consists of a 2 mode network linkig developer to software files. The edge is the work a developer has done on the file (a = added, m = modified). Additional data is available about developers (e.g., job role, work-location, type of contract) and about the software files (tecnical dependencies between software files). 

## Communication realized
Communication realized describes the interaction between developers. Two developers are interacting with each other if they make changes to the same file in a given version. The file is created in the script *comm_realized.R*. Detailed instructions are included in that script. In brief, the steps are:
1. Using the microtask dataset (the file that contains every action of a developer with information about when this was done, what file etc), create a matrix *Developer X Filename*. This creates a 2-mode network. This network contains no time information and no weights. 
2. Using the 2-mode network created in step 1, transpose it to a *Developer X Developer* matrix using 2-mode project. 


## Ownership assignment
file: ownership_developer_sna.R
THe owner of folders are added to the data set. Ownership is defined as (1) the first person who created the folder or (2) the first person who did a modification to a file in a folder if the original folder owner is not anymore part of the project team. Ownership can not be regained when re-entering the project. 
There remains an error with some folders having no owner. This is an issue for version 4. The frequency of wrong assignments of folder owner (assigning a folder owner who is not a member of the project) is low for the other version 

    ver |  n  | freq
  ______   _____ _____
  3   |  1   |0.77
  4   | 102  |78.5 
  5   |   12 | 9.23
  6   |   15 |11.5 


## OLD NOTES

### Dependent network

The dependent network is the **realized communication** between developers. A realization is when two developers work on the same file (e.g. one could creates it another one could modified it) while developing a given version. This is regardless of who is owner of what. 

### Independent Network
The independent network is the **required (needed) communication** between developers. It was first named `cn_net` and computed in `version4_mirrioring.R`. After discussion, it has been re-computed in the file `tech_interface.R`. It is called `td_net`.

The required communication measures how often two developers should have been communicating. Communication in this context refers to *making changes in a file*. Communication between two developers is required if the developers have ownership of files which are technically dependent on each other. Two files are technically dependent on each other, if file 1 makes a reference to objects/functions in file 2. When something is changed in file 1, it is necessary to check file 2 to ensure no bug is created. *Ownership* is defined in the following way:
1. The developer who created first a folder owns the files that are in the folder. 
2. If a developer is not anymore member of the project, s/he looses ownership of the files of the previously owned folder. 
3. Ownership of the folder is transferred to the developer who makes most contribution to the folder in the version. The new owner remains until s/he leaves the project. Ownership can not be regained by re-joining the team. 

The required communication network is created by replacing the file IDs in the file-by-file edgelist by the owners. Ownership information has been computed and added to the developer-file table. This table contains information about each change in the software. We have added one column to the data to indicate who owns the folder where the file is located which has been modified by a developer. 

The network is created in step 3 in the file `tech_interface.R`. The network object is *big* because it also contains information about the version and the strength of technical dependence. The advantage is that a network can be easily created for each version, however, large objects clog down the memory. 








### Familiarity between developers
Familiarity between developers is the number of times two developers worked on a previous version of the software together. 


### Valued ERGM
For valued ERGMs it is necessary to provide a reference distribution. The space from which to draw sample networks. For example if the values in the networks can only be 0 or 1 then the sample distirbution is a uniform or truncated geometric distribution or a binominal distribution. In case there is no upper bound on the values of ties a *geometric* or *Poission* distribution should be used. Andy Pilny argues that the Poisson distribution is useful when average tie value isn't much different to the variance. If this is not the case geometric distribution should be used. This makes geometric distribution very useful for very skewed distribution of edge values, where most nodes are 0.

A histogram of frequency shows that most edges were 0. 
![Degree Distribution for Software Dvelopment.](fig/histogram_frequency_developer_projections.png). But when running various ergms models with a geometric distribution I kept on running into problems with the eigenvalues (*Error in eigen(crossprod(x1c), symmetric=TRUE): infinite or missing values in x*). There aren't any missing values.

Based on advice by Pavel Kirsti adding a control term 'control = control.ergm(MCMC.prop.weights='0inflated')'together with the Poisson reference distribution could work. 

The following problem was a singular approximate Hessian matrix. Often the reason is collinearity between variables. If I understand it correctly, the Hessian matrix is a derivative (*second order partial derivative*). It describes local points (curves) of a function which has many variables. It is singular if at least (?) two variables in the Hessian matrix (or its approximation) are the same and thus have a perfect (positive) correlation

The first solution was to check collinearity between variables. The command 'kcycle' calculates path or cycle census information. The output in our case 'kcycle(developer_net, mode="graph", maxlen = 4)' provides in the first column the aggregate of path counts and in the other columns information abhout path counts for each developer's. Concretely, it counts how often someone has two paths, three cycle, 4 cycle
*Example*
   Agg chiara.moretti fabio.boldrin alessandro.basso andrea.rana
2    0              0             0                0           0
3  453             48           103               82           6
4 4041            585          1230              982          42


### Technical information
`data_import.R` imports and cleans the datasets. In that file the following objects are created:
* DF: data set with *microtasks*. This is the export Mahdi got from the software company. It describes every event during software development, meaning all the files that were created and modified. 
* DF2: data set with file dependencies. It shows how often a file is dependent on another file. 
* authatt: data set with information about developers for each version. 

`version4_mirroring.R` contains the script necessary to create the needed and realized collaboration networks. It also contains the valued ergms. Due to changes in operationalization of realized and needed collaboration, the script can be confusing. All information is left in for auditing purposes. Line 62 - 301 describe the old method. 
