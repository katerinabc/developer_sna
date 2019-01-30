# Microdynamics in Software developer teams

## Data

The dataset consists of a 2 mode network linkig developer to software files. The edge is the work a developer has done on the file (a = added, m = modified). Additional data is available about developers (e.g., job role, work-location, type of contract) and about the software files (tecnical dependencies between software files). 

## Dependent network

The dependent network is the **realized communication** between developers (`cr_net` in `version4_mirrioring.R`). It measures how often two developers were communicating with each otehr. Communication here refers to making changes in the same file. 
Realized collaboration is $\Task_{d,f} X Task_{d,f}^T$. $Task_{d,f}$ refers to the matrix of developrs and file ownership. 

## Independent Network
The independent network is the **needed communication** between developers (`cn_net` in `version4_mirrioring.R`).It measures how often two developers should have been communicating. Communication in this context refers to *making changes in a file*. Communication between two developers is required if the developers have ownership on files which are technically dependent on each other. Two files are technically dependent on each other, if file 1 makes a reference to objects/functions in file 2. When something is changed in file 1, it is necessary to check file 2 to ensure no bug is created. *Ownership* is defined in the following way:
1. The developer who created first a folder owns the files that are in the folder. 
2. If a developer is not anymore member of the project, s/he looses ownership of the files of the previously owned folder. 
3. Ownership of the folder is transferred to the first developer who makes a change to files in the respective folder.  
Needed collaboration is $\Ownership_{d,f} X Dependencies_{f,f} X Ownership_{d,f}^T$. $\Ownership_{d,f}$ is the matrix describing who owns what folder. $\Dependencies_{f,f}$ is the square matrix listening the technical dependencies between folders. The task dependencies matrix currently does not take into account how strongly two files are dependent on each other. 

Important for calculating `cn_net` was that the files in the ownership matrix are also the files in the task dependencies matrix. Files that appeared in one matrix, but not in the other one, needed to be deleted. Additionally, the names of the files needed to be in the same sequence in both matrices. Line 303 - 443 describe the process:
1. Create a bipartite graph with developers and files they own based on folder ownership. 
2. Create a graph with file dependencies. 
3. For each graph, extract the vertex names. For the first graph, the biparite graph, names are developers and file ids. For the second graph the name is only file ids. 
4. Match the two vertex names. The names in the bipartite graph serve as a reference. This means that all file ids not in the Onwernship matrix will be excluded from the task dependencies. 
5. Create a subgroup from the 2nd graph (file depencnies), keeping only the files which are also in the ownership file. 
6. Transform the graphs into matrices and do the multiplication necessary to create `cr_net` (line 442).

After the two networks are created, I checked if the same people are in the networks (line 474 - 486). Those that were missing were added to the task dependencies network. I only added the nodes with no edges to anybody.

## Familiarity between developers
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


### Creating the Networks
The dependent network is the number of times two developers worked on the same file. This is called *g_mt* in the file *version4_mirroring*. Ties are predicted using the ownership network (*g_own*) and the technical interdependencies network (*g_req*).

My first attempt was to build these 3 networks using the data file. However, this created a problem as not all developers were in each networks. The solution was to build an empty network (*g*), add the vertex attributes, and then to copy this, and add the edges. The following lines of code show how it is done using the *g_mt* network as an example:

First we are creating the empty network. The developer number 22 is an artifical developer. Some files are required for a version, but they are not modified. Therefore it wasn't possible to assign a developer to the file. To account for the technical dependencies between a file that has been modified and one that hasn't, I added this artificial developer. 
```{r}
#Initialize a network object
g<-network.initialize(22)

# add vertex attributes
set.vertex.attribute(g, 'vertex.names', c(unique(as.character(authatt$ID_author)), '22'))
set.vertex.attribute(g, 'developers', c(unique(as.character(authatt$author)), 'not.modified'))
participants <- NULL
jobtitle <- NULL
location <- NULL
contract <- NULL
authors_v4 <- authatt[authatt$ver == 4,]
for (i in get.vertex.attribute(g, 'developers')){
  tmp_ver <- authatt[authatt$author == i, 1]
  if(4 %in% tmp_ver){present <- 1}else{present<-0}
  participants <- cbind(participants, present)
  
  if(4 %in% tmp_ver){tmp_job <- authors_v4[authors_v4$author == i, 4]}else{tmp_job<- 99}
  jobtitle <- cbind(jobtitle, tmp_job)
  
  if(4 %in% tmp_ver){tmp_loc <- authors_v4[authors_v4$author == i, 5]}else{tmp_loc<- 99}
  location <- cbind(location, tmp_loc)
  
  if(4 %in% tmp_ver){tmp_con <- authors_v4[authors_v4$author == i, 6]}else{tmp_con<- 99}
  contract <- cbind(contract, tmp_con)
  
}
set.vertex.attribute(g, 'ver4', t(participants)[,1])
set.vertex.attribute(g, 'jobtitle', t(jobtitle)[,1])
set.vertex.attribute(g, 'location', t(location)[,1])
set.vertex.attribute(g, 'contract', t(contract)[,1])
```
This creates an empty network, adds the attributes for the developers for that version. This code can easily be modified for other versions. The *4* in the loop needs to be changed. 

Using this empty network we can add the edge value. 
```{r}
g_mt <- g

# add edges
head(microtask_file)

# replace developer name with vertex id
for (i in 1:length(microtask_file$author)){ # could be coded easier
  tmp_dev <- microtask_file$author[i]
  tmp_id <- authors_v4[authors_v4$author == tmp_dev, 3]
  microtask_file$author[i] <- tmp_id
}

mt_file <- as.matrix(bipart_to_row_projection(microtask_file$author, microtask_file$Filename)[[2]])
# modify matrix into an edgelist. maybe via first creating a network
mtfi_net <- as.network(mt_file, directed=F, matrix.type='a', ignore.eval=F, names.eval='frequency')
# nodes = developer
# edges = number of times developers worked on same file
mtfi_el <- as.edgelist(mtfi_net, attrname = 'frequency')

add.edges(g_mt, mtfi_el[,1], mtfi_el[,2], names.eval = 'freq_collab', vals.eval = mtfi_el[,3])
g_mt
gplot(g_mt)
```

## Technical information
`data_import.R` imports and cleans the datasets. In that file the following objects are created:
* DF: data set with *microtasks*. This is the export Mahdi got from the software company. It describes every event during software development, meaning all the files that were created and modified. 
* DF2: data set with file dependencies. It shows how often a file is dependent on another file. 
* authatt: data set with information about developers for each version. 

`version4_mirroring.R` contains the script necessary to create the needed and realized collaboration networks. It also contains the valued ergms. Due to changes in operationalization of realized and needed collaboration, the script can be confusing. All information is left in for auditing purposes. Line 62 - 301 describe the old method. 