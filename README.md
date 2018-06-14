<-- load the following source file with data to be included in this file -->
# Microdynamics in Software developer teams

**work-in-progress Paper**
## Data

The dataset consists of a 2 mode network linkig developer to software files. The edge is the work a developer has done on the file (a = added, m = modified). Additional data is available about developers (e.g., job role, work-location, type of contract) and about the software files (tecnical dependencies between software files)



## Task 1: Replication of Sosa (2015) paper

A full replication of the methodology by Sosa is only possible if the data is dichotomized. The  developer-to-developer network is a projection of the 2-mode matrix: Developer - Software file. The devleoper-to-developer matrix indicates which two developer worked on a file. It is an undirected  *collaboration* network. 

To run an ERGM the dependent network needs to contain binary data. 

The collaboration matrix for each software version is pretty small (see Figure 1), therefore running a valued ERGM on the complete network taking all collaboration into account is better. Figure 2 visualizes the complete network. 

![Collaboration Network for Software v1.](~/Documents/gitrepo/developer_sna/fig/ugly_dev_1_network.png)

![Collaboration Network for Software development.](~/Documents/gitrepo/developer_sna?fig)


### Valued ERGM
For valued ERGMs it is necessary to provide a reference distribution. The space from which to draw sample networks. For example if the values in the networks can only be 0 or 1 then the sample distirbution is a uniform or truncated geometric distribution or a binominal distribution. In case there is no upper bound on the values of ties a *geometric* or *Poission* distribution should be used. Andy Pilny argues that the Poisson distribution is useful when average tie value isn't much different to the variance. If this is not the case geometric distribution should be used. This makes geometric distribution very useful for very skewed distribution of edge values, where most nodes are 0.

A histogram of frequency shows that most edges were 0. 
![Degree Distribution for Software Dvelopment.](~/Documents/gitrepo/developer_sna/fig/histogram_frequency_develoepr_projections.png). But when running various ergms models with a geometric distribution I kept on running into problems with the eigenvalues (*Error in eigen(crossprod(x1c), symmetric=TRUE): infinite or missing values in x*). There aren't any missing values.

Based on advice by Pavel Kirsti adding a control term 'control = control.ergm(MCMC.prop.weights='0inflated')'together with the Poisson reference distribution could work. 

The following problem was a singular approximate Hessian matrix. Often the reason is collinearity between variables. If I understand it correctly, the Hessian matrix is a derivative (*second order partial derivative*). It describes local points (curves) of a function which has many variables. It is singular if at least (?) two variables in the Hessian matrix (or its approximation) are the same and thus have a perfect (positive) correlation

The first solution was to check collinearity between variables. The command 'kcycle' calculates path or cycle census information. The output in our case 'kcycle(developer_net, mode="graph", maxlen = 4)' provides in the first column the aggregate of path counts and in the other columns information abhout path counts for each developer's. Concretely, it counts how often someone has two paths, three cycle, 4 cycle
*Example*
   Agg chiara.moretti fabio.boldrin alessandro.basso andrea.rana
2    0              0             0                0           0
3  453             48           103               82           6
4 4041            585          1230              982          42


### Hypothesis for model 1
Model 1 does not make a distinction between the type of relationship
The following effects are included:
1. sum: Sum is the valued version of edges. It controls for the general intensity of relationships in the network (control variable).
2. nonzero: Might be excluded. In sparse matrices, this term accounts for the possibility of a tie, when most ties are zero (control variable). 71 % of relationships do not exist.
3. Nodematch - location: This test if relationships are influenced by location (homophily argument). 
4. Nodematch - Title: This test if relationships are influenced by title (homophily argument).
5. Nodematch - Contract: This test if relationships are influenced by title (homophily argument).
6. Nodesqrtcovar: Individuals have different propensity to interact. This terms accounts for the individual differences (control variable).
7. Transitiveweights: A stable/ common social structure are triads. As we are modeling the appearance of relationships, taking into account that ties often appear in clusters, helps to get a better understanding of the effects. A negative transitive weights sugest hiearchical structures as ties are not formed with everyone equally. Transitiveweights takes 3 arguments: twopath, combine, affect. The default values of 'min', 'max', and 'min' are used. Twopath measures the strenght ot the two paths between i and k, and k and j). Given the strength of the two paths from i to k and k to j, combine measures their strenght on the path i to j. Finally, given this combined strenght, what is its affect on the the relationship i to j. 

- occurence of non-mirroring is more likely when performing routine tasks vs innovative tasks --> measured using edgecov (covariate)
###

