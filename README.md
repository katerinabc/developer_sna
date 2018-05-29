<-- load the following source file with data to be included in this file -->
# Microdynamics in Software developer teams

**work-in-progress Paper**
## Data

The dataset consists of a 2 mode network linkig developer to software files. The edge is the work a developer has done on the file (a = added, m = modified). Additional data is available about developers (e.g., job role, work-location, type of contract) and about the software files (tecnical dependencies between software files)



## Task 1: Replication of Sosa (2015) paper

A full replication of the methodology by Sosa is only possible if the data is dichotomized. The  developer-to-developer network is a projection of the 2-mode matrix: Developer - Software file. The devleoper-to-developer matrix indicates which two developer worked on a file. It is an undirected  *collaboration* network. 

To run an ERGM the dependent network needs to contain binary data. The collaboration matrix for version 1 of the software is this
![Collaboration Network for Software Version 1.](~/Documents/gitrepo/developer_sna/fig/ugly_dev_1_network.png)

### Valued ERGM
For valued ERGMs it is necessary to provide a reference distribution. The space from which to draw sample networks. For example if the values in the networks can only be 0 or 1 then the sample distirbution is a uniform or truncated geometric distribution or a binominal distribution. In case there is no upper bound on the values of ties a *geometric* or *Poission* distribution should be used. Andy Pilny argues that the Poisson distribution is useful when average tie value isn't much different to the variance. If this is not the case geometric distribution should be used. This makes geometric distribution very useful for very skewed distribution of edge values, where most nodes are 0

# Hypothesis
# 