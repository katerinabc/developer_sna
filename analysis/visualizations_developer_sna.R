# Visualizations

library(igraph)
library(ggraph)

#better in Gephi for complete data ?

names(DF2)
g2 <- graph.adjacency(dev_v1, mode="directed", weighted=TRUE)
g2

ggraph(g2) + # raw depiction. improve for publication
  geom_edge_link(aes(edge_width = weight)) + #width = size arguemtn?
  geom_node_point()

dev_igraph <- graph.adjacency(developer, weighted=T)
V(dev_igraph)$location <- authatt$location
V(dev_igraph)$title <- authatt$jobtitle_raw

ggraph(dev_igraph) + 
  geom_edge_link(aes(edge_width = weight/1000)) + 
  geom_node_point(aes(shape = as.factor(location), color = as.factor(title)))
ggsave("developer_1_mode_projection.png")

# quick visualization
library(ggraph)
dev4g<- graph.adjacency(dev4, mode='undirected', weighted=T)

#quick visualization with igraph
plot(dev4g)

# customized visualization with ggraph
ggraph(dev4g, layout='kk') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                     end_cap = label_rect(node2.name)), 
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_label(aes(label = name)) + 
  theme_graph()
#not the nicest. 
#one arrow with no label
V(dev4g)$'name'
dev4g


ggplot(authatt, aes(x = author)) + geom_bar() + coord_flip() + facet_wrap(~ ver) + labs(title="Membership for each Version")
ggsave('developer_per_version.png')
ggplot(authatt, aes(x = ver)) + geom_bar() + coord_flip() + facet_wrap(~ author) + labs("Version participation")
ggsave("Version_per_developer.png")

df <- authatt[,c(1:2)]
names(df) <- c('group', 'person')
library(igraph)
library('Matrix')
A <- spMatrix(nrow=length(unique(df$person)),
              ncol=length(unique(df$group)),
              i = as.numeric(factor(df$person)),
              j = as.numeric(factor(df$group)),
              x = rep(1, length(as.numeric(df$person))) )
row.names(A) <- levels(factor(df$person))
colnames(A) <- levels(factor(df$group))
A
ownership_sequence <- reshape2::melt(as.matrix(A))
ownership_sequence[ownership_sequence$value == 0, 3] <- 'Not a member'
ownership_sequence[ownership_sequence$value == 1, 3] <- 'Member'
ggplot(ownership_sequence, aes(x = Var2, y = Var1)) + 
  guides(fill=guide_legend(title="Membership")) +
  geom_tile(aes(fill= as.factor(value))) + labs(title="Membership turnover", 
                                                x = "Version", 
                                                y = "Developers")
ggsave("membership_heatmap.png")

aut_ver <- graph.incidence(A)
V(aut_ver)$type <-c(rep('TRUE',21),rep("FALSE", 13) )
V(aut_ver)$color <- V(aut_ver)$type
V(aut_ver)$color=gsub("TRUE","red",V(aut_ver)$color)
V(aut_ver)$color=gsub("FALSE","blue",V(aut_ver)$color)
plot(aut_ver, edge.color="gray30", layout=layout_as_bipartite)
