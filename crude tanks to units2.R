library(igraph)

## Create edge list for definition of network (nodes and how they are connected)

el <- c("tk301","tk301_302_head","tk302","tk301_302_head","tk301_302_head","nthchg",
        "tk303","tk303_304_head","tk304","tk303_304_head","tk303_304_head","nthchg",
        "tk303_304_head","sthchg",
        "tk305","tk305_306_head","tk306","tk305_306_head","tk305_306_head","sthchg",
        "nthchg","CDU2","sthchg","CDU1",
        "tk303","tk303_304_inj_head","tk303_304_inj_head","CDU1",
        "tk303_304_inj_head","CDU2")
el <- matrix(el,ncol=2,byrow=TRUE)  #convert to matrix form
g <- graph_from_edgelist(el,directed = TRUE)   #Create the graph from edge list

## blah

#List of committed tanks (i.e. tank is feeding a CDU)
comm_tanks <- c("tk301","tk303","tk305")
tank_flows <- c(11000,3000,7000)

for(i in 1:length(comm_tanks)){
        #pth1 <- list()
        pth1[[i]] <- c(all_simple_paths(g,comm_tanks[i],"CDU1"),all_simple_paths(g,comm_tanks[i],"CDU2"))
}
        
allpaths <- unlist(pth1,recursive = FALSE)  #flatten the list without touching the sub-lists, i.e. now a single-level list of paths
allpaths_combos <- combn(1:length(allpaths),length(comm_tanks))   #all combinations of those paths

## Create a function and run it over all the combinations of these paths, test for independence and that they provide a path for each tank
isvalidcombo <- function(x){
        #x <- allpaths_combos[,1]
        g_subset <- induced_subgraph(g,unlist(allpaths[x]))
        valid_alltanks <- sum(comm_tanks %in% V(g_subset)$name) == length(comm_tanks)   #only want combos that provide a valid path for every tank committed
        adjmatrix <- as_adj(g_subset)   #create adjacency matrix for this combination of paths
        adjmatrix <- adjmatrix[,-c(which(colnames(adjmatrix)=="CDU1"),which(colnames(adjmatrix)=="CDU2"))]   #multiple streams can go to CDU, so drop
        valid_independent <- sum(colSums(as.matrix(adjmatrix)) > 1)==0   #only want combos where each node used 1 or less times (except CDUs)
        return(valid_alltanks & valid_independent)
}

valid_combos_idx <- apply(allpaths_combos,2,isvalidcombo)
valid_combos <- allpaths_combos[,valid_combos_idx]

## Convert the valid combinations of paths into pairs of tank and CDU, to calculate results
results_bytank <- list()
for(i in 1:dim(valid_combos)[[2]]){
        g_combo <- graph.empty()
        for(j in 1:length(allpaths[valid_combos[,i]])){
                g_combo <- g_combo + induced_subgraph(g,unlist(allpaths[valid_combos[,i]][[j]]))
        }
        result_map <- distances(g_combo,v=comm_tanks,to=c("CDU1","CDU2")) != Inf
        results_bytank[[i]] <- result_map*tank_flows
}
results_total <- lapply(results_bytank,colSums)
