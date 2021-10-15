
# -------------------------------------------------------------------------
#
#                     Code template - Network analysis
#
# -------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

if (!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
if (!require(readxl)) {install.packages("readxl")}; library(readxl)
if (!require(car)) {install.packages("car")}; library(car)
if (!require(psych)) {install.packages("psych")}; library(psych)
if (!require(gtsummary)) {install.packages("gtsummary")}; library(gtsummary)
if (!require(naniar)) {install.packages("naniar")}; library(naniar)
if (!require(GGally)) {install.packages("GGally")}; library(GGally)
if (!require(bootnet)) {install.packages("bootnet")}; library(bootnet)
if (!require(qgraph)) {install.packages("qgraph")}; library(qgraph)
if (!require(mgm)) {install.packages("mgm")}; library(mgm)
if (!require(igraph)) {install.packages("igraph")}; library(igraph)
if (!require(NetworkComparisonTest)) {install.packages("NetworkComparisonTest")}; library(NetworkComparisonTest)



# Load your data ----------------------------------------------------------

# The data came from a subsample of the National Health Survey - Brazil 2013.
# 1000 participants answered the PHQ-9 to assess depressive symptoms. Also, we
# have the sex of the participants as sociodemographic indicator.

# Type of data: cross-sectional
# Type of variables: 9 continuous and 1 dichotomous

dataset <- readxl::read_excel(path = "PNS_depression_2021-10-14.xlsx")


# Inspect your data -------------------------------------------------------

# Use View() function to look at your data
View(dataset)

# Check variable names
names(dataset)

# Rename variables
## Here, use the function dplyr::rename() to rename
## The syntax is: new_name = old_name

dataset <- dataset %>%
  dplyr::rename(sex = C006,
                sleep = N010,
                tiredness = N011,
                anhedonia = N012, 
                concentration = N013,
                appetite = N014,
                motor_behavior = N015, 
                depressed_mood = N016,
                inutility = N017,
                suicidal_thoughts = N018)


# Recode variables

# Forcing continuous to be continuous
dataset <- dataset %>%
  purrr::map_df(as.numeric)

## Here, I want to recode 'sex', transforming it into a categorical variable
## We can use the function car::recode()

dataset$sex <- car::recode(dataset$sex,
                           "1 = 'Male';
                            2 = 'Female'",
                           as.factor = TRUE)


# -------------------------------------------------------------------------
# Network analysis --------------------------------------------------------
# -------------------------------------------------------------------------


# First, we will look at the depressive symptoms network (remove sex)

dataset_net <- dataset %>%
  select(-sex)

# The ideia is to estimate a partial correlation network (GGM). This is the most
# common approach in psychological networks.
# Because of this, I will assume the data is multivariate normally distributed
# and the items are continuous.
# However, you have to check these assumptions in your own dataset.



# Partial correlation network ---------------------------------------------

# With no regularization ----

# Estimate the network
ggm_pcor <- bootnet::estimateNetwork(dataset_net,
                                     default = "pcor")

print(ggm_pcor) # density of the network: 36 / 36 = 1 (fully connected network)

# Plot the network
plot(ggm_pcor,
     layout = "spring", # Fruchterman-Reingold node placement algorithm
     theme = "Borkulo") # good edge colors for color blindness



# With regularization (LASSO) ----

# This is the most common method. We need to define how "good" or "bad"
# we will be with the network edges by defining the EBIC tuning parameter
# Usually, it is set to 0.5 (we are expecting few edges in the network).
# But, you could decrease it. Increase this value is not recommended.
# If you put the tuning parameter to 0, you are askinf for BIC selection method

# Estimate the network
ggm_ebicglasso <- bootnet::estimateNetwork(dataset_net,
                                           default = "EBICglasso",
                                           tuning = 0.5)

print(ggm_ebicglasso) # Density: 30 / 36 = 0.83 (dense network)

# Get the adjacency matrix
matrix_ggm_ebicglasso <- ggm_ebicglasso$graph

# Predictability
# If you are looking for how well nodes are predicted by the others,
# you can run a mgm method (runs nodewise regressions).
# For my knowledge, this is the only method able to compute predictability.

# Define the type and the level of variables
type <- c(rep("g", 9)) # g means gaussian (continuous). All the variables are continuous. Other options are "c" for categorical and "p" for poisson.
level <- c(rep(1, 9)) # 1 means variables are continuous. If they were categorical, you have to identify how much categories each variable has.

# Estimate MGM network
mgm_ebicglasso <- bootnet::estimateNetwork(dataset_net,
                                           default = "mgm",
                                           criterion = "EBIC",
                                           type = type,
                                           level = level,
                                           tuning = 0.5, 
                                           rule = "AND") # computes edges just if they are non-zero in both directions

predict_output <- predict(mgm_ebicglasso$results, data = dataset_net)
predictability <- predict_output$errors$R2
mean(predictability) # computes the predictability mean


# Community analysis
# If you want to estimate communities for your variables,
# you can use the walktrap algorithm, for example

g <- qgraph::qgraph(matrix_ggm_ebicglasso, DoNotPlot = TRUE)
igr <- igraph::as.igraph(g) # transform into an igraph object
walktrap <- igraph::cluster_walktrap(igr)
walktrap <- walktrap$membership; walktrap # 2 communities; membership stores the community each variable belongs to. For example, the first variable belongs to the community 2 and the last variable, community 2 
community <- list("Community 1" = which(walktrap == 1),
                  "Community 2" = which(walktrap == 2))


# Colors for groups
# You can use any color palette, but here I will use a R native palette
community_colors <- palette.colors(n = 3)[-1] # remove the first color (black)

# Define beautiful names for your variables and store in a vector
var_names <- c("Sleep problems", "Tiredness",
               "Anhedonia", "Concentration problems", "Appetite problems",
               "Problems in motor behavior", "Depressed mood",
               "Feeling of inutility", "Suicidal thoughts")

# Plot the network
net_ebicglasso <- plot(ggm_ebicglasso, # output of estimateNetwork
                       layout = "spring",
                       theme = "Borkulo",
                       maximum = max(abs(matrix_ggm_ebicglasso)), # defines the biggest thickness of the edge. Here, the maximum partial correlation
                       pie = predictability, # pie is the border of the node. We can plot the predictability here
                       labels = seq_along(dataset_net), # put numbers inside the nodes
                       vsize = 6, # defines the node size
                       groups = community,
                       nodeNames = var_names, # node names
                       legend.cex = 0.4, # legend size
                       color = community_colors, # colors for nodes
                       pieBorder = 0.25, # pie size
                       label.cex = 1.5, # size of labels inside nodes (numbers)
                       GLratio = 1.8) # network size in the plot
#                      filetype = "pdf",
#                      filename = "net-ebicglasso")









# -------------------------------------------------------------------------
# Centrality measures -----------------------------------------------------
# -------------------------------------------------------------------------

# (if you want, create a new script and paste this analysis there.)

# Here, we will estimate centrality measures.
# The most common is: strength, closeness, betweenness, and expected influence

centrality <- centralityPlot(net_ebicglasso, labels = var_names,
                             include = c("Strength", "Closeness", "Betweenness",
                                         "ExpectedInfluence"),
                             scale = "z-score") # the values are in z-score. If you want raw values, type "raw"

# Save the plot
ggplot2::ggsave(filename = "centrality-measures.png",
                plot = centrality,
                device = "png",
                width = 16,
                height = 12,
                units = c("cm"))












# -------------------------------------------------------------------------
# Accuracy, stability, and difference test --------------------------------
# -------------------------------------------------------------------------

# Here, we will use the estimateNetwork output as our data.

# Accuracy and difference test----

# The method below computes both the accuracy and the difference test
# We ask for what we want in the statistics argument.
# edge means edge accuracy and difference test for edges.
# centrality measures mean difference test for centrality measures

# This procedure takes longer if you have bigger sample size,
# more parameters, and if the method to estimate the network takes more time

boot_ggm_accuracy <- bootnet::bootnet(ggm_ebicglasso,
                                      nBoots = 1000, # the number of resamples/iterations (1000 is the default)
                                      type = "nonparametric",
                                      nCores = 1, # the number of cores in your computer. I have just 1 core
                                      statistics = c("edge",
                                                     "strength",
                                                     "expectedInfluence",
                                                     "closeness",
                                                     "betweenness"))


# To see the results, we can use the plot function

# Plot edge accuracy and save in pdf
edge_accuracy <- plot(boot_ggm_accuracy, labels = FALSE, order = "sample")
pdf("edge_accuracy.pdf")
plot(edge_accuracy)
dev.off() # close the pdf


# Plot edge differences and save in pdf
edge_differences <- plot(boot_ggm_accuracy, plot = "difference",
                         onlyNonZero = TRUE,
                         order = "sample")
pdf("edge_differences.pdf")
plot(edge_differences)
dev.off() # close the pdf


# Plot centrality differences and save in pdf
centrality_differences <- plot(boot_ggm_accuracy,
                               statistics = c("strength", "expectedInfluence",
                                              "closeness", "betweenness"),
                               order = "sample")
pdf("centrality_differences.pdf")
plot(centrality_differences)
dev.off() # close the pdf











# -------------------------------------------------------------------------
# Mixed Graphical Models (MGM) --------------------------------------------
# -------------------------------------------------------------------------

# As an example, I will create a mgm, mixing continuous and categorical variables
# When you are running, pay attention to the type and level.
# We have to define the information of the variables in the right order

# PS: sex is a factor and the mgm (and network procedures, in general)
# need integer or numeric variables. So, I have to transform sex in order 
# to build the model

dataset$sex <- as.numeric(dataset$sex)
unique(dataset$sex) # 1 = female; 2 = male

# I want the "success" be female sex, so I need to recode it
dataset$sex <- car::recode(dataset$sex, "2 = 0")

type <- c("c", rep("g", 9)) # my first variable, sex, is dichotomous.
level <- c(2, rep(1, 9)) # sex has two categories

mgm_full_dataset <- estimateNetwork(dataset,
                                    default = "mgm",
                                    criterion = "EBIC",
                                    type = type,
                                    level = level,
                                    tuning = 0.25, # the recommendation for mgm
                                    rule = "AND")

# Get the matrix
mgm_matrix <- mgm_full_dataset$graph

predict_output <- predict(mgm_full_dataset$results, data = dataset)
predictability <- predict_output$errors$R2
predictability[1] <- predict_output$errors$CCmarg[1] # sex is categorical, doesn't accept R2
mean(predictability) # computes the predictability mean


# Community analysis
# If you want to estimate communities for your variables,
# you can use the walktrap algorithm, for example

g <- qgraph::qgraph(mgm_matrix, DoNotPlot = TRUE)
igr <- igraph::as.igraph(g) # transform into an igraph object
walktrap <- igraph::cluster_walktrap(igr)
walktrap <- walktrap$membership; walktrap # 2 communities; membership stores the community each variable belongs to. For example, the first variable belongs to the community 2 and the last variable, community 2 
community <- list("Community 1" = which(walktrap == 1),
                  "Community 2" = which(walktrap == 2))


# Colors for groups
# You can use any color palette, but here I will use a R native palette
community_colors <- palette.colors()[-1] # remove the first color (black)

# Define beautiful names for your variables and store in a vector
var_names <- c("Sex", "Sleep problems", "Tiredness",
               "Anhedonia", "Concentration problems", "Appetite problems",
               "Problems in motor behavior", "Depressed mood",
               "Feeling of inutility", "Suicidal thoughts")

# Plot the network
net_mgm <- plot(mgm_full_dataset, # output of estimateNetwork
                layout = "spring",
                theme = "Borkulo",
                maximum = max(abs(mgm_matrix)), # defines the biggest thickness of the edge. Here, the maximum partial correlation
                pie = predictability, # pie is the border of the node. We can plot the predictability here
                labels = seq_along(dataset), # put numbers inside the nodes
                vsize = 6, # defines the node size
                groups = community,
                nodeNames = var_names, # node names
                legend.cex = 0.4, # legend size
                color = community_colors, # colors for nodes
                pieBorder = 0.25, # pie size
                label.cex = 1.5, # # size of labels inside nodes
                GLratio = 1.8) # Network size
#               filetype = "pdf",
#               filename = "net-ebicglasso")




# You can repeat the same process to estimate centrality plots and accuracy/stability











# -------------------------------------------------------------------------
# Networks by groups ------------------------------------------------------
# -------------------------------------------------------------------------

# Here, the first thing to do is to create two new datasets, one for each
# category of your group variable

dataset$sex <- car::recode(dataset$sex,
                           "0 = 'Male';
                            1 = 'Female'",
                           as.factor = TRUE)
dataset_male <- dataset %>%
  filter(sex == "Male") %>%
  select(-sex)
dataset_female <- dataset %>%
  filter(sex == "Female") %>%
  select(-sex)

# Colors for groups
# You can use any color palette, but here I will use a R native palette
community_colors <- palette.colors()[-1] # remove the first color (black)

# Define beautiful names for your variables and store in a vector
var_names <- c("Sleep problems", "Tiredness",
               "Anhedonia", "Concentration problems", "Appetite problems",
               "Problems in motor behavior", "Depressed mood",
               "Feeling of inutility", "Suicidal thoughts")

# Now, we can estimate the network for each dataset


# Male --------------------------------------------------------------------

# Estimate the network
ggm_male <- bootnet::estimateNetwork(dataset_male,
                                     default = "EBICglasso",
                                     tuning = 0.5)

print(ggm_male) # Density: 26 / 36 = 0.72 (dense network)

# Get the adjacency matrix
matrix_ggm_male <- ggm_male$graph

# Predictability
# Define the type and the level of variables
type <- c(rep("g", 9))
level <- c(rep(1, 9))

# Estimate MGM network
mgm_male <- bootnet::estimateNetwork(dataset_male,
                                     default = "mgm",
                                     criterion = "EBIC",
                                     type = type,
                                     level = level,
                                     tuning = 0.5, 
                                     rule = "AND") # computes edges just if they are non-zero in both directions

predict_output <- predict(mgm_male$results, data = dataset_male)
predictability_male <- predict_output$errors$R2
mean(predictability_male) # computes the predictability mean


# Community analysis
# If you want to estimate communities for your variables,
# you can use the walktrap algorithm, for example

g <- qgraph::qgraph(matrix_ggm_male, DoNotPlot = TRUE)
igr <- igraph::as.igraph(g) # transform into an igraph object
walktrap <- igraph::cluster_walktrap(igr)
walktrap <- walktrap$membership; walktrap # 2 communities; membership stores the community each variable belongs to. For example, the first variable belongs to the community 2 and the last variable, community 2 
community_male <- list("Community 1" = which(walktrap == 1),
                       "Community 2" = which(walktrap == 2))

# Plot the network
net_male <- plot(ggm_male, # output of estimateNetwork
                 layout = "spring",
                 theme = "Borkulo",
                 maximum = max(abs(matrix_ggm_male)), # defines the biggest thickness of the edge. Here, the maximum partial correlation
                 pie = predictability_male, # pie is the border of the node. We can plot the predictability here
                 labels = seq_along(dataset_male), # put numbers inside the nodes
                 vsize = 6, # defines the node size
                 groups = community_male,
                 nodeNames = var_names, # node names
                 legend.cex = 0.4, # legend size
                 color = community_colors, # colors for nodes
                 pieBorder = 0.25, # pie size
                 label.cex = 1.5, # size of labels inside nodes
                 GLratio = 1.8) # Network size
#                filetype = "pdf",
#                filename = "net-male")


# Female --------------------------------------------------------------------

# Estimate the network
ggm_female <- bootnet::estimateNetwork(dataset_female,
                                       default = "EBICglasso",
                                       tuning = 0.5)

print(ggm_female) # Density: 29 / 36 = 0.80 (dense network)

# Get the adjacency matrix
matrix_ggm_female <- ggm_female$graph

# Predictability
# Define the type and the level of variables
type <- c(rep("g", 9))
level <- c(rep(1, 9))

# Estimate MGM network
mgm_female <- bootnet::estimateNetwork(dataset_female,
                                       default = "mgm",
                                       criterion = "EBIC",
                                       type = type,
                                       level = level,
                                       tuning = 0.5, 
                                       rule = "AND") # computes edges just if they are non-zero in both directions

predict_output <- predict(mgm_female$results, data = dataset_female)
predictability_female <- predict_output$errors$R2
mean(predictability_female) # computes the predictability mean


# Community analysis
# If you want to estimate communities for your variables,
# you can use the walktrap algorithm, for example

g <- qgraph::qgraph(matrix_ggm_female, DoNotPlot = TRUE)
igr <- igraph::as.igraph(g) # transform into an igraph object
walktrap <- igraph::cluster_walktrap(igr)
walktrap <- walktrap$membership; walktrap # 2 communities; membership stores the community each variable belongs to. For example, the first variable belongs to the community 2 and the last variable, community 2 
community_female <- list("Community 1" = which(walktrap == 1),
                         "Community 2" = which(walktrap == 2))

# Plot the network
net_female <- plot(ggm_female, # output of estimateNetwork
                   layout = "spring",
                   theme = "Borkulo",
                   maximum = max(abs(matrix_ggm_female)), # defines the biggest thickness of the edge. Here, the maximum partial correlation
                   pie = predictability_female, # pie is the border of the node. We can plot the predictability here
                   labels = seq_along(dataset_female), # put numbers inside the nodes
                   vsize = 6, # defines the node size
                   groups = community_female,
                   nodeNames = var_names, # node names
                   legend.cex = 0.4, # legend size
                   color = community_colors, # colors for nodes
                   pieBorder = 0.25, # pie size
                   label.cex = 1.5, # size of labels inside nodes (numbers)
                   GLratio = 1.8) # Network size 
#                  filetype = "pdf",
#                  filename = "net-female")




# Plot networks together --------------------------------------------------

# It's recommended to plot the two networks with the same layout
net_layout <- averageLayout(net_male, net_female)

# The maximum of the edge sizes need to be the same, for the sake of comparison
maximum <- max(abs(c(unlist(matrix_ggm_male),
                     unlist(matrix_ggm_female))))

# Define the plot layout (where networks will be ploted)
layout(matrix(c(1, 2), nrow = 1))
qgraph(net_male, layout = net_layout,
       maximum = maximum, legend.cex = 0.25,
       mar = c(2, 2, 2, 1),
       title = "Male")
qgraph(net_female, layout = net_layout,
       maximum = maximum, legend.cex = 0.25,
       mar = c(2, 2, 2, 1),
       title = "Female")

# Save in pdf
pdf("network-sex.pdf", width = 15, height = 7)
layout(matrix(c(1, 2), nrow = 1))
qgraph(net_male, layout = net_layout,
       maximum = maximum, legend.cex = 0.45,
       mar = c(2, 2, 2, 1),
       title = "Male",
       GLratio = 1.5)
qgraph(net_female, layout = net_layout,
       maximum = maximum, legend.cex = 0.45,
       mar = c(2, 2, 2, 1),
       title = "Female",
       GLratio = 1.5)
dev.off()

# Normalize the layout
layout(matrix(1))




# Centrality measures -----------------------------------------------------

# Let's find centrality measures for both networks

centrality <- centralityPlot(list(net_male,
                                  net_female),
                             labels = var_names,
                             include = c("Strength", "Closeness", "Betweenness",
                                         "ExpectedInfluence"),
                             scale = "z-score") # the values are in z-score. If you want raw values, type "raw"
# Change the legend
centrality <- centrality + scale_color_discrete(name = "",
                                                labels = c("type 1" = "Male",
                                                           "type 2" = "Female"))

# Save the plot
ggplot2::ggsave(filename = "centrality-measures-sex.png",
                plot = centrality,
                device = "png",
                width = 16,
                height = 12,
                units = c("cm"))







# Network comparison test -------------------------------------------------


# Similarity
# Here, we can perform Spearman's correlation to see to what extent
# the matrixes are similar
cor(as.numeric(matrix_ggm_male), as.numeric(matrix_ggm_female), method = "spearman")


# To compare the networks, we will use the function NetworkComparisonTest::NCT()
# This is a permutation test with k iterations
# Data are the results from bootnet::estimateNetwork

net_comparison <- NCT(ggm_male, ggm_female,
                      it = 1000, # the number of iterations
                      test.edges = TRUE,
                      edges = "all",
                      test.centrality = TRUE,
                      centrality = c("strength", "closeness", "betweenness",
                                     "expectedInfluence"))

# Network structure invariance
net_comparison$nwinv.real # this result means the biggest edge difference between the two networks
net_comparison$nwinv.pval # this is the p-value. p < 0.05 and we assume the networks are different in structure

# Global strength invariance
net_comparison$glstrinv.real # this shows the difference between the global strength (i.e., the sum of all edges in a network) of the two networks.
net_comparison$glstrinv.sep # this shows the global strength of each network.
net_comparison$glstrinv.pval # p-value

# Edge weight invariance
sum(net_comparison$einv.pvals$`p-value` < 0.05) # how many edges was statistically different at p < 0.05?
net_comparison$einv.pvals %>%
  filter(`p-value` < 0.05) # shows the different edges

# Now, we will plot a network of the edge difference
# This network will have just those edges that were statistically different
# Blue edges means "for the male network, the edge x was bigger than for the female network"
# Red edges means the inverse; the edge was bigger for the female network

edge_difference <- net_comparison$einv.real
edge_pvalues <- net_comparison$einv.pvals
for (i in 1:nrow(edge_pvalues)) {
  edge_difference[as.character(edge_pvalues$Var1[i]),
                  as.character(edge_pvalues$Var2[i])] <- ifelse(edge_pvalues$`p-value`[i] < 0.05,
                                                                edge_difference[as.character(edge_pvalues$Var1[i]),
                                                                                as.character(edge_pvalues$Var2[i])], 0)
}
edge_difference[lower.tri(edge_difference)] <- t(edge_difference)[lower.tri(edge_difference)]

net_edge_difference <- qgraph(edge_difference,
                              layout = "spring",
                              theme = "Borkulo",
                              labels = seq_along(dataset_female), # put numbers inside the nodes
                              vsize = 6, # defines the node size
                              nodeNames = var_names, # node names
                              legend.cex = 0.4,
                              details = TRUE)






#             |￣￣￣￣￣￣￣￣￣￣￣￣￣|
#                  That's all, folks!
#             |＿＿＿＿＿＿＿＿＿＿＿＿＿| 
#                    (\__/)  ||
#                    (•ㅅ•)  ||
#                   /  　  づ

