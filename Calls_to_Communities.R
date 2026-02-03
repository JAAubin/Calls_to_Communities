####FROM CALLS TO COMMUNITIES: ACOUSTIC MONITORING OF CONTACT CALLS INDICATES SPATIAL STRUCTURE IN ENDANGERED BELUGAS ====

#Jaclyn AubiN
#Started Nov. 2023

### packages ----

# #Install CMRNet from github
# remotes::install_github("matthewsilk/CMRnet", build_vignettes = FALSE)
# library(CMRnet)
# 
# #install old version of timeDate (until CMRnet is updated)
# remotes::install_version("timeDate", version = "3043.102")

Packages <- c("data.table", 'ggplot2', 'stringr', 'lubridate', 'klaR', 'MASS', 'caret', 'dplyr', 'UpSetR', 'CMRnet', 'igraph')
lapply(Packages, require, character.only = TRUE)

###read in data ----
DT <- read.csv('input/contact_calls_by_site.csv')
DT <- as.data.table(DT)

###Summarizing the data ----
#Number of unique types
DT[, uniqueN(type)]

#Number of calls and call rates by site
sitea <- DT[site == 'A', .N]
sitea/1676.7
siteb <- DT[site == 'B', .N]
siteb/1108.3
sitec <- DT[site == 'C', .N]
sitec/1411.6
sited <- DT[site == 'D', .N]
sited/483
sitee <- DT[site == 'E', .N]
sitee/563
sitef <- DT[site == 'F', .N]
sitef/692

###Verifying the classification of contact calls (linear discriminant analysis) ----
#following http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/

#remove rows where a contact call appears fewer than 10 times (model is sensitive to small classes)
DT[,nrows := .N, by = type]
DT2 <- DT[nrows > 9]
DT2 <- na.omit(DT2)
DT2[, uniqueN(type)]

# ST_type <- ST[, .SD, .SDcols = c('TYPE', 'ff' , 'ef', 'df', 'ss', 'ip', 'f95cc', 'lf', 'dt' )]

#check for correlated call features
cor_matrix <- cor(DT2[, c('hf', 'df', 'pf', 'dt', 'snr', 'ip', 'sig_segm', 'sf', 'ef', 'ff', 'df_cc', 'pf_cc', 'cf_cc', 'f5_cc', 'f25_cc', 'f75_cc', 'f95_cc', 'lf_cc', 'hf_cc')])
print(cor_matrix)

#uncorrelated variables to keep: 
DT3 <- DT2[, c('type', 'hf', 'dt', 'snr', 'ip', 'sig_segm', 'ff', 'pf', 'hf_cc', 'pf_cc', 'cf_cc', 'f5_cc', 'f95_cc', 'lf_cc')]

#doublecheck for correlated call features
cor_matrix <- cor(DT3[, !c('type')])
print(cor_matrix)
#we have removed one variable for each correlated pair

#use a stepwise linear discriminant analysis to determine which variables are good predictors
stepwise_lda <- stepclass(type~., data = DT3, method = "lda", direction = "both", improvement = 0.01)
#good predictors identified: ff, ip, hf, snr, sig_segm, f95_cc, dt, pf

###Construct an LDA to test the accuracy of our subjective classification
DT4 <- DT3[, c('type', 'ff', 'ip', 'hf', 'snr', 'sig_segm', 'pf', 'f95_cc', 'dt')]

# Run the model multiple times with different random splits
n_iterations <- 100
accuracies <- numeric(n_iterations)

for(i in 1:n_iterations) {
  set.seed(i)  # Different seed each time
  
  # Split data
  training.samples <- DT4$type %>%
    createDataPartition(p = 0.95, list = FALSE)
  train.data <- DT4[training.samples, ]
  test.data <- DT4[-training.samples, ]
  
  # Preprocess
  preproc.param <- train.data %>%
    preProcess(method = c("center", "scale"))
  train.transformed <- preproc.param %>% predict(train.data)
  test.transformed <- preproc.param %>% predict(test.data)
  
  # Fit and predict
  model <- lda(type~., data = train.transformed)
  predictions <- model %>% predict(test.transformed)
  
  # Store accuracy
  accuracies[i] <- mean(predictions$class == test.transformed$type)
}

# Examine the distribution
summary(accuracies)
mean(accuracies)
sd(accuracies)

###Limit re-detections to re-detections with >24h interval ----
#make a column with lag time between previous row and current row
setorder(DT, cols = 'type', 'datetime')
DT[, diff_sec := as.numeric(difftime(datetime, shift(datetime), units = 'secs')), by = 'type']

#Keep only re-detections with a more than 24h interval
DT[diff_sec > 86400, diff_sec := NA]
DTnb <- DT[is.na(diff_sec)]
DTnb[, uniqueN(type)]

#Check what re-detections look like with a 12h interval instead
# DT[, diff_sec2 := as.numeric(difftime(datetime, shift(datetime), units = 'secs')), by = 'type']
# DT[diff_sec2 > 43200, diff_sec := NA]
# DTnb12<- DT[is.na(diff_sec2)]
# DTnb12[, uniqueN(type)]

###Construct an upset plot to illustrate redetections ----

#include only calls that were resampled at least once
DTnb2 <- DTnb[, c('type', 'site')]
DTnb2[, 'count' := .N, by = 'type']
DTnb3 <- DTnb2[count > 1]
DTnb3[, uniqueN(type)]

##upset plot
# Assuming your data is in long format with columns: call_type, site
# Convert to binary matrix (call_type x site)
upset_data <- dcast(DTnb3 , type ~ site, 
                    fun.aggregate = length, value.var = "site")

# Convert counts to binary (0/1)
cols_to_convert <- names(upset_data)[-1]  # all columns except 'type'
upset_data[, (cols_to_convert) := lapply(.SD, function(x) as.integer(x > 0)), 
           .SDcols = cols_to_convert]

upset_df <- as.data.frame(upset_data[, !"type"])

upset_plot <- upset(upset_df, 
                    sets = c("F", "E", "D", "C", "B", "A"),
                    keep.order = TRUE,
                    nsets = 6,
                    order.by = "freq",
                    mainbar.y.label = "Number of call types re-detected",
                    sets.x.label = "Call types per site",
                    main.bar.color = "#6f6f6f", 
                    matrix.color = "#6f6f6f",
                    sets.bar.color = c('#2e86c1', '#5dade2', '#20b2aa', 
                                       '#48d1cc', '#ff7043', "#ff4757"), 
                    point.size = 3.5, 
                    line.size = 1,
                    text.scale = 1.3,
                    queries = list(
                      list(query = intersects, params = list("F"), color = '#2e86c1', active = TRUE),
                      list(query = intersects, params = list("E"), color = '#5dade2', active = TRUE),
                      # NO query for D - it has zero unique call types
                      list(query = intersects, params = list("C"), color = '#48d1cc', active = TRUE),
                      list(query = intersects, params = list("B"), color = '#ff7043', active = TRUE),
                      list(query = intersects, params = list("A"), color = "#ff4757", active = TRUE)
                    ))

upset_plot

png(filename = "output/upset_Feb03.png", width = 6, height = 4, units = "in", res = 300)
print(upset_plot)
dev.off()

###Construct repertoire accumulation curves for each site ----
##Generate the curves
### Generate curve for site A
DT_A <- DTnb[site == 'A']
calls_A <- DT_A$type
rac <- function(calls_A) {
  n <- length(calls_A)
  counts <- numeric(n)
  for (i in 1:n) {
    counts[i] <- length(unique(calls_A[1:i]))
  }
  return(counts)
}
reps <- 100
n <- length(calls_A)
racs_A <- matrix(0, nrow = n, ncol = reps)
for (i in 1:reps) {
  sample_items_A <- sample(calls_A, n, replace = FALSE)
  rac_values_A <- rac(sample_items_A)
  racs_A[, i] <- rac_values_A
}
mean_rac_A <- apply(racs_A, 1, mean)
sd_rac_A <- apply(racs_A, 1, sd)

### Generate curve for site B
DT_B <- DTnb[site == 'B']
calls_B <- DT_B$type
rac <- function(calls_B) {
  n <- length(calls_B)
  counts <- numeric(n)
  for (i in 1:n) {
    counts[i] <- length(unique(calls_B[1:i]))
  }
  return(counts)
}
reps <- 100
n <- length(calls_B)
racs_B <- matrix(0, nrow = n, ncol = reps)
for (i in 1:reps) {
  sample_items_B <- sample(calls_B, n, replace = FALSE)
  rac_values_B <- rac(sample_items_B)
  racs_B[, i] <- rac_values_B
}
mean_rac_B <- apply(racs_B, 1, mean)
sd_rac_B <- apply(racs_B, 1, sd)

### Generate curve for site C
DT_C <- DTnb[site == 'C']
calls_C <- DT_C$type
rac <- function(calls_C) {
  n <- length(calls_C)
  counts <- numeric(n)
  for (i in 1:n) {
    counts[i] <- length(unique(calls_C[1:i]))
  }
  return(counts)
}
reps <- 100
n <- length(calls_C)
racs_C <- matrix(0, nrow = n, ncol = reps)
for (i in 1:reps) {
  sample_items_C <- sample(calls_C, n, replace = FALSE)
  rac_values_C <- rac(sample_items_C)
  racs_C[, i] <- rac_values_C

}
mean_rac_C <- apply(racs_C, 1, mean)
sd_rac_C <- apply(racs_C, 1, sd)

  ### Generate curve for site D
  DT_D <- DTnb[site == 'D']
  calls_D <- DT_D$type
  rac <- function(calls_D) {
    n <- length(calls_D)
    counts <- numeric(n)
    for (i in 1:n) {
      counts[i] <- length(unique(calls_D[1:i]))
    }
    return(counts)
  }
  reps <- 100
  n <- length(calls_D)
  racs_D <- matrix(0, nrow = n, ncol = reps)
  for (i in 1:reps) {
    sample_items_D <- sample(calls_D, n, replace = FALSE)
    rac_values_D <- rac(sample_items_D)
    racs_D[, i] <- rac_values_D
  }
  mean_rac_D <- apply(racs_D, 1, mean)
  sd_rac_D <- apply(racs_D, 1, sd)
  
  ### Generate curve for site E
  DT_E <- DTnb[site == 'E']
  calls_E <- DT_E$type
  rac <- function(calls_E) {
    n <- length(calls_E)
    counts <- numeric(n)
    for (i in 1:n) {
      counts[i] <- length(unique(calls_E[1:i]))
    }
    return(counts)
  }
  reps <- 100
  n <- length(calls_E)
  racs_E <- matrix(0, nrow = n, ncol = reps)
  for (i in 1:reps) {
    sample_items_E <- sample(calls_E, n, replace = FALSE)
    rac_values_E <- rac(sample_items_E)
    racs_E[, i] <- rac_values_E
  }
  mean_rac_E <- apply(racs_E, 1, mean)
  sd_rac_E <- apply(racs_E, 1, sd)
  
### Generate curve for site F
DT_F <- DTnb[site == 'F']
calls_F <- DT_F$type
rac <- function(calls_F) {
  n <- length(calls_F)
  counts <- numeric(n)
  for (i in 1:n) {
    counts[i] <- length(unique(calls_F[1:i]))
  }
  return(counts)
}
reps <- 100
n <- length(calls_F)
racs_F <- matrix(0, nrow = n, ncol = reps)
for (i in 1:reps) {
  sample_items_F <- sample(calls_F, n, replace = FALSE)
  rac_values_F <- rac(sample_items_F)
  racs_F[, i] <- rac_values_F
}
mean_rac_F <- apply(racs_F, 1, mean)
sd_rac_F <- apply(racs_F, 1, sd)

# Create a data frame with the means and standard deviations
df_A <- data.frame(x = 1:length(mean_rac_A), y = mean_rac_A, sd = sd_rac_A, dataset = 'A')
df_B <- data.frame(x = 1:length(mean_rac_B), y = mean_rac_B, sd = sd_rac_B, dataset = 'B')
df_C <- data.frame(x = 1:length(mean_rac_C), y = mean_rac_C, sd = sd_rac_C, dataset = 'C')
df_D <- data.frame(x = 1:length(mean_rac_D), y = mean_rac_D, sd = sd_rac_D, dataset = 'D')
df_E <- data.frame(x = 1:length(mean_rac_E), y = mean_rac_E, sd = sd_rac_E, dataset = 'E')
df_F <- data.frame(x = 1:length(mean_rac_F), y = mean_rac_F, sd = sd_rac_F, dataset = 'F')

df <- rbind (df_A, df_B, df_C, df_D, df_E, df_F)
df<- as.data.table(df)

#make the plot
out <- ggplot(df, aes(x = x, y = y, group = dataset, color = dataset)) +
  geom_ribbon(aes(ymin = y - sd, ymax = y + sd, fill = dataset), alpha = 0.3) +
  geom_line(linewidth = 1) +
  geom_rect(aes(xmin = 0, xmax = 25, ymin = 0, ymax = 20), 
            fill = "transparent", 
            color = "black",
            linetype = 'dashed',
            linewidth = 0.5)  +
  labs(x = "Calls sampled", y = "Call types") +
  theme_classic() +
  ylim(0, 85) +
  xlim(0, 160) +
  theme(text = element_text(size = 16),
        legend.key.size = unit(1.5, "lines")) + # Increase the size of legend keys for spacing
  scale_color_manual(values = c('#ff4757', '#ff7043', '#48d1cc', '#20b2aa', '#5dade2', '#2e86c1')) +
  scale_fill_manual(values =c('#ff4757', '#ff7043', '#48d1cc', '#20b2aa', '#5dade2', '#2e86c1'))

out

#create the inset plot
df2 <- rbind (df_E,df_D)

zoom <- ggplot(df2, aes(x = x, y = y, group = dataset, color = dataset)) +
  geom_ribbon(aes(ymin = y - sd, ymax = y + sd, fill = dataset), alpha = 0.3) +
  geom_line(size = 1) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.ticks = element_blank(),  # Remove axis ticks
        axis.text = element_blank(),    # Remove axis text
        plot.background = element_rect(fill = "white", color = NA),  # White background for the inset plot
        panel.background = element_rect(fill = "white", color = "black", size = 0.5),  # White panel with black border
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines) +
        text = element_text(size = 16)) +
  guides(fill = 'none', color = 'none') +
  scale_color_manual(values = c('#20b2aa', '#5dade2')) +
  scale_fill_manual(values = c('#20b2aa', '#5dade2'))

zoom

inset_grob <- ggplotGrob(zoom)

final_plot <- out +
  annotation_custom(grob = inset_grob, xmin = 80, xmax = 145, ymin = -5, ymax = 50)

final_plot

ggsave(filename = "output/Fig3_feb03_2.png", plot = final_plot, width = 8, height = 5, dpi = 300)

###Construct the movement network ----

##Tidy up the data
#Turn datetime into date
DT[, 'date' := as.character(as.Date(datetime))]

#Rename columns to suit CMRnet
DT[, 'id' := as.numeric(.GRP), by = type]
DT[, 'loc' := site]

#add coordinates
DT[site == 'A', x := 48.249695]
DT[site == 'A', y := -69.968731]
DT[site == 'B', x := 48.202048]
DT[site == 'B', y := -69.897806]
DT[site == 'C', x := 47.929794]
DT[site == 'C', y := -69.550552]
DT[site == 'D', x := 47.849620]
DT[site == 'D', y := -69.591867]
DT[site == 'E', x := 47.940624]
DT[site == 'E', y := -69.6431047]
DT[site == 'F', x := 47.627303]
DT[site == 'F', y := -69.867303]

#Remove extra columns
DTcmr <- DT[, c('id', 'loc', 'x', 'y', 'date')]

##Define the model parameters
mindate <- "2017-07-24"
maxdate <- "2023-07-24"
intwindow <-1 #days
netwindow <-72 #months
overlap <-0

##Construct the movement model
mvmt <-
  MoveNetCreate(
    data = DTcmr,
    intwindow = intwindow,
    mindate = mindate,
    maxdate = maxdate,
    netwindow = netwindow,
    overlap = overlap,
    nextonly = FALSE,
    index = FALSE
  ) 

net <- cmr_igraph(mvmt,type='movement')

cmrMovPlot(nets=net,
           fixed_locs=TRUE,
           dynamic=TRUE, label = TRUE)

#cluster into communities
community <- cluster_infomap(net[[2]])
print(membership(community))
modularity(community)
#modularity of 0.40 indicates strong community structure

#plot the communities
membership_vector <- membership(community)
unique_communities <- unique(membership_vector)
community_colors <- c("#c74a4a", "#faae66")  # Adjust as needed
vertex_colors <- community_colors[membership_vector]

plot(net[[2]], 
     vertex.color = vertex_colors,  # Color by community membership
     vertex.size = 35,                        # Size of the vertices
     vertex.label = V(net[[2]])$name,                # Label the vertices (if you have names)
     edge.arrow.size = 0.75,
     vertex.label.color = 'black',
     edge.color = 'black',# Size of arrow heads
     layout = layout_with_fr)    

png(filename = "output/Fig5_Feb03.png", width = 6, height = 4, units = "in", res = 300)
dev.off()

#degree represents number of edges in and out of node (directed so not just counting lines)
degree(net[[2]], mode = 'in')
degree(net[[2]], mode = 'out')
degree(net[[2]])

#edge density is number of edges compared to total possible edges
graph_density <- edge_density(net[[2]])
print(graph_density)
#0.366 indicates about a third of edges possible are present, partial connectivity

#betweenness
betw <- betweenness(net[[2]])
print(betw)

#eigenvector centrality
weights_first <- E(net[[2]])$weight
eigen <- eigen_centrality(net[[2]], directed = TRUE, weights = weights_first)
eigenvector_scores <- eigen$vector
print(eigenvector_scores)

##Permutations of the network
perm <- DatastreamPermSpat(
  data = DTcmr,
  intwindow=1,
  mindate=mindate,
  maxdate=maxdate,
  netwindow=72,
  overlap=0,
  nextonly = TRUE,
  same.time=FALSE,
  time.restrict=1,
  spat.restrict=1000,
  same.id=FALSE,
  n.swaps=5,
  n.rand=1000,
  burnin = TRUE,
  n.burnin=100,
  warn.thresh=1000,
  iter = TRUE
)

#plot one random network for Figure 5
mat <- as.matrix(perm[[2]][[1]][,,666])
graph <- graph_from_adjacency_matrix(mat, weighted = TRUE, mode = "directed", diag = FALSE)
community <- cluster_infomap(graph)
membership_vector <- membership(community)
unique_communities <- unique(membership_vector)
community_colors <- c("#AEAEAE", '#868686')  # Adjust as needed
vertex_colors <- community_colors[membership_vector]

plot(graph,
     vertex.color = vertex_colors,  # Color by community membership
     vertex.size = 35,                        # Size of the vertices
     vertex.label = V(net[[2]])$name,                # Label the vertices (if you have names)
     edge.arrow.size = 0.75,
     vertex.label.color = 'black',
     edge.color = 'black',# Size of arrow heads
     vertex.label.cex = 1.5, 
     layout = layout_with_fr)

text(x = 1.5, y = 1.5, labels = "(b)", cex = 2, col = "black")
dev.off()

#modularity of permutated networks
mod_scores <- list()

for(i in 1:1000){
  mat <- as.matrix(perm[[2]][[1]][,,i])
  graph <- graph_from_adjacency_matrix(mat, weighted = 'weight', mode = "directed", diag = FALSE)
  community <- cluster_infomap(graph)
  mod_scores[[i]] <- modularity(community)
}

mod <- unlist(mod_scores)
mod <- as.data.table(mod)

mod[, mean(mod)]
mod[, sd(mod)]

#compare to the modularity of the true network
wilcox_test <- wilcox.test(mod$mod, mu = 0.40)
print(wilcox_test)

#edge density of randomized networks

edge <- list()
for(i in 1:1000){
  mat <- as.matrix(perm[[2]][[1]][,,i])
  graph <- graph_from_adjacency_matrix(mat, weighted = 'weight', mode = "directed", diag = FALSE)
  edge[[i]] <- edge_density(graph)
}

edge <- as.data.table(unlist(edge))
edge[, mean(V1)]
edge[, sd(V1)]

#compare to the edge density of the true network
wilcox_test <- wilcox.test(edge$V1, mu = 0.37)
print(wilcox_test)
