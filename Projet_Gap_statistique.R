# Charger le package nécessaire
set.seed(123)  # pour la reproductibilité

# Générer des données pour chaque cluster
cluster1 <- data.frame(
  x = rnorm(50, mean = 8, sd = 1),
  y = rnorm(50, mean = 5, sd = 1)
)

cluster2 <- data.frame(
  x = rnorm(50, mean = 10, sd = 1),
  y = rnorm(50, mean = 10, sd = 1)
)

cluster3 <- data.frame(
  x = rnorm(50, mean = 12, sd = 1),
  y = rnorm(50, mean = 5, sd = 1)
)

# Combiner les clusters dans un seul data frame
data <- rbind(cluster1, cluster2, cluster3)
model=kmeans(data,1)
model$tot.withinss
model$size
length(model$size)
V=(model$tot.withinss)#/length(model$size -1)
V
runif(100,min = min(data$x),max = max(data$y))
# Visualiser les données
plot(data$x, data$y, col = rep(1:3, each = 50), pch = 19,
     xlab = "X", ylab = "Y", main = "Données avec 3 clusters")

model=list()
W=list()
log_W=list()
model_2=list()
W_2=list() ## vecteur des within des données simulées pour chaque valeur de k
log_W2=list() # vecteur contenant le log des W simulées simulations
Gap=list()
log_WS=list()
Sd=list()     # liste des écart types
S=list()      # Seuil de Tabshiki
nb_sim=50     ## Nb de simulation Monte Carlo
for (k in 1:10) {
  model[[k]]=kmeans(data,k)
  W[[k]]=(model[[k]]$tot.withinss)#/(length(model[[k]]$size)-1)
  B=log(W[[k]])
  log_W[[k]]=B
  #### Simulation Monté Carlo
  for (i in 1:nb_sim) {
    X=runif(nrow(data),min = min(data$x),max = max(data$x))
    #X=sample(data$x)
    Y=runif(nrow(data),min = min(data$y),max = max(data$y))
    #Y=sample(data$y)
    base=cbind(X,Y)
    model_2[[i]]=kmeans(base,k)
    W_2[[i]]=(model_2[[i]]$tot.withinss)
    
  }
  S=mean(log(unlist(W_2)))
  Gap[[k]]=S-B
  log_WS[[k]]=S
  Sd[[k]]=sqrt(sum((log(unlist(W_2))-S)**2)/nb_sim)
}
Gap

S=sqrt(1+1/nb_sim)*unlist(Sd)

########## Calcul écart type



k <- 1:10 
# Plot observed values
plot(k, unlist(log_W), type = "o", pch = 1, lty = 1, col = "black", 
     ylim = range(c(unlist(log_W), unlist(log_WS))),
     xlab = "number of clusters k", ylab = "obs and exp log(Wk)", 
     main = "Observed and Expected log(Wk)")

# Ajouter la seconde série de données
lines(k, unlist(log_WS), type = "o", pch = 15, lty = 2, col = "black")

# Ajouter une légende pour différencier les courbes
legend("topright", legend = c("Observed log(Wk)", "Expected log(Wk)"),
       pch = c(1, 15), lty = c(1, 2), col = "black")

## Shéma Gap statistique
plot(k, Gap, type = "o", pch = 1, lty = 1, col = "blue", 
     ylim = range(Gap), 
     xlab = "number of clusters k", ylab = "Gap Statistique", 
     main = "Gap Statistique")


###### Utilisation de la méthode de R.Tibshirani
Gap
Gap=unlist(Gap)
p=length(Gap)-1
nb_cluster=function(Gap){
  for (k in 1:p) {
    if (Gap[k]>(Gap[k+1]-S[k+1])){
      cat("Le nombre optimal de cluster est :", k)
      break
      
    }
  }

}


error_values <- c(0.05, 0.08, 0.07, 0.06, 0.08, 0.07, 0.06, 0.05, 0.09)
sd(error_values)



for (i in 1:3) {
  X=runif(nrow(data),min = min(data$x),max = max(data$x))
  Y=runif(nrow(data),min = min(data$y),max = max(data$y))
  base=cbind(X,Y)
  model_2[[i]]=kmeans(base,1)
  W_2[[i]]=(model_2[[i]]$tot.withinss)
  
}
S[[k]]=mean(log(unlist(W_2)))



# Filter out any non-numeric or NULL values from W_2

mean(B)
print(mean_W2)


col1 = 1:10
col1
sample(col1)
for (i in 1:5) {
  shuffled_col <- sample(col1)  # Mélange col1
  print(shuffled_col)  # Affiche le résultat pour chaque itération
}

# Initialiser une liste pour stocker les résultats
shuffled_results <- list()
set.seed(123)
for (i in 1:8) {
  
  shuffled_results[[i]] <- sample(col1)
}

# Afficher tous les mélanges
print(shuffled_results)
shuffled_results[1:3]

simul=function(data){
  
  
}


# Exemple de données
 # Nombre de clusters
gap_values <- c(-0.25, -0.15, -0.20, -0.10, -0.18, -0.12, -0.15, -0.08, -0.20)  # Valeurs de Gap
error_values <- c(0.05, 0.08, 0.07, 0.06, 0.08, 0.07, 0.06, 0.05, 0.09)  # Erreurs pour les barres d'erreur

# Tracer le graphique avec la courbe Gap
plot(k, gap_values, type = "o", pch = 16, lty = 1, col = "black", ylim = range(c(gap_values - error_values, gap_values + error_values)),
     xlab = "number of clusters k", ylab = "Gap", main = "Gap with Error Bars")

# Ajouter les barres d'erreur
arrows(k, gap_values - error_values, k, gap_values + error_values, angle = 90, code = 3, length = 0.05, col = "black")



########### Silhouette
# Charger les bibliothèques nécessaires
library(cluster)
library(factoextra)

# 1. Générer des données simulées avec 3 clusters
set.seed(123)  # Fixer la seed pour la reproductibilité

# Simuler des données avec 3 clusters
n <- 1000  # Nombre d'observations
p <- 2    # Nombre de dimensions (2D)

# Générer 3 clusters avec différentes distributions
cluster1 <- matrix(rnorm(n / 3 * p, mean = 3), ncol = p)
cluster2 <- matrix(rnorm(n / 3 * p, mean = 7), ncol = p)
cluster3 <- matrix(rnorm(n / 3 * p, mean = 11), ncol = p)

cluster1 <- data.frame(
  x = rnorm(1000, mean = 8, sd = 0.01), # Ecart-type très faible pour un bloc compact
  y = rnorm(1000, mean = 8, sd = 0.01)
)
cluster1 <- cluster1[cluster1[["x"]] >= 7.98 & cluster1[["x"]] < 8.02, ]

# Combiner les clusters pour créer le jeu de données
data_simulated <- cluster1
plot(data_simulated)


# 3. Calculer la silhouette

silhouette_scores <- c()
for (k in 2:10) {
  kmeans_result <- kmeans(data_simulated, centers = k, nstart = 50)
  sil <- silhouette(kmeans_result$cluster, dist(data_simulated))
  silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))
}

# Trouver le nombre de clusters avec le score de silhouette maximal
optimal_k <- which.max(silhouette_scores) + 1  # +1 car le vecteur commence à 2 clusters

cat("Nombre optimal de clusters :", optimal_k, "\n")
cat("Score de silhouette optimal :", max(silhouette_scores), "\n")

##### La méthode de  Krzanowski et Lai (1985)

library(NbClust)
nb <- NbClust(data = data_simulated, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# Résultat : le nombre optimal de clusters selon la majorité des indices
cat("Nombre optimal de clusters :", nb$Best.nc[1, "Number_clusters"], "\n")

# Afficher les indices pour chaque nombre de clusters testé
print(nb$All.index)

######## Gap

# Appliquer la méthode du Gap Statistique
gap_stat <- clusGap(data_simulated, FUN = kmeans, nstart = 25, K.max = 10, B = 100)

# Afficher les résultats du Gap Statistique
print(gap_stat, method = "Tibs2001SEmax")

# Plot du Gap Statistique pour visualiser le nombre optimal de clusters
plot(gap_stat, main = "Méthode du Gap Statistique pour le nombre optimal de clusters")
# Afficher le nombre optimal de clusters basé sur le premier maximum du Gap Statistique
optimal_clusters <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method = "Tibs2001SEmax")
cat("Nombre optimal de clusters (Gap Statistique):", optimal_clusters, "\n")
help("clusGap")

library(ggplot2)

###############################################

# 3. Calculer la silhouette

silhouette_scores <- c()
for (k in 2:10) {
  kmeans_result <- kmeans(data, centers = k, nstart = 50)
  sil <- silhouette(kmeans_result$cluster, dist(data))
  silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))
}

# Trouver le nombre de clusters avec le score de silhouette maximal
optimal_k <- which.max(silhouette_scores) + 1  # +1 car le vecteur commence à 2 clusters

cat("Nombre optimal de clusters :", optimal_k, "\n")
cat("Score de silhouette optimal :", max(silhouette_scores), "\n")

##### La méthode de  Krzanowski et Lai (1985)
library(fpc)
k_max <- 10
k_values <- 1:k_max
scores <- numeric(k_max)

for (k in k_values) {
  # Effectuer le clustering
  kmeans_result <- kmeans(data_simulated, centers = k, nstart = 25)
  
  # Utiliser la fonction cluster.stats pour obtenir les indices
  cluster_stats <- cluster.stats(dist(data_simulated), kmeans_result$cluster)
  
  # L'indice de Krzanowski et Lai est stocké dans la partie 'ch' de cluster.stats
  scores[k] <- cluster_stats$ch
}

# Tracer les scores pour différents nombres de clusters
plot(k_values, scores, type = "b", col = "blue", xlab = "Nombre de clusters (K)", ylab = "Indice de Krzanowski et Lai", main = "Indice de Krzanowski et Lai pour différents nombres de clusters")


library(NbClust)
nb <- NbClust(data = data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# Résultat : le nombre optimal de clusters selon la majorité des indices
cat("Nombre optimal de clusters :", nb$Best.nc[1, "Number_clusters"], "\n")

# Afficher les indices pour chaque nombre de clusters testé
print(nb$All.index)

######## Gap

# Appliquer la méthode du Gap Statistique

gap_stat <- clusGap(data, FUN = kmeans, nstart = 25, K.max = 10, B = 100)

# Afficher les résultats du Gap Statistique
print(gap_stat, method = "Tibs2001SEmax")

# Plot du Gap Statistique pour visualiser le nombre optimal de clusters
plot(gap_stat, main = "Méthode du Gap Statistique pour le nombre optimal de clusters")

# Afficher le nombre optimal de clusters basé sur le premier maximum du Gap Statistique
optimal_clusters <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method = "Tibs2001SEmax")
cat("Nombre optimal de clusters (Gap Statistique):", optimal_clusters, "\n")
help("clusGap")


