library(datapasta)
library(extrafont)
library(tidyverse)
library(factoextra)
library(FactoMineR)

data <- data.frame(
  stringsAsFactors = FALSE,
                 selección = c("Spain",
                               "England","Italy","Switzerland","Belgium","Ukraine",
                               "Czech Republic","Denmark"),
  passing_accuracy = c(89.4, 87.6, 88.4, 84.6, 87, 86.6, 77.4, 82.4),
          passes_attempted = c(4307L,2630L,
                               3028L,2515L,2949L,2749L,2131L,2513L),
           pases_completed = c(3856L,2314L,
                               2674L,2146L,2575L,2385L,1651L,2084L),
         possesion = c(67.2, 53.4, 55.8, 48.6, 52.6, 48.6, 47.6, 54.2),
       total_shots = c(95L, 37L, 101L, 69L, 48L, 52L, 57L, 90L),
   shots_on_target = c(39L, 15L, 26L, 22L, 19L, 20L, 19L, 37L),
   shots_of_targed = c(36L, 17L, 48L, 31L, 17L, 19L, 24L, 28L)
        )

rownames(data) <- data$selección
data$selección <- NULL



data<-scale(data)
View(data)
PCA(data, scale.unit = TRUE, graph = TRUE)

res.pca <- PCA(data, graph = FALSE)
print(res.pca)


eigenvalues <- res.pca$eig
head(eigenvalues)

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")

lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")


fviz_screeplot(res.pca, ncp=10)


head(res.pca$var$coord)
head(res.pca$var$cos2)
importancia<-as.data.frame(res.pca$var$cos2)


plot(res.pca, choix = "var")

fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="steelblue")+
  theme_minimal()

fviz_pca_var(res.pca, col.var="contrib")

fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="green", 
                        high="red")+theme_bw()



show(res.pca$ind$coord)
show(res.pca$ind$cos2)
show(res.pca$ind$contrib)





set.seed(65647457)
fviz_pca_biplot(res.pca, col.var = "red", col.ind = "green", geom = c("point","text"), repel = T, addEllipses = F, 
                title = "Biplot de laterales izquierdo que están sin equipo y variables defensivas", 
                subtitle = "Duelos Aéreos, Intercepciones, Despejes, Entradas",
                caption = "Fuente: Whoscored.com | @likedato") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkgreen")) +
  theme(plot.subtitle = element_text(hjust = 0.5, color = "grey10"))

theme_unique_dark <- function (base_size = 12, base_family = "") {
  ret <- (theme_bw(base_size = base_size, base_family = base_family) +
            theme(text = element_text(colour = "white"),
                  title = element_text(color = "darkgreen", hjust = 0.5),
                  line = element_line(color = "white"),
                  rect = element_rect(fill = "black", color = "white"),
                  axis.ticks = element_line(color = "#969696"),
                  axis.title = element_text(color = "white"),
                  axis.text = element_text(color = "#eaeaea"),
                  axis.line = element_line(color = "#969696", linetype = 1),
                  legend.background = element_rect(fill = NULL, color = NULL),
                  legend.position = "bottom",
                  legend.key = element_rect(fill = NA, color = NA, linetype = 0),
                  strip.background = element_rect(fill=NA,colour=NA,size=NA,linetype = NULL),
                  strip.text = element_text(color="white",face="italic",vjust=.5,hjust=.5),
                  panel.background = element_rect(fill = "deepskyblue", color = NULL),
                  panel.border = element_blank(),
                  panel.grid = element_line(color = "#101010"),
                  panel.grid.major = element_line(color = "#101010"),
                  panel.grid.minor = element_line(color = "#101010"),
                  plot.background = element_rect(fill = "deepskyblue", colour = "deepskyblue", linetype = 0)))
  ret
}
set.seed(4354)
p <- fviz_pca_biplot(res.pca, col.var = "red2", col.ind = "black", geom = c("point","text"), repel = T, addEllipses = F, 
                     title = "Biplot of distribution and offensive actions of the 8 best teams in Euro 2020", 
                     subtitle = "Spain is the owner of the game",
                     caption = "Source: uefa.com | @dataR_amateur") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkgreen")) +
  theme(plot.subtitle = element_text(hjust = 0.5, color = "darkgreen")) +
  theme_unique_dark() 
p

p+ggsave("biploteuro.png", width = 13, height = 8.5, dpi = 500)
