source("helpers.R")

packages <- c( # List of this script's dependencies
  "readxl",
  "purrr", # set_names
  "ggplot2", # geom_histograms
  "exactRankTests" # wilcox_test
)
RequirePackages(packages)

df <- read_excel("data/dataset_clean_1.xlsx")



#----------two sample testing --------------------------------------------------------

# making two sub samples, dividing by sex
men <- df[df$B == 1, ]
women <- df[df$B == 2, ]

# normalizing the samples to the same number of subjects
men <- men[1:min(nrow(men), nrow(women)), ]
women <- women[1:min(nrow(men), nrow(women)), ]

# applying the wilcox test on every variable on both men and women samples and checking
# if there is a difference between the results based o the p-value

messages <- unlist(lapply(names(c(map1[1], map1[3:length(map1)])), function(x) {
  p_value <- wilcox.exact(unlist(men[x]), unlist(women[x]), paired = FALSE)$p.value # Wilcox test
  if (p_value >= 0.05) {
    paste("There is no significant difference in the", map1[x], "between men and women")
  } else {
    paste("There is a significant difference in the", map1[x], "between men and women")
  }
}))
messages

T <- names(men)
T <- set_names(T)

# plotting function
twoSamples <- lapply(T, function(x) {
  ggplot(men, aes(x = .data[[x]])) +
    geom_histogram(fill = "blue", alpha = 0.3, binwidth = NULL) +
    geom_histogram(data = women, fill = "red", binwidth = NULL, alpha = 0.3)
})

# generating grid plots and saving them to img folder
do.call(grid.arrange, twoSamples[1:12])
do.call(grid.arrange, twoSamples[13:24])
do.call(grid.arrange, twoSamples[25:36])
do.call(grid.arrange, twoSamples[37:48])
do.call(grid.arrange, twoSamples[49:60])

#----------correlations--------------------------------------------------------
