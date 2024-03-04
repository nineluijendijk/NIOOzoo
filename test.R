library(ggplot2)

df <- expand.grid(name = c("hyalina","copepod","bosmina"),
                  age = c("juvenileS","juvenileL","adult"),
                  type = c("tapwater", "pondwater"))
df$count <- sample(5:200, size = nrow(df), replace = T)

ggplot(df, aes(x = as.numeric(interaction(name,type)), y = count, fill = name)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=c(2, 5),labels=c("tapwater", "pondwater"))+
  scale_color_manual(values = c("purple","gold","firebrick")) +
  guides(color = guide_legend(override.aes = list(fill = "white")))

ggplot(df, aes(x = as.numeric(interaction(name,type)), y = count, fill = name, color=age)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=c(2, 5),labels=c("tapwater", "pondwater")) +
  scale_color_manual(values = c("purple","gold","firebrick")) +
  guides(color = guide_legend(override.aes = list(fill = "white")))

