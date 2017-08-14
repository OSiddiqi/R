boxplot(Sepal.Width ~ Species, data=iris)
plot(iris$Petal.Length, pch=c(21, 22, 24), bg=c("red", "green3", "blue"), iris$Petal.Width, xlab = "Petal Length", ylab = "Petal Width", main = "Petal Width and Length")
legend("top left", legend=levels(iris$Species), col=brewer.pal(3, "Set2"), pch=1)