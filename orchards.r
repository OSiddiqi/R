max(OrchardSprays$decrease)
max_decrease <- OrchardSprays$treatment[which.max(OrchardSprays$decrease)]
boxplot(OrchardSprays$decrease ~ OrchardSprays$treatment, xlab = "Treatment", ylab = "Decrease", main = "Decrease by Treatment")