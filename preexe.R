library(epical)
shinydata <- read.table("/home/ins-bio/Documents/nextstrain_peru_1024/results/metadata_shiny.tsv", sep ="\t", header = TRUE)
shinydata <- add_epi_week(shinydata, "date", system = "cdc")
shinydata$Date <- epi_week_date(shinydata$epi_week, shinydata$epi_year,system = "cdc")
write.csv(shinydata, "/home/fascue/Documents/Git/metadata_shiny.csv")
colnames(shinydata)
data <- read.csv("/home/fascue/Documents/Git/metadata_shiny.csv")
str(emetadata)


unique(data$VOC.VOI)
