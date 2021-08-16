

colnames(originalData)[6] <- 'Duration'


view(data_clean)
                              

#Remove index rows: 
# data_clean <- originalData[3:nrow(originalData), ]
data_clean <- originalData %>% 
  mutate(Duration = as.numeric(Duration) / 60)
         
#Leave only actual data columns (removing the default Quadratics irrelevant information):
irrelevant_colnames <- colnames(data_clean)[c(1:5, 8:17, 19:24, 26:29)]
data_clean <- select(data_clean, -irrelevant_colnames)


data_clean1 <- data_clean %>% 
  filter(Finished == TRUE)

#subset to exclude outliers: 
#1. Those that were not willing/ able to participate
data_clean2 <- data_clean1 %>% 
  filter(Alone == 'Alone')

#Those that did not complete the survey consequtively: 
data_clean3 <- data_clean2 %>% 
  filter(Consecutively == 'Yes')

#Examine the disturvances that the participants experienced:
data_clean3 %>% 
  filter(Disturbances != 'No')

data_clean4 <- data_clean3 %>% 
  filter(Disturbances == 'No')

view(data_clean3 %>% 
       filter(Disturbances != 'No'))
view(data_clean)

data_clean5 <- data_clean4 %>% 
  filter(Attention == 'Strongly agree')

# Removing timing outliers
data_clean5 %>% 
  group_by(Sex) %>% 
  skim(Duration)

data_clean5 %>% 
  ggplot(aes(x = Duration)) + 
  geom_histogram() +
  facet_grid(~Sex) + 
  xlab('Time in minutes')

data_clean5 %>% 
  ggplot(aes(y = Duration, x = Sex)) + 
  geom_boxplot() + 
  ylab('Time in minutes')

data_clean6 <- data_clean5 %>% 
  filter(similarExperiment == 'No')

data_clean7 <- data_clean6 %>% 
  mutate(cleanDuration = continuousOutliers(Duration))

data_clean7 %>% 
  filter(!is.na(cleanDuration)) %>% 
  group_by(Sex) %>% 
  skim(cleanDuration)

library(stringr)
temp <- data.frame(str_split_fixed(data_clean7$WasonTest, ",", 6))
i <- 'X1'
u <- 1
allPluse2 <- data.frame(ID = data_clean7$ID)

for (i in colnames(temp)){
  newData <- data.frame(str_split_fixed(unlist(temp[i]), "/", 3))
  pluse2 <- c()
  for(u in 1:nrow(newData)){
    if(as.numeric(newData[u, 2]) - as.numeric(newData[u, 1]) == 2){
      if(as.numeric(newData[u, 3]) - as.numeric(newData[u, 2]) == 2){
        pluse2 <- c(pluse2, 0)
      }else{
        pluse2 <- c(pluse2, 1)
      }
    }else{
      pluse2 <- c(pluse2, 1)
    }
  }
  allPluse2 <- cbind(allPluse2, pluse2)
}

data_clean8 <- cbind(data_clean7,
                     series1 = gsub("/", '_', temp$X1),
                     allPluse2[, 2],
                     series2 = gsub("/", '_', temp$X2),
                     allPluse2[, 3],
                     series3 = gsub("/", '_', temp$X3),
                     allPluse2[, 4],
                     series4 = gsub("/", '_', temp$X4),
                     allPluse2[, 5],
                     series5 = gsub("/", '_', temp$X5),
                     allPluse2[, 6],
                     series6 = gsub("/", '_', temp$X6),
                     allPluse2[, 7])


write.csv(data_clean8, 'cleanData.csv')
