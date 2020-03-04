# load the packages
library(Pmisc)
library(tidyverse)

# load datafile
File = "smokeDownload.RData"
load(File)

# what does the data look like:
smoke %>% 
  select(Age, Sex, Grade, RuralUrban, Race, Tried_cigarette_smkg_even) %>% 
  glimpse()

# what does the data look like:
smoke[1:5,c('Age','Sex','Grade','RuralUrban','Race', 'Tried_cigarette_smkg_even')]

# Check the data format of the variable that to be predicted:
smokeFormats[smokeFormats$colName == 'Tried_cigarette_smkg_even', ]

# Change value 1 to yes, 2 to no:
smoke$ifsmoked = factor(smoke$Tried_cigarette_smkg_even, levels = 1:2, labels = c('Y','N'))

# make two way tables (for age and grade):
xtabs(~smoke$Age+smoke$Grade)

# make two way tables (for age and grade):
xtabs(~smoke$everSmoke + smoke$Race)

# make a sub dataset (cleaned)
smoke2 <- smoke %>% 
  # drop missings:
  filter(!is.na(Race),
         !is.na(everSmoke),
         !is.na(Age),
         !is.na(Grade), 
         # grade 8 means ungraded or other grade, should be dropped:
         Grade != 8,
         # since age 9, 10 barely have no student smoking:
         !(Age %in% c(9,10))) %>%
  # make grade "1" to "6":
  mutate(Grade = Grade + 5)

# table
xtabs(~smoke2$Grade + smokeSub$Age)

# reshape the data (part 1)
smoke3 <- smoke2 %>% 
  select(Age, Sex, Race, RuralUrban, everSmoke) %>% 
  group_by_all() %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = everSmoke, values_from = n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes)) %>% 
  na.omit()

# reshape the data (part 2)
smoke3 = reshape2::dcast(smoke2,
                         Age + Sex + Race + RuralUrban ~ everSmoke,
                         length)
dim(smoke3)

smoke3 = na.omit(smoke3)
dim(smoke3)

# make 2 new cols naming total and proportion
smoke3$total <- smoke3$yes + smoke3$no
smoke3$prop <- smoke3$yes/smoke3$total

# ggplot
smoke3 %>% 
  ggplot(aes(x = Age, y = prop, color = Race, shape = Sex)) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(1, 2)) +T
scale_color_brewer(palette = "Set2") +
  theme_minimal()


smoke3$y <- cbind(smoke3$yes, smoke3$no)

head(smoke3)

# fit binary model
smokeFit <- glm(y ~ Race + Sex + Age + RuralUrban, family = binomial(link = "logit"), data = smokeAgg)

summary(smokeFit)

# head 10 lines
head(model.matrix(smokeFit), n=20)

# make 0 year old the reference group in new variable ageC
smoke3$ageC = smoke3$Age - 15
smokeGlm3 = glm(y ~ ageC + Sex + Race + RuralUrban, 
                family=binomial(link='logit'), data = smoke3)
summary(smokeFit2)$coef

# baseline table
smokeOddsRatio = exp(smokeTable[,c('Estimate','lower','upper')])
rownames(smokeOddsRatio)[1] = 'baseline prob'
smokeOddsRatio[1,] = smokeOddsRatio[1,]/(1+smokeOddsRatio[,1])
smokeOddsRatio