#Reading the dataset
pacman::p_load(tidyverse,reshape2,readxl,jsonlite,corrplot,XLConnect,magrittr)
data<-as.data.frame(fromJSON(file.path('E:/NUS Stuff/C2-AA/FBA - Day 7/FBA - Day 7/data/titanic','titanic3.json')))
View(data)
#--------------------------------------------------------------------------------------------------
#Task 1 - Examine impact of:
#--------------------------------------------------------------------------------------------------

#Gender of a passenger on survival
data%>%
  group_by(survived,sex)%>%
  summarise(count_level = n(),percentage = n()/nrow(data))%>%
  ggplot(aes(x=as.factor(sex),y=count_level,fill=as.factor(survived))) + geom_bar(stat = "identity",position = position_dodge()) + geom_text(aes(label=round(percentage,2))) + labs(x="Sex",y="Number of passengers", title="Survival Rate", subtitle="by sex") + scale_fill_discrete("Survival",labels = c("Casualty","Survived"))
#There are a higher number of female survivors(26%) as compared to male survivors(12%)
#Also more male passengers died(52%) as compared to female passengers(10%)(percentage on absolute)

#Embarkation point on survival
data%>%
  group_by(embarked,survived)%>%
  summarise(number = n())%>%
  ggplot(aes(x=as.factor(embarked),y=number)) + geom_bar(stat = "identity") + geom_text(aes(label=number)) + facet_grid(.~survived,labeller = labeller(survived=c('0' = 'Casualty','1' = 'Survived'))) + labs(x="Embarkation points")
#There are a higher proportion of survivors in embarkation point C (however the pattern is almost similar in all cases)

#Cabin on survival
#Create a new categorical variable called is_cabin with yes for cabin number and no for no cabin number
data$is_cabin[data$cabin == ''] = 'NO'
data$is_cabin[!data$cabin == ''] = 'YES'
View(data)
data%>%
  group_by(is_cabin,survived)%>%
  summarise(count = n())%>%
  ggplot(aes(x=as.factor(is_cabin),y=count,fill=as.factor(survived))) + geom_bar(stat = "identity",position = position_dodge()) + labs(x="Cabin Alloted?") + scale_fill_discrete("Survival",labels = c("Casualty","Survived"))
#As can be seen, a higher proportion of people who were alloted cabins have survived (compared to relative casualties)

#fare level on survival
data%>%
ggplot(aes(as.factor(survived),fare)) + geom_boxplot() + labs(x="Survival outcome") + scale_x_discrete(breaks=c(0,1),labels=c("Casualty","Survived"))
#The median fare of survivors is seen to be higher than the casualties. Thus the people who
#have paid a higher fare may have had better facilities for evacuation


#################################################################################################
#Checking significance via a full regression model
#################################################################################################
data<-read.csv('E:/NUS Stuff/C2-AA/FBA - Day 7/FBA - Day 7/data/titanic/titanic3.csv')
#Creating is_cabin as a binary factor variable as above
data$is_cabin[data$cabin == ''] = 'NO'
data$is_cabin[!data$cabin == ''] = 'YES'
View(data)
#Convert to factors
data$survived<-as.factor(data$survived)
summary(data)
#Dropping name, ticket, cabin and home.dest due to many unique levels
summary(glm(survived~pclass+survived+sex+age+sibsp+parch+fare+embarked+is_cabin,family = "binomial",data = data))
#As can be seen, pclass, sex, age, sibsp and is_cabin are the significant parameters
#Hence our assumption that fare has an influence on survival has been proved false
#------------------------------------------------------------------------------------------------
#Task 2 - Movie Dataset
#------------------------------------------------------------------------------------------------

data<-read.csv("E:/NUS Stuff/C2-AA/FBA - Day 7/movies.csv")
View(data)

#Who is the best director of the list of below popular directors(as per the critics)--
#(the eternal question:))?
directors<-c("Agnieszka Holland","Christopher Nolan","Clint Eastwood","Danny Boyle","Dennie Gordon","James Cameron","James Ivory","Kathryn Bigelow","Martin Scorsese","Quentin Tarantino","Tony Scott","Steven Spielberg")
View(data[data$director %in% directors,])
data[data$director %in% directors,]%>%
  group_by(director)%>%
  summarise(score=mean(critics_score))%>%
  ggplot(aes(x=reorder(as.factor(director),score),y=score)) + geom_bar(stat = "identity") + coord_flip()
data%>%
  top_n(n=3,wt=critics_score)%>%
  select(title)

#As can be seen James Cameron has on average received better reception from film critics :P


#Which are the top 10 most popular movies(by imdb number of votes)?
?sort
data%>%
  top_n(n=10,wt=imdb_num_votes)%>%
  ggplot(aes(x=reorder(title,imdb_num_votes),y=imdb_num_votes)) + geom_bar(stat = "identity") + coord_flip()
#Most liked movies like Prestige,Titanic,Godfather make an appearance:)

#Which Actor has won the most awards(by actor1 - lead role)?
data[data$best_actor_win == 'yes',]%>%
  group_by(actor1)%>%
  summarise(number_of_wins = n())%>%
  top_n(n=10,wt=number_of_wins)%>%
  ggplot(aes(x=reorder(as.factor(actor1),number_of_wins),y=number_of_wins)) + geom_bar(stat = "identity") + coord_flip()
#Al Pacino all the way


#Which genre of movies is most enjoyed by the audience(by audience_score)?
data%>%
  group_by(genre)%>%
  ggplot(aes(genre,audience_score)) + geom_boxplot() + coord_flip()
#Surprisingly documentaries are most liked by the audience


#Are movie genres predispositioned to mpaa ratings(Eg are most drama movies rated PG)?
data%>%
  group_by(mpaa_rating,genre)%>%
  summarise(number=n())%>%
  ggplot(aes(x=as.factor(genre),y=number,fill=as.factor(mpaa_rating))) + geom_bar(stat = "identity",position = position_dodge()) + coord_flip()
#As it turns out they are...most drama movies are PG,PG-13 or R,similarly for 
#mystery, horror and comedy