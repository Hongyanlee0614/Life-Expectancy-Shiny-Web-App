library(shiny)
library(tidyverse)
library(ggplot2)
library(shinyjs)
library(scales)
library(dplyr)
library(DT)
library(shinyalert)
library(tidyr)
#load file data
file<-read.csv("Life Expectancy Data.csv",na.strings = "NA")

#Part 1

#Graph of Mean Life Expectancy of 100 Countries
#clean data
#only choose rows which contain non Na values in Life expectancy column
fileCleaned <-file %>% drop_na(Life.expectancy)

#extract data
#produce a data frame consists of number of observations belong to a country
total_year<-fileCleaned %>% select(Year, Country, Life.expectancy) %>% count(Country)
#assign total number of years considered in every country into the variable
total_yearForEachCountry<-total_year$n
#produce a data frame consists of yyear, country and life expectancy of all rows
#which has non NA values in life expectancy column
selected<-fileCleaned %>% select(Year, Country, Life.expectancy)
#group the dataset according to country name, then sum all life expectancy in 16 years for each country
totalLifeExp<-selected %>% group_by(Country) %>% summarise(totalLifeExp=sum(Life.expectancy))
#calculate the average of life expectancy for each country
averageLifeExp <-totalLifeExp %>% mutate(meanLifeExp=totalLifeExp/total_yearForEachCountry)
#subsetting the data frame: choose only column country and the mean life expectancy(discard the total column)
averageLifeExp <- subset(averageLifeExp, select=c("Country","meanLifeExp"))
top100<-head(averageLifeExp,100)

#use stat="identity" when y variable in axis is a column in dataset, in this case: top100
#use stat="bin" when y variable is not a column in dataset
graph1<-ggplot(top100,aes(x=Country,y=meanLifeExp,fill=Country))+geom_histogram(stat="Identity",show.legend = FALSE)+ggtitle("Mean Life Expectancy of Various Countries")
#adjust the label 
plotGraph1<-graph1+theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +ylab("Mean Life Expectancy (years old)")






#Graph of Mean Life Expectancy of Developed and Developing Countries
#find the frequency of the status of the country of 2: developed and developing
group<-fileCleaned %>% count(Status)
#calculate total life expectancy of all countries grouped by status
developed<-fileCleaned %>% group_by(Status) %>% summarise(TotalLifeExp=sum(Life.expectancy))
#calculate the mean of life expectancy of developed and developing countries
mean<-developed%>% mutate(meanLifeExp=TotalLifeExp/group$n)
graph2<-ggplot(mean,aes(x=Status,y=meanLifeExp,fill=Status))+geom_bar(stat="Identity")+ggtitle("Mean Life Expectancy of Various Countries")
#adjust label
plotGraph2<-graph2+theme(axis.text.x = element_text(vjust=0.5))+ylab("Mean Life Expectancy (years old)")






#Graph of Mean Life Expectancy of Malaysia from 2000 to 2015
mas<-fileCleaned %>% subset(Country=="Malaysia")
lifeExpMas<-mas %>% select(Year,Life.expectancy)
plotGraphMas<-ggplot(lifeExpMas,aes(x=Year,y=Life.expectancy,fill=Year))+
  ylab("Mean Life Expectancy (years old)")+scale_x_continuous(breaks=seq(2000,2015,1))+
  scale_y_continuous(breaks=seq(50,100,2))+coord_cartesian(ylim=c(65,76))+
  geom_histogram(stat="Identity")+ggtitle("Mean Life Expectancy of Malaysia from 2000 to 2015")



#Graph of Life Expectancy of Developed vs Developing Countries over time
#group the number ofdeveloped and developing countries according to the year
group3<-fileCleaned %>% group_by(Year,Status) %>% count(Status)
#sum of developed and developing countries over time 
developed3<-fileCleaned %>% group_by(Year,Status) %>% summarise(sumLifeExp=sum(Life.expectancy))
temp<-group3$n[1:2]
#calculate mean life expectancy of developed and developing countries of each year
final<-developed3 %>% mutate(meanLifeExp=sumLifeExp/temp)
plotGraph3<- ggplot(final,aes(x=Year,y=meanLifeExp,color=Status))+geom_line()+ggtitle("Life Expectancy of Developed vs Developing Countries from 2000-2015")




#Part 2


#Graph of mean alcohol consumption of Developed vs Developing Countries from 2000-2015
#drop the row which contains NA in life expectancy or in alcohol column
fileCleaned1<-file %>% drop_na(Alcohol,Life.expectancy)
#group the number ofdeveloped and developing countries according to the year
group5<-fileCleaned1 %>% group_by(Year,Status) %>% count(Status)
#sum of alcohol consumption of developed and developing countries over time 
group4<-fileCleaned1 %>% group_by(Year,Status) %>% summarise(sumAlcohol=sum(Alcohol))
temp1<-group5$n[1:2]
#calculte the mean alcohol consumption of developed and developing countries of each year
alcoholMean<-group4 %>% mutate(meanAlcohol=sumAlcohol/temp1)
plotGraph4<-ggplot(alcoholMean,aes(x=Year,y=meanAlcohol,colour=Status))+geom_line()+
    ylab("Alcohol Consumption (litres of Pure Alcohol)")+
    ggtitle("Mean alcohol consumption of Developed vs Developing Countries from 2000-2015")



#Graph of Alcohol Consumption of Malaysia and Germany within 2000-2014
masAlc<-fileCleaned1 %>% subset(Country=="Malaysia")
ecuAlc<-fileCleaned1 %>% subset(Country=="Germany")
alcMas<-masAlc %>% select(Country,Alcohol)
alcecu<-ecuAlc %>% select(Country,Alcohol)
alcMas<-rbind(alcMas,alcecu)
plotGraphAlcMas<-ggplot(alcMas,aes(x=Country,y=Alcohol))+ylab("Alcohol Consumption (litres of Pure Alcohol)")+
  geom_boxplot()+ggtitle("Mean Life Expectancy of Malaysia and Germany from 2000 to 2014")





#Graph of Life Expectancy vs Alcohol Consumption in Germany and Malaysia from 2000 to 2014
alcEcuMas<-fileCleaned1 %>% select(Year, Country,Life.expectancy, Alcohol)
alcMas<-alcEcuMas[(alcEcuMas$Country=="Malaysia") ,]
alcEcu<-alcEcuMas[(alcEcuMas$Country=="Germany"),]
alcEcu<-rbind(alcEcu,alcMas)
plotGraphAlc<-ggplot(alcEcu,aes(x=Alcohol,y=Life.expectancy,col=Country))+geom_text(label=alcEcu$Year)+
  xlab("Alcohol Consumption (litres of Pure Alcohol)")+ylab("Life Expectancy (years old)")+ggtitle("Life Expectancy vs Alcohol Consumption in Germany and Malaysia from 2000 to 2014")






#Part 3

#Graph of Mean Life Expectancy of Country vs Mean Total Expenditure

fileCleaned2 <-file %>% drop_na(Life.expectancy,Total.expenditure)
#produce a data frame consists of number of observations belong to a country
total_year<-fileCleaned2 %>% select(Year, Country, Life.expectancy) %>% count(Country)
#assign total number of years considered in every country into the variable
total_yearForEachCountry<-total_year$n
#produce a data frame consists of yyear, country and life expectancy of all rows
#which has non NA values in life expectancy column
selected<-fileCleaned2 %>% select(Year, Country, Life.expectancy)
#group the dataset according to country name, then sum all life expectancy in 16 years for each country
totalLifeExp<-selected %>% group_by(Country) %>% summarise(totalLifeExp=sum(Life.expectancy))
#calculate the average of life expectancy for each country
averageLifeExp <-totalLifeExp %>% mutate(meanLifeExp=totalLifeExp/total_yearForEachCountry)
#subsetting the data frame: choose only column country and the mean life expectancy(discard the total column)
averageLifeExp <- subset(averageLifeExp, select=c("Country","meanLifeExp"))
lifeExp1<-averageLifeExp$meanLifeExp  #from line25


#produce a data frame consists of number of observations belong to a country
total_year1<-fileCleaned2 %>% select(Year, Country, Life.expectancy) %>% count(Country)
#assign total number of years considered in every country into the variable
total_yearForEachCountry1<-total_year1$n
#produce a data frame consists of yyear, country and total expenditure of all rows
#which has non NA values in life expectancy column
selected1<-fileCleaned2 %>% select(Year, Country, Total.expenditure)
#group the dataset according to country name, then sum all total expenditure in 16 years for each country
totalExp<-selected1 %>% group_by(Country) %>% summarise(totalExpenditure=sum(Total.expenditure))
#calculate the average of total expenditure for each country
averageExp <-totalExp %>% mutate(meanExp=totalExpenditure/total_yearForEachCountry1)
#subsetting the data frame: choose only column country and the mean total expenditure(discard the total column)
averageExp <- subset(averageExp, select=c("Country","meanExp"))
exp<-averageExp$meanExp
plotGraph5<-ggplot(averageExp,aes(x=exp,y=lifeExp1))+xlab("Mean Percentage Government Expenditure on Health (%)")+
    ylab("Mean Life Expectancy of Country")+geom_point()+
    ggtitle("Mean Life Expectancy of Country vs Mean Percentage Government Expenditure on Health")



#Graph of Mean Total Expenditure of People in Malaysia and Germany from 2000 to 2014
masExp<-fileCleaned2 %>% subset(Country=="Malaysia")
ecuExp<-fileCleaned2 %>% subset(Country=="Germany")
expMas<-masExp %>% select(Country,Total.expenditure)
expEcu<-ecuExp %>% select(Country,Total.expenditure)
expMas<-rbind(expMas,expEcu)
plotGraphexpMas<-ggplot(expMas,aes(x=Country,y=Total.expenditure))+ylab("Total Expenditure (%)")+
  geom_boxplot()+ggtitle("Mean Percentage Government Expenditure on Health in Malaysia and Germany from 2000 to 2014")





#Graph of Life Expectancy vs Percentage Government Expenditure on Health in Germany and Malaysia from 2000 to 2014
expEcuMas<-fileCleaned2 %>% select(Year, Country,Life.expectancy, Total.expenditure)
expMas<-expEcuMas[(expEcuMas$Country=="Malaysia") ,]
expEcu<-expEcuMas[(expEcuMas$Country=="Germany"),]
expEcu<-rbind(expEcu,expMas)
plotGraphExp<-ggplot(expEcu,aes(x=Total.expenditure,y=Life.expectancy,col=Country))+geom_text(label=expEcu$Year)+
  xlab("Percentage Government Expenditure on Health (%)")+ylab("Life Expectancy (years old)")+ggtitle("Life Expectancy vs Percentage Government Expenditure on Health in Germany and Malaysia from 2000 to 2014")






#Part 4


#Graph of GDP for every country
fileCleaned3 <-file %>% drop_na(GDP)

#extract data
#produce a data frame consists of number of observations belong to a country
total_year2<-fileCleaned3 %>% select(Year, Country, GDP) %>% count(Country)
#assign total number of years considered in every country into the variable
total_yearForEachCountry2<-total_year2$n
#produce a data frame consists of yyear, country and GDP of all rows
#which has non NA values in GDP column
selected2<-fileCleaned3 %>% select(Year, Country, GDP)
#group the dataset according to country name, then sum all GDP in 16 years for each country
totalGDP<-selected2 %>% group_by(Country) %>% summarise(totalGDP=sum(GDP))
#calculate the average of GDP for each country
averageGDP <-totalGDP %>% mutate(meanGDP=totalGDP/total_yearForEachCountry2)
#subsetting the data frame: choose only column country and the mean GDP(discard the total column)
averageGDP <- subset(averageGDP, select=c("Country","meanGDP"))
graph6<-ggplot(averageGDP,aes(x=Country,y=meanGDP,fill=Country))+geom_histogram(stat="Identity",show.legend = FALSE)+ggtitle("Mean GDP of Various Countries")
#adjust the label 
plotGraph6<-graph6+theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))






#Graph of Mean Life Expectancy vs Mean GDP
#only choose rows which contain non Na values in Life expectancy column
fileCleaned7 <-file %>% drop_na(Life.expectancy,GDP)
#produce a data frame consists of number of observations belong to a country
total_year<-fileCleaned7 %>% select(Year, Country, Life.expectancy) %>% count(Country)
#assign total number of years considered in every country into the variable
total_yearForEachCountry<-total_year$n
#produce a data frame consists of yyear, country and life expectancy of all rows
#which has non NA values in life expectancy column
selected<-fileCleaned7 %>% select(Year, Country, Life.expectancy)
#group the dataset according to country name, then sum all life expectancy in 16 years for each country
totalLifeExp<-selected %>% group_by(Country) %>% summarise(totalLifeExp=sum(Life.expectancy))
#calculate the average of life expectancy for each country
averageLifeExp <-totalLifeExp %>% mutate(meanLifeExp=totalLifeExp/total_yearForEachCountry)
#subsetting the data frame: choose only column country and the mean life expectancy(discard the total column)
averageLifeExp <- subset(averageLifeExp, select=c("Country","meanLifeExp"))
lifeExp<-averageLifeExp$meanLifeExp  #from line25

#extract data
#produce a data frame consists of number of observations belong to a country
total_year2<-fileCleaned7 %>% select(Year, Country, GDP) %>% count(Country)
#assign total number of years considered in every country into the variable
total_yearForEachCountry2<-total_year2$n
#produce a data frame consists of yyear, country and GDP of all rows
#which has non NA values in GDP column
selected2<-fileCleaned7 %>% select(Year, Country, GDP)
#group the dataset according to country name, then sum all GDP in 16 years for each country
totalGDP<-selected2 %>% group_by(Country) %>% summarise(totalGDP=sum(GDP))
#calculate the average of GDP for each country
averageGDP <-totalGDP %>% mutate(meanGDP=totalGDP/total_yearForEachCountry2)
#subsetting the data frame: choose only column country and the mean GDP(discard the total column)
averageGDP <- subset(averageGDP, select=c("Country","meanGDP"))
GDP<-averageGDP$meanGDP

plotGraph7<-ggplot(totalGDP,aes(x=GDP,y=lifeExp))+xlab("Mean GDP")+
    ylab("Mean Life Expectancy of Country")+geom_point()+
    ggtitle("Mean Life Expectancy of Country vs Mean GDP")




#Graph of GDP of Malaysia and Germany from 2000 to 2015
gdp<-fileCleaned7 %>% select(Year,Country,GDP) 
gdp<-gdp[(gdp$Country=="Malaysia") | (gdp$Country=="Germany"),]
plotGraphGdpMas<-ggplot(gdp,aes(x=Country,y=GDP,fill=Country))+geom_boxplot()+ylab("GDP (USD)")+
  ggtitle("GDP of Malaysia and Germany from 2000 to 2015")



#Graph of Live Expectancy vs GDP in Germany and Malaysia from 2000 to 2015
gdpEcuMas<-fileCleaned7 %>% select(Year, Country,Life.expectancy, GDP)
gdpMas<-gdpEcuMas[(gdpEcuMas$Country=="Malaysia") ,]
gdpEcu<-gdpEcuMas[(gdpEcuMas$Country=="Germany"),]
gdpEcu<-rbind(gdpEcu,gdpMas)
plotGraphGDP<-ggplot(gdpEcu,aes(x=GDP,y=Life.expectancy,col=Country))+geom_text(label=gdpEcu$Year)+
  xlab("GDP (USD)")+ylab("Life Expectancy (years old)")+ggtitle("Life Expectancy vs GDP in Germany and Malaysia from 2000 to 2015")



#Part 5


#Graph of Population of Various Countries 
fileCleaned8 <-file %>% drop_na(Population)

#extract data
total_year<-fileCleaned8 %>% select(Year, Country, Population) %>% count(Country)
total_yearForEachCountry<-total_year$n
selected<-fileCleaned8 %>% select(Year, Country, Population)
totalPop<-selected %>% group_by(Country) %>% summarise(totalPop=sum(Population))
averagePop <-totalPop %>% mutate(meanPop=totalPop/total_yearForEachCountry)
averagePop <- subset(averagePop, select=c("Country","meanPop"),meanPop>=10000000)
graph8<-ggplot(averagePop,aes(x=Country,y=meanPop,fill=Country))+geom_histogram(stat="Identity",show.legend = FALSE)+ggtitle("Population of Different Countries")
plotGraph8<-graph8+theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))+
  ylab("Mean Population (Million)")+scale_y_continuous(breaks = seq(0, 4.5e+08, 5e+07),labels=unit_format(unit="",scale=1e-06))






fileCleaned88 <-file %>% drop_na(Life.expectancy,Population)
popEcuMas<-fileCleaned88 %>% select(Year, Country, Life.expectancy, Population) 
popMas<-popEcuMas[(popEcuMas$Country=="Malaysia") ,]
popEcu<-popEcuMas[(popEcuMas$Country=="Germany"),]
#fill in missing 0
popMas$Population[popMas$Population==3723155]<-30723155
popMas$Population[popMas$Population==322817]<-30228170
popMas$Population[popMas$Population==2976724]<-29767240
popMas$Population[popMas$Population==2917456]<-29174560
popMas$Population[popMas$Population==2765383]<-27653830
popMas$Population[popMas$Population==2711169]<-27111690
popMas$Population[popMas$Population==2517419]<-25174190
popMas$Population[popMas$Population==2468873]<-24688730
popMas$Population[popMas$Population==2369897]<-23698970
popMas$Population[popMas$Population==2318568]<-23185680

popEcu$Population[popEcu$Population==89825]<-89825000
popEcu$Population[popEcu$Population==864565]<-86456500
popEcu$Population[popEcu$Population==8425823]<-84258230
popEcu$Population[popEcu$Population==8274983]<-82749830
popEcu$Population[popEcu$Population==8177693]<-81776930
popEcu$Population[popEcu$Population==819237]<-81923700
popEcu$Population[popEcu$Population==821197]<-82119700
popEcu$Population[popEcu$Population==8251626]<-82516260
popEcu$Population[popEcu$Population==8221158]<-82211580

#Graph of Population in Malaysia from 2000 to 2015
options(scipen=1)
plotGraphMasPop<-ggplot(popMas,aes(x=Year,y=(Population)))+geom_histogram(fill="lightblue",stat="Identity")+
  scale_x_continuous(breaks=popMas$Year)+ylab("Population (Million)")+
  ggtitle("Population in Malaysia from 2000 to 2015")+scale_y_continuous(breaks = seq(0, 4e+07, 5e+06),labels=unit_format(unit="",scale=1e-06))+
  theme(legend.position = "none")



#Graph of Life Expectancy vs Mean Population in Germany and Malaysia from 2000 to 2015
popEcu<-rbind(popEcu,popMas)
plotGraphPop<-ggplot(popEcu,aes(x=Population,y=Life.expectancy,col=Country))+geom_text(label=popEcu$Year)+
  xlab("Population (Million)")+ylab("Life Expectancy (years old)")+ggtitle("Life Expectancy vs Population in Germany and Malaysia from 2000 to 2015")+
  scale_x_continuous(breaks = seq(0, 9e+07, 1e+07),labels=unit_format(unit="",scale=1e-06))





# Part 6: Correlation
#cors <- cor(subset(fileCleaned, select = c(Life.expectancy,Alcohol,Total.expenditure,GDP,Population)),use="complete.obs") %>%
#  datatable() %>% formatRound(columns=c("Life.expectancy","Alcohol","Total.expenditure","GDP","Population"), digits=3)
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
filenona<-file %>% drop_na() %>% select(Life.expectancy,Adult.Mortality,infant.deaths,Alcohol,percentage.expenditure,Hepatitis.B,Measles,BMI,under.five.deaths,Polio,Total.expenditure,Diphtheria,HIV.AIDS,GDP,Population,thinness..1.19.years,thinness.5.9.years,Income.composition.of.resources,Schooling)
data_num <- filenona %>% 
  select_if(is.numeric)

cor<-ggcorr(data_num, 
       label = T, 
       label_size = 2,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "royalblue",
       layout.exp = 5,
       low = "green3", 
       mid = "gray95", 
       high = "darkorange",
       name = "Correlation")+labs(caption = "0: zero correlation\n>0: positive correlation (increase in one variable will cause an increase in the other)\n<0: negative correlation (increase in one variable will cause an decrease in the other)")



#Part 7: Multivariable Linear Regression
clean<-file %>% drop_na() 
require(dplyr)
#clean data where we treat developing as 0 and developed as 1 to ease linear modelling
clean <- clean %>%
  mutate(clean_status = ifelse(Status == "Developing",0,1))
linear_model<-lm(Life.expectancy~.-Country-Year-Status,data=clean)
summary(linear_model)
#p value<2.2e-16 means there is at least one variable that is highly significant to life expectancy
#to see which predictor variables are significant (Significant test)
coefficient<-summary(linear_model)$coefficient
#ignore those variables in our model where p value>0.05(not significant)
linear_model<-lm(Life.expectancy~Adult.Mortality+infant.deaths+Alcohol+percentage.expenditure+BMI+under.five.deaths+Total.expenditure+Diphtheria+HIV.AIDS+Income.composition.of.resources+Schooling+clean_status,data=clean)
#our model equation can be written as 

#Error measure of prediction
error<-sigma(linear_model)/mean(clean$Life.expectancy) # In our multiple regression example, the RSE is 0.0518 corresponding to 5% error rate.
#fitted(linear_model) # predicted values
error_from_actual<-influence(linear_model) # regression diagnostics, give the error from actual life expectancy
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot_regression_lineplot<-plot(linear_model) 

#Prediction
#52.94+0.0004285*71.27962+0.0146054*65-0.016848*263+0.0381165*19.1-0.4385139*0.1+0.0900533*62+4.79-0.0687366*83-0.0774547*0.01+0.0807168*8.16+0.8752576*10.1
#Output: 64.33919
predict<-predict(linear_model,data.frame(Adult.Mortality=263,infant.deaths=62,Alcohol=0.01,percentage.expenditure=71.27962,BMI=19.1,under.five.deaths=83,
                                         Total.expenditure=8.16,Diphtheria=65,HIV.AIDS=0.1,Income.composition.of.resources=0.479,Schooling=10.1,clean_status=0),
                 interval = "prediction",level=0.95) 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Life Expectancy"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          useShinyalert(),
          useShinyjs(),
          tabsetPanel(type = "tabs",
                      tabPanel("Visualization",
                               div(
                                 id = "form",
                                 selectInput("gervsmas","Germany(Developed Country) vs Malaysia(Developing Country)",choices=c("Please Select","Life Expectancy vs Alcohol Consumption in Germany and Malaysia from 2000 to 2014","Life Expectancy vs Percentage Government Expenditure on Health in Germany and Malaysia from 2000 to 2014","Live Expectancy vs GDP in Germany and Malaysia from 2000 to 2015","Life Expectancy vs Population in Germany and Malaysia from 2000 to 2015")),
                                 selectInput("lifeexpectancy", "Life expectancy", choices=c("Please Select",c("Mean Life Expectancy of 100 Countries","Mean Life Expectancy of Developed and Developing Countries","Mean Life Expectancy of Malaysia from 2000 to 2015"))),
                                 selectInput("alcohol", "Alcohol Consumption ", choices=c("Please Select",c("Mean Alcohol Consumption of Developed and Developing Countries","Alcohol Consumption in Malaysia and Germany from 2000 to 2014"))),
                                 selectInput("totalexpenditure", "Total Expenditure ", choices=c("Please Select",c("Mean Life Expectancy of Country vs Mean Percentage Government Expenditure on Health","Mean Percentage Government Expenditure on Health in Malaysia and Germany from 2000 to 2014"))),
                                 selectInput("gdp", "GDP ", choices=c("Please Select",c("Mean Life Expectancy of Country vs Mean GDP","Mean GDP of Various Countries","GDP of Malaysia and Germany from 2000 to 2015"))),
                                 selectInput("population", "Population ", choices=c("Please Select",c("Population of Various Countries","Population in Malaysia from 2000 to 2015"))),
                                 checkboxInput("hide_show", "Show Correlation")
                                 ),
                               
                               actionButton("reset_all", "Reset Graph"),
                               
                               ),
                      
                      tabPanel("Predictions", textOutput("prd"),
                               sliderInput("adult_mortality","Adult Mortality (Probability of dying between 15 and 60 years per 1000 population)",0,1000,500),
                               sliderInput("infant_deaths","Infant deaths per 1000 population",0,500,250),
                               sliderInput("alcohol2","Alcohol consumption (in litres of pure alcohol)",0,50,25),
                               sliderInput("percentage_expenditure","Expenditure by government on health per Gross Domestic Product per capita(in %)",0,20000,100),
                               sliderInput("BMI","BMI",0,90,25),
                               sliderInput("under_five_deaths","Number of under-five deaths per 1000 population",0,900,50),
                               sliderInput("total_expenditure","General government expenditure on health per total government expenditure (in %)",0,100,20),
                               sliderInput("diphtheria","Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds (in %)",0,100,50),
                               sliderInput("HIV_AIDS","Deaths per 1 000 live births HIV/AIDS (0-4 years)",0,80,10),
                               sliderInput("income","Human Development Index in terms of income composition of resources",0,1,0.5),
                               sliderInput("schooling","Number of years of Schooling(in years)",0,25,10),
                               selectInput("clean_status","Status of country",choices=c("Please Select","Developed","Developing")),
                               
                      )
                      
          )
          
          
          
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          tabsetPanel(type = "tabs",
                      tabPanel("Plot", plotOutput("plot2")),
                      tabPanel("Predictions",
                      actionButton("calc","Calculate Life Expectancy")
                               )
          ),
            #DT::dataTableOutput("cor"),
          h3(textOutput("caption", container = span))
        )
   )

)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    output$plot2<-renderPlot(
      if(input$gervsmas=="Life Expectancy vs Alcohol Consumption in Germany and Malaysia from 2000 to 2014"){
        
        plotGraphAlc
      }else if(input$gervsmas=="Life Expectancy vs Percentage Government Expenditure on Health in Germany and Malaysia from 2000 to 2014"){
        plotGraphExp
      }else if(input$gervsmas=="Live Expectancy vs GDP in Germany and Malaysia from 2000 to 2015"){
        plotGraphGDP
      }else if(input$gervsmas=="Life Expectancy vs Population in Germany and Malaysia from 2000 to 2015"){
        plotGraphPop
      }else if(input$lifeexpectancy=="Mean Life Expectancy of 100 Countries"){
        plotGraph1
      }else if(input$lifeexpectancy == "Mean Life Expectancy of Developed and Developing Countries"){
        plotGraph2   
      }else if(input$lifeexpectancy=="Mean Life Expectancy of Malaysia from 2000 to 2015"){
        plotGraphMas
      }else if(input$alcohol=="Mean Alcohol Consumption of Developed and Developing Countries"){
        plotGraph4
      }else if(input$alcohol=="Alcohol Consumption in Malaysia and Germany from 2000 to 2014"){
        plotGraphAlcMas
      }else if(input$totalexpenditure=="Mean Life Expectancy of Country vs Mean Percentage Government Expenditure on Health"){
        plotGraph5
      }else if(input$totalexpenditure=="Mean Percentage Government Expenditure on Health in Malaysia and Germany from 2000 to 2014"){
        plotGraphexpMas
      }else if(input$gdp=="Mean Life Expectancy of Country vs Mean GDP"){
        plotGraph7
      }else if(input$gdp=="Mean GDP of Various Countries"){
        plotGraph6
      }else if(input$gdp=="GDP of Malaysia and Germany from 2000 to 2015"){
        plotGraphGdpMas
      }else if(input$population=="Population of Various Countries"){
        plotGraph8
      }else if(input$population=="Population in Malaysia from 2000 to 2015"){
        plotGraphMasPop
      }else if(input$hide_show){
        cor
      }
      
    
  )
        observeEvent(input$calc,{ 
        if(input$clean_status=="Developed"){
          shinyalert("The life expectancy (years old) is ",predict(linear_model,data.frame(Adult.Mortality=input$adult_mortality,infant.deaths=input$infant_deaths,Alcohol=input$alcohol2,percentage.expenditure=input$percentage_expenditure,BMI=input$BMI,under.five.deaths=input$under_five_deaths,
                                                                                           Total.expenditure=input$total_expenditure,Diphtheria=input$diphtheria,HIV.AIDS=input$HIV_AIDS,Income.composition.of.resources=input$income,Schooling=input$schooling,clean_status=1),
                                                                   interval = "prediction",level=0.95) [1],type="info")
        }
        else if (input$clean_status=="Developing"){
          shinyalert("The life expectancy (years old) is ",predict(linear_model,data.frame(Adult.Mortality=input$adult_mortality,infant.deaths=input$infant_deaths,Alcohol=input$alcohol2,percentage.expenditure=input$percentage_expenditure,BMI=input$BMI,under.five.deaths=input$under_five_deaths,
                                                                                           Total.expenditure=input$total_expenditure,Diphtheria=input$diphtheria,HIV.AIDS=input$HIV_AIDS,Income.composition.of.resources=input$income,Schooling=input$schooling,clean_status=0),
                                                                   interval = "prediction",level=0.95) [1],type="info")
        }
        })
      
    
    observeEvent(input$reset_all, {
      reset("form")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
