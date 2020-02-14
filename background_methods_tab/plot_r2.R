library(plotly)
library(dplyr)
#load the data
r2_mae_internal=read.csv("data/r2_mae_internal.csv")
admin_names=read.csv("data/admin_names_final.csv")

# add countries name
r2_mae_internal=r2_mae_internal%>%
  rename("ISO"="countries")%>%
  select(-X)%>%
  mutate(Sex=ifelse(Sex=="F","Female model","Male model"))

countries_names=admin_names%>%
  distinct(COUNTRY_NAME,ISO,CONT)

data2plot=r2_mae_internal%>%
  left_join(countries_names,
            by="ISO")%>%
  mutate(CONT=as.character(CONT),
         CONT=ifelse(CONT=="AFR","Africa",CONT),
         CONT=ifelse(CONT=="ASI","Asia and Oceania",CONT),
         CONT=ifelse(CONT=="LAC","Latin America and Caribbean",CONT))

#summary
data2plot%>%
  group_by(Sex)%>%
  summarise(mean_R2=mean(R2))

# plot parameters
m <- list( # plot margins
  l = 50,
  r = 100,
  b = 200,
  t = 100,
  pad = 4
)

# plot r2
plot_ly(data=data2plot%>%
          filter(Sex=="Female model")%>%
          arrange(CONT,COUNTRY_NAME)%>%
          mutate(COUNTRY_NAME_f=factor(1:40,
                                       labels=COUNTRY_NAME)),
        x=~COUNTRY_NAME_f,
        y=~R2,
        color=~CONT,
        type="scatter",
        mode="markers")%>%
  layout(title="Female migration model",
         xaxis=list(title="",tickangle=45),
         yaxis=list(title="R-squared",tickformat = "%"),
         margin=m,
         legend = list(orientation = "h",   
                       x = 0.5,
                       y=-0.8))

plot_ly(data=data2plot%>%
          filter(Sex=="Male model")%>%
          arrange(CONT,COUNTRY_NAME)%>%
          mutate(COUNTRY_NAME_f=factor(1:40,
                                       labels=COUNTRY_NAME)),
        x=~COUNTRY_NAME_f,
        y=~R2,
        color=~CONT,
        type="scatter",
        mode="markers")%>%
  layout(title="Male migration model",
         xaxis=list(title="",tickangle=45),
         yaxis=list(title="R-squared",tickformat = "%"),
         margin=m,
         legend = list(orientation = "h",   
                       x = 0.5,
                      y=-0.8))



# plot MAE
plot_ly(data=data2plot%>%
          filter(Sex=="Female model")%>%
          arrange(CONT,COUNTRY_NAME)%>%
          mutate(COUNTRY_NAME_f=factor(1:40,
                                       labels=COUNTRY_NAME)),
        x=~COUNTRY_NAME_f,
        y=~MeanAbsoluteError,
        color=~CONT,
        type="scatter",
        mode="markers")%>%
  layout(title="Female migration model",
         xaxis=list(title="",tickangle=45),
         yaxis=list(title="Mean Absolute Error"),
         margin=m,
         legend = list(orientation = "h",   
                       x = 0.5,
                       y=-0.8))

plot_ly(data=data2plot%>%
          filter(Sex=="Male model")%>%
          arrange(CONT,COUNTRY_NAME)%>%
          mutate(COUNTRY_NAME_f=factor(1:40,
                                       labels=COUNTRY_NAME)),
        x=~COUNTRY_NAME_f,
        y=~MeanAbsoluteError,
        color=~CONT,
        type="scatter",
        mode="markers")%>%
  layout(title="Mean Absolute Error",
         xaxis=list(title="",tickangle=45),
         yaxis=list(title="Mean Absolute Error"),
         margin=m,
         legend = list(orientation = "h",   
                       x = 0.5,
                       y=-0.8))