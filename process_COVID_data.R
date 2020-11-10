library(data.table)
library(tidyverse)
library(dplyr)
library(plotly)
library(DT)
library(knitr)
library(ggplot2)

cv_states_readin <- as.data.frame(data.table::fread("/Users/sherryshen/Desktop/us-states.csv") )

# load state population data
### FINISH THE CODE HERE ###
state_pops_readin <- as.data.frame(data.table::fread("/Users/sherryshen/Desktop/us_census.csv"))

state_pops <- state_pops_readin
state_pops$abb <- state_pops$state
state_pops$state <- state_pops$state_name
state_pops$state_name <- NULL

### FINISH THE CODE HERE ###
cv_states <- merge( cv_states_readin, state_pops, by="state")

### 2. Look at the data

dim(cv_states)
head(cv_states)
tail(cv_states)
str(cv_states) #structure



# format the date
cv_states$date <- as.Date(cv_states$date, format="%Y-%m-%d")

# format the state variable
state_list <- unique(cv_states$state)
#table(cv_states$state)
cv_states$state <- factor(cv_states$state, levels = state_list)

# format the state abbreviation (abb) variable
### FINISH THE CODE HERE ###
abb_list = unique(cv_states$abb)
cv_states$abb = factor(cv_states$abb, levels = abb_list)

# order the data first by state, second by date
cv_states = cv_states[order(cv_states$state, cv_states$date),]

# Confirm the variables are now correctly formatted
str(cv_states)
head(cv_states)
tail(cv_states)

# Inspect the range values for each variable. What is the date range? The range of cases and deaths?
head(cv_states)
summary(cv_states)
min(cv_states$date)
max(cv_states$date)

# Add variables for new_cases and new_deaths:
for (i in 1:length(state_list)) {
  cv_subset = subset(cv_states, state == state_list[i])
  cv_subset = cv_subset[order(cv_subset$date),]

  # add starting level for new cases and deaths
  cv_subset$new_cases = cv_subset$cases[1]
  cv_subset$new_deaths = cv_subset$deaths[1]

  #### FINISH THE CODE HERE ###
  for (j in 2:nrow(cv_subset)) {
    cv_subset$new_cases[j] = cv_subset$cases[j] - cv_subset$cases[j-1] 
    cv_subset$new_deaths[j] = cv_subset$deaths[j] - cv_subset$deaths[j-1]
  }

  # include in main dataset
  cv_states$new_cases[cv_states$state==state_list[i]] = cv_subset$new_cases
  cv_states$new_deaths[cv_states$state==state_list[i]] = cv_subset$new_deaths
}

# Inspect outliers in new_cases and new_deaths using plotly
### FINISH THE CODE HERE ###
p1<-ggplot(cv_states, 
           aes( x=date, y=new_cases, color=state  )
           ) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p1)
p1<-NULL # to clear from workspace

### FINISH THE CODE HERE ###
p2<-ggplot(cv_states, 
           aes(x=date, y=new_deaths, color=state )
           ) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p2)
p2<-NULL # to clear from workspace

# set negative new case or death counts to 0
cv_states$new_cases[cv_states$new_cases<0] = 0
cv_states$new_deaths[cv_states$new_deaths<0] = 0

# Recalculate `cases` and `deaths` as cumulative sum of updates `new_cases` and `new_deaths`
for (i in 1:length(state_list)) {
  cv_subset = subset(cv_states, state == state_list[i])

  # add starting level for new cases and deaths
  cv_subset$cases = cv_subset$cases[1]
  cv_subset$deaths = cv_subset$deaths[1]

  for (j in 2:nrow(cv_subset)) {
    cv_subset$cases[j] = cv_subset$new_cases[j] + cv_subset$cases[j-1]
    cv_subset$deaths[j] = cv_subset$new_deaths[j] + cv_subset$deaths[j-1]
  }
  # include in main dataset
  cv_states$cases[cv_states$state==state_list[i]] = cv_subset$cases
  cv_states$deaths[cv_states$state==state_list[i]] = cv_subset$deaths
}


# add population normalized (by 100,000) counts for each variable
cv_states$per100k =  as.numeric(format(round(cv_states$cases/(cv_states$population/100000),1),nsmall=1))
cv_states$newper100k =  as.numeric(format(round(cv_states$new_cases/(cv_states$population/100000),1),nsmall=1))
cv_states$deathsper100k =  as.numeric(format(round(cv_states$deaths/(cv_states$population/100000),1),nsmall=1))
cv_states$newdeathsper100k =  as.numeric(format(round(cv_states$new_deaths/(cv_states$population/100000),1),nsmall=1))

# add a naive_CFR variable = deaths / cases
cv_states = cv_states %>% mutate(naive_CFR = round((deaths*100/cases),2))

# create a `cv_states_today` variable
### FINISH THE CODE HERE ###
max_date <- max(cv_states$date)
cv_states_today = cv_states %>% filter(date==as.Date(max_date))




# pop_density vs. cases

cv_states_today %>% 
  plot_ly(x = ~pop_density, y = ~cases, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))

# filter out "District of Columbia"
cv_states_today_scatter <- cv_states_today %>% filter(state!="District of Columbia")

# pop_density vs. cases after filtering
cv_states_today_scatter %>% 
  plot_ly(x = ~pop_density, y = ~cases, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))

# pop_density vs. deathsper100k
cv_states_today_scatter %>% 
  plot_ly(x = ~pop_density, y = ~newdeathsper100k, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))

# Adding hoverinfo
cv_states_today_scatter %>% 
  plot_ly(x = ~pop_density, y = ~deathsper100k,
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5),
          hoverinfo = 'text',
          text = ~paste( paste(state, ":", sep=""), paste(" Cases per 100k: ", per100k, sep="") , paste(" Deaths per 100k: ",
                        deathsper100k, sep=""), sep = "<br>")) %>%
  layout(title = "Population-normalized COVID-19 deaths (per 100k) vs. population density for US states",
                  yaxis = list(title = "Deaths per 100k"), xaxis = list(title = "Population Density"),
         hovermode = "compare")


### FINISH THE CODE HERE ###
p <- ggplot(cv_states_today_scatter, aes(x=pop_density, y=per100k, color=state, size=population)) + geom_point() + geom_smooth() 
ggplotly(p)


# Line chart for naive_CFR for all states over time using `plot_ly()`
plot_ly(cv_states, x = ~date, y = ~naive_CFR, color = ~state, type = "scatter", mode = "lines")

# Line chart for Texas showing new_cases and new_deaths together
### FINISH THE CODE HERE ###
cv_states %>% filter(state=="Texas") %>% plot_ly(x = ~date, y = ~new_cases, type = "scatter", mode = "lines") %>% add_lines(x = ~date, y = ~new_deaths, type = "scatter", mode = "lines")



# Map state, date, and new_cases to a matrix
library(tidyr)
cv_states_mat <- cv_states %>% select(state, date, new_cases) %>% filter(date>as.Date("2020-04-01"))
cv_states_mat2 <- as.data.frame(pivot_wider(cv_states_mat, names_from = state, values_from = new_cases))
rownames(cv_states_mat2) <- cv_states_mat2$date
cv_states_mat2$date <- NULL
cv_states_mat2 <- as.matrix(cv_states_mat2)

# Create a heatmap using plot_ly()
plot_ly(x=colnames(cv_states_mat2), y=rownames(cv_states_mat2),
        z=~cv_states_mat2,
        type="heatmap",
        showscale=T)

# Create a second heatmap after filtering to only include dates every other week
filter_dates <- seq(as.Date("2020-04-01"), as.Date("2020-10-01"), by="2 weeks")

### FINISH THE CODE HERE ### 
cv_states_mat <- cv_states %>% select(state, date, new_cases) %>% filter( date %in% filter_dates )

cv_states_mat2 <- as.data.frame(pivot_wider(cv_states_mat, names_from = state, values_from = new_cases))
rownames(cv_states_mat2) <- cv_states_mat2$date
cv_states_mat2$date <- NULL
cv_states_mat2 <- as.matrix(cv_states_mat2)

# Create a heatmap using plot_ly()
plot_ly(x=colnames(cv_states_mat2), y=rownames(cv_states_mat2),
        z=~cv_states_mat2,
        type="heatmap",
        showscale=T)


### For May 1 2020

# Extract the data for each state by its abbreviation
cv_CFR <- cv_states %>% filter(date=="2020-05-01") %>% select(state, abb, naive_CFR, cases, deaths) # select data
cv_CFR$state_name <- cv_CFR$state
cv_CFR$state <- cv_CFR$abb
cv_CFR$abb <- NULL

# Create hover text
cv_CFR$hover <- with(cv_CFR, paste(state_name, '<br>', "CFR: ", naive_CFR, '<br>', "Cases: ", cases, '<br>', "Deaths: ", deaths))

# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Make sure both maps are on the same color scale
shadeLimit <- 9

# Create the map
fig <- plot_geo(cv_CFR, locationmode = 'USA-states') %>% 
  add_trace(
    z = ~naive_CFR, text = ~hover, locations = ~state,
    color = ~naive_CFR, colors = 'Purples'
  )
fig <- fig %>% colorbar(title = "CFR May 1 2020", limits = c(0,shadeLimit))
fig <- fig %>% layout(
  title = paste('CFR by State as of', Sys.Date(), '<br>(Hover for value)'),
  geo = set_map_details
)
fig_May1 <- fig

#############
### For Today

# Extract the data for each state by its abbreviation
cv_CFR <- cv_states_today %>%  select(state, abb, naive_CFR, cases, deaths) # select data
cv_CFR$state_name <- cv_CFR$state
cv_CFR$state <- cv_CFR$abb
cv_CFR$abb <- NULL

# Create hover text
cv_CFR$hover <- with(cv_CFR, paste(state_name, '<br>', "CFR: ", naive_CFR, '<br>', "Cases: ", cases, '<br>', "Deaths: ", deaths))

# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Create the map
fig <- plot_geo(cv_CFR, locationmode = 'USA-states') %>% 
  add_trace(
    z = ~naive_CFR, text = ~hover, locations = ~state,
    color = ~naive_CFR, colors = 'Purples'
  )
fig <- fig %>% colorbar(title = "CFR May 1 2020", limits = c(0,shadeLimit))
fig <- fig %>% layout(
  title = paste('CFR by State as of', Sys.Date(), '<br>(Hover for value)'),
  geo = set_map_details
)
fig_Today <- fig


### Plot side by side 
### FINISH THE CODE HERE ###
subplot( fig_May1, fig_Today )