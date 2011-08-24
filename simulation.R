source('simulation_functions.R')
get.data = function(demography=list(), start=2004, datastart=1975, end=2004, data=list()) {
  T=end-datastart + 1 # +1 wegen dem ersten Jahr...
# demographic stuff
  population_data = read.csv2('Data/used for simulation/population_smoothed.csv', stringsAsFactors=F)
  yearly_data = read.csv2('Data/used for simulation/birthrate_mortality.csv', stringsAsFactors=F)
  population_data2 = read.csv2('Data/used for simulation/population_2004_smoothed.csv', stringsAsFactors=F)
#big simulation:
  #demography$population = round(as.numeric(population_data$Population)) # round is necessary because we have comma values as population is smoothed
  workforce_data = read.csv2('Data/age_specific_unemployment.csv', stringsAsFactors=F)
  demography$population = as.numeric(population_data2$Population)
  demography$in_workforce = rep(0, 150)

# big simulation:
  #probability = yearly_data$labor.force..oecd.[yearly_data$Year==1975]/sum(demography$population[16:66]) # probability that someone who is in working age (15 to 65...) is in the labor force
  #for (i in 16:66)
  #  demography$in_workforce[i] = rbinom(1, demography$population[i], probability) # this assumes that labor force is evenly distributed among the population
  #demography$out_workforce = demography$population - demography$in_workforce
  
  workforce = as.numeric(workforce_data$Value[workforce_data$Series=="Labour Force" & workforce_data$Sex=="All persons" & workforce_data$Frequency=="Annual" & workforce_data$Age!="Total"])
  age = workforce_data$Age[workforce_data$Series=="Labour Force" & workforce_data$Sex=="All persons" & workforce_data$Frequency=="Annual" & workforce_data$Age!="Total" & workforce_data$Age!="Total"]
  years = as.numeric(workforce_data$Time[workforce_data$Series=="Labour Force" & workforce_data$Sex=="All persons" & workforce_data$Frequency=="Annual" & workforce_data$Age!="Total"])
  demography$in_workforce[16:20] = round(workforce[years==start & age=="15 to 19"]*1000 / 5)
  demography$in_workforce[21:25] = round(workforce[years==start & age=="20 to 24"]*1000 / 5)
  demography$in_workforce[26:30] = round(workforce[years==start & age=="25 to 29"]*1000 / 5)
  demography$in_workforce[31:35] = round(workforce[years==start & age=="30 to 34"]*1000 / 5)
  demography$in_workforce[36:40] = round(workforce[years==start & age=="35 to 39"]*1000 / 5)
  demography$in_workforce[41:45] = round(workforce[years==start & age=="40 to 44"]*1000 / 5)
  demography$in_workforce[46:50] = round(workforce[years==start & age=="45 to 49"]*1000 / 5)
  demography$in_workforce[51:55] = round(workforce[years==start & age=="50 to 54"]*1000 / 5)
  demography$in_workforce[56:60] = round(workforce[years==start & age=="55 to 59"]*1000 / 5)
  demography$in_workforce[61:65] = round(workforce[years==start & age=="60 to 64"]*1000 / 5)
  demography$out_workforce = demography$population - demography$in_workforce

  demography$employed = rep(0, 150)
  prob = yearly_data$employment..oecd.[yearly_data$Year==start]/sum(demography$in_workforce) 
  for (i in 16:66)
    demography$employed[i] = rbinom(1, demography$in_workforce[i], prob) 
  demography$unemployed = demography$in_workforce - demography$employed
  
  
  demography$in_to_employment = as.numeric(data$pr_out_of_AL.grund) # vector of length 30, 1 for each year
  demography$out_of_employment = as.numeric(data$pr_in_to_AL.grund) # vector of length 30
  demography$out_of_workforce = as.numeric(data$pr_out_of_workforce) # vector of length 30
  demography$in_to_workforce = demography$out_of_workforce - runif(length(demography$out_of_workforce),0,demography$out_of_workforce)  
  
  
  demography$fertility[1:150] = as.numeric(population_data$birth.rate) 
  demography$mortality[1:150] = as.numeric(population_data$death.rate) 
  # demography$fertility = matrix(rep(c(rep(0,16), rep(0.10, 14), rep(0.075, 10), rep(0,110)), T), ncol=T) # dunno
  # demography$mortality = matrix(rep(c(rep(0.03,5), rep(0.005, 11), rep(0.0075, 14), rep(0.0075,10),rep(0.05,10),rep(0.15,10),rep(0.2,10),rep(0.4,10),rep(0.5,70)), T), ncol=T) # dunno
  demography$mortality[150] = 1 # at that age everybody has to die
  demography$migration = as.numeric(yearly_data$migration..OECD.)

  if (T>30) { 
  # take averages for projections
    demography$in_to_workforce[31:T] = sum(demography$in_to_workforce[1:30]) / 30
    demography$out_of_workforce[31:T] = sum(demography$out_of_workforce[1:30]) / 30
    #demography$in_to_employment[31:T] = sum(demography$in_to_employment[1:30]) / 30
    #demography$out_of_employment[31:T] = sum(demography$out_of_employment[1:30]) / 30
    demography$migration[31:T] = sum(demography$migration[1:30]) / 30
  # second projection: regression
    #vec = 1:length(demography$in_to_employment[-1])
    #res1 = lm(demography$in_to_employment[-1] ~ vec)
    #res2 = lm(demography$out_of_employment[-1] ~ vec)
    #pr1 = demography$in_to_employment[30] + res1$coefficients[2]*((30:T)-30)
    #pr2 = demography$out_of_employment[30] + res2$coefficients[2]*((30:T)-30)        
    #demography$in_to_employment[30:T] = pr1[1:length(pr1)]
    #demography$out_of_employment[30:T] = pr2[1:length(pr2)]
  # third projection: average from 1998-2004
    demography$in_to_employment[31:T] = sum(demography$in_to_employment[24:30])/length(demography$in_to_employment[24:30])
    demography$out_of_employment[31:T] = sum(demography$out_of_employment[24:30])/length(demography$out_of_employment[24:30])
  }
  
  
# only for second simulation: ?? weiß nicht mehr, was das war...
  #demography$in_to_workforce[1:T] = 0
  #demography$out_of_workforce[1:T] = 0
  #demography$in_to_employment[1:T] = 0
  #demography$out_of_employment[1:T] = 0
  return (demography)
}

set.data = function(start=2004, datastart=1975, end=2010, data=list()) {
  set.seed(1)
  
  
  # the following variables are stock variables per age cohort. They can be changed to include time. For example by defining them as a matrix with columns representing time points
  # population = rep(0,150) # total population divided by age cohorts. 150 cohorts (1 per year of age)
  # employed = rep(0,150) # number of employed. 150 cohorts (1 per year of age)
  # unemployed = rep(0,150) # number of unemployed. 150 cohorts (1 per year of age)
  # out_workforce = rep(0,150) # number of people not in the workforce. 150 cohorts (1 per year of age) 
  # in_workforce = rep(0,150) # number of people in the workforce. 150 cohorts (1 per year of age)
  # open_jobs = rep(0,150) # (will be ignored for now... maybe use later for more complicated simulation) 150 cohorts (1 per year of age)
  
  # the following variables are flow variables per age cohort (see above) 
  #crude_birth_rate = matrix(rep(0,150*T), 150) # each Kohort has a different cbr and it changes with t
  # fertility = matrix(rep(0,150*T), 150) # each Kohort has a different fertility and it changes with t
  # mortality = matrix(rep(0,150*T), 150) # each Kohort has a different mortality and it changes with t
  # out_of_workforce = matrix(rep(0,150*T), 150) # each Kohort has a different behaviour and it changes with t
  # in_to_workforce = matrix(rep(0,150*T), 150) # each Kohort has a different behaviour and it changes with t
  # net_migration = matrix(rep(0,150*T), 150) # net_migration per age cohort and time t
  # in_to_employment = matrix(rep(0,150*T), 150) # each Kohort has a different behaviour and it changes with t
  # out_of_employment = matrix(rep(0,150*T), 150) # each Kohort has a different behaviour and it changes with t
  #population_growth = matrix(rep(0,150*T), 150) # population increases through migration and births...
  
# initializion:
  demography = get.data(start=start, datastart=datastart, end=end, data=data)
  return (demography)
}

do.simulation = function(start=2004, end=2010) {
  data = read.csv2('Data/extracted from iab/csv/probabilities.csv', stringsAsFactors=F)
  datastart = 1975
  #start = min(as.numeric(data$Time))
  #end = max(as.numeric(data$Time))
  
  data = set.data(start=start, datastart=datastart, end=end, data) # initialize everything, data needs to start at 1975 so we get the right averages...
# list of results for plotting, columns in Array are for respective years
  
  data$results = list(years = rep(0,(end-datastart+2)), population=matrix(rep(0,(end-datastart+2)*150),nrow=150), in_workforce=matrix(rep(0,(end-datastart+2)*150),150), out_workforce=matrix(rep(0,(end-datastart+2)*150),150), employed=matrix(rep(0,(end-datastart+2)*150),150), unemployed=matrix(rep(0,(end-datastart+2)*150),150), out_of_workforce_unemployed=matrix(rep(0,(end-datastart+2)*150),150), out_of_unemployment=matrix(rep(0,(end-datastart+2)*150),150))
  data$start_year = start
  data$end_year = end
# initial values  
  data$results$years[start-datastart+1] = start
  data$results$population[,start-datastart+1] = data$population
  data$results$in_workforce[,start-datastart+1] = data$in_workforce
  data$results$out_workforce[,start-datastart+1] = data$out_workforce
  data$results$employed[,start-datastart+1] = data$employed
  data$results$unemployed[,start-datastart+1] = data$unemployed
  
  for (t in start:end) {
    T = t-(datastart-1) # year counter, starts from start-datastart+1 (e.g. 2004 - 1975 + 1 = 30
    
      

    print(paste('t: ',t));
    print(paste('population: ', sum(data$population)))
    print(paste('in workforce: ', sum(data$in_workforce)))
    print(paste('employed: ', sum(data$employed)));
    print(paste('unemployed: ', sum(data$unemployed)));
    print(paste('unemployment rate: ', sum(data$unemployed)/sum(data$in_workforce))); 

   # population stuff
    data = deaths(data, T=T)
    #print('data after deaths');print(data$unemployed)
    data = age(data, T=T) # let population age...
    #print('data after age');print(data$unemployed)
    data = births(data, T=T) # put births...
    #print('data after births');print(data$unemployed)	
    data = migrate(data, T=T) # put births...
  # workforce stuff
   
    change = move_workforce(data, T=T)
    data = change_employment_on_workforce_change(data, change, T=T)
    data$out_workforce = data$out_workforce + change$move_out_workforce - change$move_in_workforce
    data$in_workforce = data$in_workforce + change$move_in_workforce - change$move_out_workforce
    
    
 # employment effect of workforce movement:   
  # employment stuff
    change = change_employment(data, T=T)
    data$unemployed = data$unemployed + change$unemploy - change$employ
    data$employed = data$employed - change$unemploy + change$employ
    data$results$out_of_unemployment[,T+1] = data$results$out_of_unemployment[,T+1] + change$employ
   # save results by year (T+1 because row 1 is initial value:
    data$results$years[T+1] = t+1
    data$results$population[,T+1] = data$population
    data$results$in_workforce[,T+1] = data$in_workforce
    data$results$out_workforce[,T+1] = data$out_workforce
    data$results$employed[,T+1] = data$employed
    data$results$unemployed[,T+1] = data$unemployed
  }

  return (data)
}