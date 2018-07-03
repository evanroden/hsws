
.# We want to design new batteries suited to particular applications. 
# To do so, we need to understand what properties are related to capacity and 
# voltage, and what properties are necessary for a particular type of battery.
# From there, we can make "design rules" that help dictate what possible 
# compounds we can use.

# You will be looking at all possible correlations between the sets of 
# elements and compounds.

#######################  Materials Science Background  ########################
# Battery capacity is the available current and lifetime of a battery, 
# for a specific discharge rate and time.
# For example, a battery with a capacity of 1 amp-hour can supply 1 amp of 
# current for one hour, or two amps for half an hour, and so on, before it is
# completely drained.

# Voltage is the difference in electric potential energy.
# A fully charged battery will have a higher voltage than a discharged
# battery, similar to how a ball being held above the ground will have a higher
# potential energy than a ball resting on the floor.
###############################################################################

### Part 1: Filtering Numerical Data  ###

setwd("~/Evan HSWS/project/battery-correlations")
read.csv("battery-data.csv")
batteryFrame = read.csv("battery-data.csv")
View(batteryFrame)

elemFrame=read.csv("../elemental-data.csv")
View(elemFrame)
# Write a functilon that takes a data frame as the input and returns a data 
batteryData=subset(batteryFrame,select=c(Average.Output.Voltage, Capacity))
View(batteryData)
# frame containing only the numerical values as the output.
# This will allow you to apply your correlation function to an entire data frame.

### Part 2: Finding Correlations  ###

# Take a subset of your battery data set that only has data for compounds
# including a specific element of your choosing. 
compoundData=subset(batteryFrame,select=c(A,B))
View(compoundData)
# Repeat your previous step: using your subset of the data, apply the 
# correlation function to the battery property and elemental data.
# You may need to use the as.vector() function when searching the data frames.

cor(batteryData)

# Make a plot of the data to confirm the result of the correlation.

head(batteryFrame)
batteryFrame %>%
  filter(Average.Output.Voltage>0)%>%
  ggplot(aes(x=Average.Output.Voltage,y=Capacity),col=A,size=B)+
  geom_point(alpha=.6)

# Was your correlation weak or strong? How was this supported by the plot?
#
# Weak, as many of the points are far from the line
#

# Find the dimensions of the elemental data frame. How many properties does
# it contain? How many of those can be compared with the elemental data?

dim(batteryFrame)
# 24, 2

# Clearly, there is too much data to go through and repeat the earlier process
# manually. (Incidentally, the elemental data set is still relatively small.) 
# Fortunately, you can use functions to generalize those steps and 
# do the filtering and correlating for you. 

# Write a function to find correlations between battery data and elemental
# properties, given the element A of a compound.

cor(batteryFrame$A, batteryData&Average.Output.Voltage)

batteryFunction = function(asym){
  asym=as.character(asym)
  print(asym)
  subFrame=batteryFrame[batteryFrame$A==asym,]
  subElement=elemFrame[elemFrame$Symbol %in% subframe_vector,]
  
  subFrame = select_if(subFrame, is.numeric)
  subElement = select_if(subElement, is.numeric)
  
  View(subFrame)
  View(subElement)
  
  corData=cor(subFrame,subElement)
  return(as.data.frame(corData))
}

batteryFunction = function(asym){
  
}

# The function should take an element symbol as the input.
# It should then find the elements used in conjunction with the given
# element, and find properties of the compounds (from the battery data)
# and the properties of the associated elements (from the elemental data).
# It should then apply the function you wrote previously to filter out 
# non-numerical data from both data frames.
# Finally, it should return a data frame that with the correlation coefficients
# between the battery data and elemental data. 


# Write a second function that takes element B in the compound as input and 
# subsets the data the same way as the first function.


# As before, take a moment to explain what it is you are correlating, and 
# what the correlation coefficients indicate.


###  Part 3: Finding Candidate Compounds   ###

# Batteries used for different applications will have different design 
# requirements. For example, a car battery shouldn't be too heavy, since the 
# weight will impact fuel efficiency. It also needs to be able to withstand 
# higher temperatures than, say, a watch battery would.

# In general, capacity is a property we wish to maximize, since we want 
# batteries that can go a long time without being recharged.

# Voltage depends more on the particular application of the battery.
# In general, the more power required, the higher the voltage should be.
# Returning to the previous example, the watch battery will have a lower voltage
# than the car battery, since it doesn't require as much energy to operate.

# Suppose you were designing a new pacemaker battery, using zinc and another
# currently undecided element. 
# What features are important for a pacemaker battery to have?
# Consider the not only the battery properties, but also the impact that the
# wearer's body will have on the battery and vice-versa.

# Search the battery data set for only zinc compounds, and the elemental
# data set for the other elements used with zinc.

# Decide on one property in the elemental data set that correlates well with
# battery capacity.
# Make plots comparing this property with battery capacity and voltage.

# What current battery compound would be best for use in a pacemaker?
# What issues might there be using these compound?
# What additional properties would you consider when making a pacemaker battery?

# Use the values obtained from the plots to get a sense of a reasonable cutoff 
# point for the elemental property you chose. 
# Use this and the other factors you decided on to narrow down the list of 
# possible elements to use with zinc. 


# How did you determine what values of the properties to use?
# What final elements do you think are worth testing?


# Next, suppose you are designing a battery for a backup generator using
# magnesium and another undecided element.

# Decide on one or more properties in the elemental data set that correlate
# well with battery capacity and/or voltage.
# Make plots comparing this property with battery capacity and voltage.

# What current battery compound would be best for use in a generator?
# What properties would you consider when making a generator battery?
# How are they different from the pacemaker battery?
# Once again, use the values obtained from the plots to get a sense of 
# reasonable cutoff points for the elemental property or properties you chose. 
# Use this and the other factors you decided on to narrow down the list of 
# possible elements to use with magnesium in the generator battery. 


# How did you determine what values of the properties to use?
# What final elements do you think are worth testing?


# Are there other types of batteries you're interested in? Determine what 
# properties would be necessary for another type not specified here. Choose
# an element to specify, and repeat the process.

# Were you able to find new compounds for this battery? 