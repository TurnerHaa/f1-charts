# Automated Formula 1 visualizations 
Using rvest to web scrape [motorsportstats.com](https://motorsportstats.com/) Formula 1 race data and create bump chart visualizations of how drivers and teamed performed lap-by-lap (using gghighlight).

**Note:** This code requires semi-frequent updates to keep up with the changing structure of data on motorsport.com. 

## The brief
Running nearly every week between March and December every year, it can be difficult keeping up with every Formula 1 race. Even when you watch live, there are 20 drivers on a grid, and it's easy to miss key stories, comebacks and clashes. 

So to make it easier, I wanted to build a tool to automate the process of making simple stylish bump charts summarising each drivers position throughout a race.

## Sourcing the data
Many sites list the final results of F1 races, but not as many provide positions every lap. Thankfully, Motorsportstats.com provides race tables using each drivers unique number that we can convert to a name later using lookup tables in R.

The default settings for the page don't show the full number of laps, so we have to set the page limit to 'all' and grab the updated url that appears in Chrome's Dev Tools > Network page

<img src="https://github.com/TurnerHaa/f1-charts/blob/main/process-pics/obtain-data.png" height="30%">

## Data cleaning/analysis
We scrape the data from our url using rvest, creating a data frame that at first matches the structure seen on Motorsport.com. Then, we clean the data to make it more user friendly like changing driver numbers to driver namesm mutating additional columns for driver's teams and adding shades of each team's colours as a column for each driver for scaling colour values when we visualize.

<img src="https://github.com/TurnerHaa/f1-charts/blob/main/process-pics/cleaning-data.png"  height="30%">

## Result
With our cleaned data, we're ready to create data visualizations in ggplot2. A bump chart is the obvious choice, essentially reflecting the 'rank' of each driver every lap. Alternating background rectangles help distinguish every 10 laps and we've introduced a 'DNF' vector for drivers who don't complete the race (crashes, malfunctions, forfeits). These are treated differently in the visualization, marked with a diamond on the respective lap they end on.

With a social media audience in mind we produce two versions of the plots, one 16x9 and another 1x1 (square) to suit the image requirements of different platforms.

With 20 drivers and colours, these visuals are still busy. So we can use gghighlight to spotlight specifc drivers or teams to call out specific stories, highlighting only the geom_lines and geom_points relevant to the narrative we want to call out.


<img src="https://github.com/TurnerHaa/f1-charts/blob/main/images/Miami.png" height="50%">
<img src="https://github.com/TurnerHaa/f1-charts/blob/main/images/Singapore.png" height="50%">
<img src="https://github.com/TurnerHaa/f1-charts/blob/main/images/United%20Statessquare.png"  height="50%">
