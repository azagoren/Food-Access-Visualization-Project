# Food-Access-Visualization-Project

### Project Overview: 

In conjunction with Professor Valerie Nazzaro's Data Visualization course, I developed an interactive dashboard to visually explore food access within the US. This project utilizes the USDA's Food Access Research Atlas and geometries from the Census Bureau to explore the linkages between access to food stores, race, and income. Because food access typically occurs at the neighborhood level, all visualizations are displayed in terms of census tracts. This application is [publically available](https://azagoren.shinyapps.io/food_access) in the hopes that low to middle income individuals may use it as a tool to consider when making housing decisions. 


![](fa.gif)


### Dashboard Components: 

*Selections*: 
Users can select a state, degree of sensitivity (distance from the nearest food store), and a Census geographic identifier  to customize the plots to respective interests. They also have the option to receive tract recommendations based off of their income so they can easily identify tracts within their budget. 

#### Demographic Plots: 

In order to provide users with some background on the census tracts, I have included bar, pie and box plots detailing demographics and how food store access is distributed across the Tract. These graphs were included in an attempt to improve this application's accessibility, which has been constructed to be accessible across varying levels of data literacy. 

#### Sensitivity Plots: 

Our data allows for two distance measures of food access: 

**High** : 1/2 mile from the nearest food store for urban areas; 10 miles for rural areas. <br>
**Low** :  1 mile from the nearest food store for urban areas; 20 miles for rural areas. <br>

This dashboard provides sensitivity plots to contextualize the results of our earlier graphs and highlight tracts where these measures may not communicate a consistent story of food accessibility. Given that the dumbbell plot is limited to displaying 30 states, it may not fully capture tracts that should be flagged for high sensitivity within more populous states due to their proportionately greater number of census tracts. I would also like to draw attention to the absence of a suburban level within this dataset, which likely increases the sensitivity of visualizations within this study further.
