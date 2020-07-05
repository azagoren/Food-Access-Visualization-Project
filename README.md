# Food Access Visualization Project

### Project Overview: 

In conjunction with Professor Valerie Nazzaro's Data Visualization course at Wesleyan, I developed an interactive dashboard to visually explore food access within the US. This project utilizes the USDA's Food Access Research Atlas and geometries from the Census Bureau to explore the linkages between access to food stores, race, and income. Because food access typically occurs at the neighborhood level, all visualizations are displayed in terms of census tracts. This application is [publically available](https://azagoren.shinyapps.io/food_access) in the hopes that low to middle income individuals may use it as a tool to consider when making housing decisions. 


![](fa.gif)


### Dashboard Components: 

#### Selections: 
Users can select a state, degree of sensitivity (distance from the nearest food store), and a Census geographic identifier and receive a series of customized plots tailored to their interest and selected census tract. They also have the option to receive tract recommendations based off of their income so they can easily identify tracts within their budget. 

#### Demographic Plots: 

Bar, pie and box plots describing tract demographics, food store access distribution and tract characterteristics provide additional background on the selected Tract. Stories of food access vary greatly by neighborhood, so providing visuals can plainly show the distinctions between a small low to moderate income city with pervasive food access issues and a sprawling, affluent suburb where large property sizes influence the geographic proximity of food stores plays an key role in the visual storytelling. These graphs were included in an attempt to improve this application's accessibility, which has been constructed to be accessible across varying levels of data literacy. 

#### Sensitivity Plots: 

Our data allows for two distance measures (sensitivity) of food access: 

**High** : 1/2 mile from the nearest food store for urban areas; 10 miles for rural areas. <br>
**Low** :  1 mile from the nearest food store for urban areas; 20 miles for rural areas. <br>

This dashboard provides sensitivity plots to contextualize the results of our demographic plots and highlight tracts where these measures may not communicate a consistent story of food accessibility. Given that the dumbbell plot is limited to displaying 30 states, it may not fully capture tracts that should be flagged for high sensitivity within more populous states due to their proportionately greater number of census tracts. I would also like to draw attention to the absence of a suburban level within this dataset, which likely increases the sensitivity of visualizations further. Especially due to the micro-level at which food access occurs, giving a tract in a suburban town the same urban status as a tract in a metropolitan area may lead to a mischaracterization of food access within suburban areas. 
