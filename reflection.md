## Reflection
#### Useful features of our app:
- Our app has a nice interaction feature that helps efficiently retrieve the information that users need. By clicking on an area name from the drop-down menu, the selected area will be highlighted on the map. If the user doesn't know the name of the area, they can note the area number on the map and search within the dropdown by number. The bars corresponding to the selected areas will be highlighted at the same time. 

- The dropdown menu also allows users to select multiple areas in which they want to compare the density of the distribution of the squirrels. The interaction feature between charts also applies when users are doing multi-selection.

- The tooltips on the bar chart show the exact count of the squirrels in that area. Users don't need to measure the length of the bars and eyeball to get a rough number by themselves.

- The squirrel counts bars are sorted in ascending order from top to bottom, which helps users easily find a place that has the most or least amount of squirrels.

- Users can select a behavior they are interested in observing to see in which areas squirrels are most often observed exhibiting that behavior.

#### Things need to be fixed or improved:  
- The biggest problem is that we can't manage to deploy our app on Heroku. Lots of work and thoughts were put into it, but we, along with Firas, had to agree that we should leave it as a local deployment for now.  

- There are also some problems with the tooltips.   
    - First, the tooltip on the map only shows the area's number instead of its name. It is not showing the count of squirrels either. Ideally, we would like to have the area's longitude, latitude, name of that area and the count of squirrel in that area in the tooltip of the map.
    - Secondly, after using drop-down selection, the tooltip is not showing in a nice-look customized way. (The labels could be more clear.)   

- Attempts to remove certain plotly functionalities from the plotly bar that do not add to the use of our app (select, lasso-select, logo) as per our lecture notes and plotly reference documents were unsuccessful. Given more time, we could investigate this issue further.

- The layout could be better-organized. If we had more time, we would try to replicate the layout we had in our Python app (including our great squirrel logo!).  

- The font size on the plots could be bigger. It was difficult to reduce overlap of the region labels while achieving a big-enough font size. It was suggested that we remove the region names from the bar plots, but we decided as a group not to do that right now.

- The latitude labels on the choropleth are far removed from the map. We attempted to fix this through various margin, sizing, and padding commands but were unsuccessful.

