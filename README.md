# DSCI 532 Group 203
Milestones 2 and 3 for DSCI 532


## Authors:  
* Cari Gostic  
* Roc Zhang  
* Lori Fang  

## Milestone 3 & 4 - Description of the app & sketch  

#### App description:

> Our app contains four plots to help our users learn about the distribution of squirrels in the Central Park. It will enable users to  efficiently plan their visit to the park, either to have more interaction with the squirrels or avoid them (for example, if they were planning a picnic). Additionally, users can find information about where to go to observe a certain behavior of the squirrels.
>
>The first plot on the top-left is a map of the park, with a color gradient filling in each region. The deeper the color, the more squirrels are observed in that region. With this map, the users can directly plan to which part of the park they should go to find more or fewer squirrels. Each region on the map will be marked by an id number, which can be used to look up the name of the park region within the app. 
>
>The bar chart on the top-right gives the count of squirrels observed in each region of the park during the 2018 Squirrel Census. This bar chart displays explicitly the difference in number of squirrels across park regions. The bars are ordered so that our users can quickly identify the most- and the least-squirrel-populated regions. The bar chart on the bottom-right shows the difference in the number of squirrels between morning (AM) and afternoon (PM). The value is calculated by `PM` - `AM`, which means positive-value bars (pink) indicates more squirrels in the afternoon (PM).  
>
>The last plot on the bottom-left gives information about the squirrel behavior throughout the park. Users can choose a behavior of interest in the drop-down menu, and the plot will update accordingly to show the number of observed instances of that behavior in each park area.  
>
>Users can select multiple regions by choosing them in a dropdown, and the bars in all three bar charts will be highlighted based on the chosen regions.
>

#### Sketch:

![image](https://i.ibb.co/rMjtBg1/sketch-full.png)  


> An example of how the plots will interact with user's selection of regions:

![image](https://i.ibb.co/6wqpDBw/sketch-highlight.png)  


## Milestone2

### App Functionality 
> 
* View squirrel distribution by park region, time of the day, and behavior.  
* Use the drop-down menu to select regions to highlight across all charts. 
* Use the drop-down menu to change the behavior displayed in the bottom-left chart. 
