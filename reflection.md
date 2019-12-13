## Reflection
#### Useful features of our app:
- Our app has a nice interaction feature that helps efficiently retrieve the information that users need. By clicking the area name from the drop-down menu, the selected area will be highlighted on the map. The corresponding bars that show the count of the squirrel from the other two plots, will also be highlighted at the same time. 

- The dropdown menu also allows users to select multi-area that they want to compare the density of the distribution of the squirrels. The interaction feature also applies when users are doing multi-selection.

- The tooltips on the bar chart show the exact count of the squirrel in that area. Users don't need to measure the length of the bars and eyeball to get a rough number by themselves.

- The squirrel counts bars are sorted in ascending order from top to bottom, which helps users easily find a place that has the most or least amount of squirrels.

#### Things need to be fixed or improved:  
- The biggest problem is that we can't manage to deploy it on Heroku. Lots of work and thoughts were put into it but Firas and us had to agree that we should leave it for now.  

- There are also some problems with the tooltips.   
    - First of all, the third plot's tooltip is messy (the one that shows the diff between am/pm). This is because when we try to make it tidy (as we did for the other plots except the map), the interactive drop-down highlight function will break for this particular plot. This is quite strange since the other plots are functioning well with their tooltips fixed in the same way. We can't really figure out the solution so we decided to keep this messy tooltip so that the drop-down highlight function won't be affected.  
    - The second problem is that, after using drop-down selection, the tooltip won't show. It's really not making sense to us..   
    - Thirdly, the tooltip on the map only shows the area's number instead of its name. It is not showing the count of squirrel either. Ideally, we would like to have the area's longitude, latitude, name of that area and the count of squirrel in that area in the tooltip of the map.

- The layout could obviously be better-organized.   

- The font size could be bigger.

