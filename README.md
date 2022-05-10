# CAPrecipVis
Playing with water data visualizations using monthly California precipitation data

Data is from NOAA, National Centers for Environmental Information: https://www.ncdc.noaa.gov/cag/statewide/time-series/4/pcp/all/1/1895-2022?base_prd=true&begbaseyear=1981&endbaseyear=2010

Starting figure (note: unsure what dataset this figure represents):
![Starting Figure](https://github.com/cclatterbuck/CAPrecipVis/figures/StartingFigure.jpg)

Sticking points identified in internal Teams chat:
1. Ranges of data used: try calendar year vs. water year vs. months-to-date.
2. Do all years need to be displayed? Try a few key years, mean, median, and the confidence interval around the average
3. Consider splitting years into historic vs. contemporary or plotting a rolling 3-year average rather than yearly data

Formatting points identified:
4. Cite data sources
5. Label the x-axis
6. Use colorblind friendly palette
7. Years on the legend should be in chronological order
8. Highlight current year in different ways
