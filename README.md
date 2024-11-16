# What Can Survey Data Tell Us About Ideological Differences Between Black Voters and Black Nonvoters?
Analysis comparing public opinion among Black Americans who voted or stayed home in 2020.

This repository contains the code used to replicate, in its entirety, the analysis for [my blog post](https://zacharylhertz.github.io/posts/2024/11/black-voters-and-nonvoters) unpacking differences in attitudes and demographics among Black voters and nonvoters from the 2020 election.

## Data 
I used the [2020 Cooperative Election Study](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E9N6PH) in my analysis. This dataset, which is a nationally-representative sample, originally contacted 60,000 respondents. I filter to just Black respondents (n=4,912). 

Voter status is determined using the CL_2020gvm variable: respondents with a validated voting record, no matter their mode of participation, are defined as voters. There are 3,096 Black respondents who were successfully matched to the voterfile. Both matched non-voters and non-matched respondents are defined as non-voters; a deeper discussion of this definition can be found in the CES Guide. There are 1,816 respondents who were defined as non-voters.

All analysis is done using the CES common weight using the survey package. We use the common weight because our analysis includes questions from the pre and post-election waves of our survey.

## A note on the analysis
I've done my best to thoroughly comment my code and make steps clear, particularly to those who might not be familiar with the data or methods used here. If things remain unclear or you have any questions about the analysis, please reach out to me at zachary_hertz@berkeley.edu and I'll do my best to get back to you.
