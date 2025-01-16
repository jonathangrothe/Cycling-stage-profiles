# Cycling-stage-profiles
A project to work on predicting how a cycling stage will be won based on its profile. This treats a stage's profile as a quantitative variable, as I was intersted in seeing an alternative to "sprint," "hilly," and "mountain" classifications that captures a little more information about a stage (ie: something that could pick up on the differnce between a Pogacar and a Vingegaard stage). Hopefully, this could inform predictions about how likely a rider is to win a race based on the stage's profile. 

I am using data from the 2024 men's and women's world tour, with neutralized stages removed (one in Itzulia Basque country and one in Criterium du Dauphine). All data is from procyclingstats.com.

This uses random forests to predict three variables: number of riders who make the finish group, gap between group 1 and group 2, and distance solo for the winner. I am in the process of looking for other statistics that say more about how the race was won, especially for the women's world tour, which has some notable differences in the distributions of these outcome statistics. 
