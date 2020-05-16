# evolvo

*Disclaimer:* There is no political motivation in this at all and would like to remain so. Please keep any politcal discussion outside the repo. Also I am not an expert on the subject. I am very curious of it but couldn't find relevant literature. If you happen to know the right place to look please, let me know. I appreciate it.

This little program simulates the evolution within a society.
For lack of a better term, I use `power` to represent the overall fitness of an individual in a society. 
This `power` encompasses serveral traits of an individual including IQ, EQ, etc. `Power` can also be deemed as the overall internal factor for a person's success in the society.  

This little program simulates the evolution of this `power` using the following assumptions

1. People tends to marry with others with similiar `power`
2. Children's power are in a normal distrubtion around the mean of the two parents' `power`. 
3. Most people reproduce at about the same rate regardless of their `power`, except those whose uncommonly low `power` incurs severe disabilities. 


To run 

```bash
sbt plot/run
```
You will see
![img](https://i.imgur.com/G7jEfnk.gif)


### Quick take away

The model is still highly simplified of the real world situation. It shows that the evolution push the `power` of the top percentile much higher than the general population, caushing a widening gap between the elite class and the rest of the population. 
This is intuitive in that while there is no selection pressure on the general population, the top circles remains selective in regards to genetic fitness, and people in those circles tends to breed with each other hence driving the natural selection in the group. 
