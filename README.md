[Benifit Cost Analysis]

In this project I will implement the simulation techniques to select the best project among two or more projects under consideration.

Corporations must select among many projects that are under consideration by the management. Their primary instrument for evaluating and selecting among the available projects is the benefit-cost analysis. 
In this analysis, both the annual benefits and the annual costs deriving from a project are estimated in several different categories. 
Then the total benefit is divided by the total cost to produce a benefit-cost ratio. This ratio is then used by corporations to compare numerous projects under consideration. 
A benefit-cost ratio greater than 1.0 indicates that the benefits are greater than the costs, and the higher a project’s benefit-cost ratio, the more likely it is to be selected over projects with lower ratios. 
Currently, the JET Corporation is evaluating two dam project constructions, one in southwest Georgia (Dam #1) and the other in North Carolina (Dam #2). 
The company has identified six areas of benefits: improved navigation, hydroelectric power, fish and wildlife, recreation, flood control, and the commercial development of the area. 
Furthermore, there are three estimates available for each type benefit – a minimum possible value, a most likely value (i.e., a mode or peak), and a maximum possible value. 
For the costs, two categories associated with a construction project of this type have been identified: the total capital cost, annualized over 30 years (at a rate specified by the creditors and the government), and the annual operations and maintenance costs. 
These benefits and costs estimations for both dam projects (in millions of dollars) are as follows:

![image](https://github.com/user-attachments/assets/34d7632c-66e5-40dc-a74b-7933b6e31236)


I performed a simulation to compare the benefits and costs of dam 1 and dam 2. It begins by initializing several vectors to store the results of 10,000 iterations of simulated benefits, costs, and benefit-cost ratios for each dam. 
These vectors include cost1, cost2, benifit1, benifit2, ratio1, and ratio2. The simulation employs a triangular distribution to generate random variates, facilitated by the getRandomVariate function, which takes three parameters: a (minimum), c (mode), and b (maximum). 
This function determines whether to use the ascending or descending portion of the triangular distribution based on a uniform random number and calculates the corresponding random variate.

![image](https://github.com/user-attachments/assets/c2f93b62-d2c3-43b6-8f9d-e009a24e5fc3)


I fitted three different theoretical distributions to the ratio1 data: Normal, Gamma, and Log-normal distributions. This step estimates the parameters for each distribution based on the observed data.

![image](https://github.com/user-attachments/assets/d625467c-4bcf-42c7-aacc-55226235fef7)


For the comparison, a custom function named “getRatioP (ratio, alpha)” was defined. This function calculates the probability that a value in the given ratio dataset is greater than a specified threshold alpha. 
I calculated the probability that a value from ratio1 is greater than a corresponding value from ratio2. From the result, the estimated probability that Dam 1's benefit-cost ratio will be greater than Dam 2's is approximately 55.16%.

![image](https://github.com/user-attachments/assets/ff4706e4-aa48-4d94-a83b-052202d284af)


Below are the rationales for the recommended project (Dam 1):
1.	Minimum and Maximum Values:
Ratio1 has a slightly higher minimum and maximum value compared to ratio2, indicating potentially better performance in extreme scenarios.
2.	Mean and Median:
Ratio1 also shows a slightly higher mean and median, suggesting a central tendency towards better outcomes on average.
3.	Variance and Standard Deviation:
Ratio1 also shows a slightly lower variance and standard deviation, indicating lower levels of variability in outcomes.
4.	Skewness:
Ratio1 has lower skewness compared to ratio2, implying a more symmetric distribution and potentially more stable outcomes.
5.	Probabilities (P(alpha>2) to P(alpha>1)):
The probabilities of ratio1 being greater than specific thresholds (2, 1.8, 1.5, 1.2, and 1) are generally higher than those of ratio2, indicating a higher likelihood of favorable outcomes for ratio1.
6.	Probability (ratio 1 > ratio 2):
The estimated probability that Dam 1's benefit-cost ratio will be greater than Dam 2's is approximately 55.16%, further supporting Dam 1 as the preferred option.

-------------------------------------------------------------------------------------------------------------------------------------------------------

[Inventory Management Decision Model]

This is another problem. 
As a consultant, my task is to develop and implement a decision model to help the company make the best inventory decisions.

![image](https://github.com/user-attachments/assets/0a88863f-9179-40f8-ad93-651a5a350ab2)


![image](https://github.com/user-attachments/assets/e30cd285-0e84-4a7c-bfc8-7d713939e230)

Conduct what-if analyses by using two-way tables in Excel to study the sensitivity of total cost to changes in the model parameters.

![image](https://github.com/user-attachments/assets/a1b171cb-6d40-4e16-91fd-7d0357b73c80)


The plot of Total Cost versus Order Quantity helps to understand how the total cost varies with different order quantities. It shows a clear minimum point at the optimal order quantity. The vertical red dashed line indicates the optimal order quantity, while the horizontal green dashed line represents the minimum total cost. The blue point marks the intersection point of the optimal order quantity and the minimum total cost. 

![image](https://github.com/user-attachments/assets/8f0df614-4232-4ed0-a926-d963e0ca4ee3)


