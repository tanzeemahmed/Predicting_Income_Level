# Predicting_Income_Level
Classification of an Individual into High Income or Low Income group based on certain metrics

Attribute Information:
1.age: continuous variable
2. working_sector: sector under which the employee is working
3. financial_weight: weighted attribute to balance the difference in the monetary and
working conditions. It is a continuous variable
4. qualification: Eduacational qualification
5. years_of_education: number of years of education; continuous variable
6. tax paid: amount of tax paid by the peron(continuous variable)
7. loan taken: it is a two level categorical variable defining whether the person has
taken loan or no
8. marital status: categorical variable
9. occupation : area of work, a categorical variable with 14 levels
10. relationship : provides relationship status of the employee
11. ethnicity : social background (categorical variable )
12. gender: two level categorical variable
13. gain : it illustrates the financial gain of an person, it is a continuous variable
14. loss: financial loss of the person , it is a continuous variable
15. working_hours : it is a continuous variables describing hours of work of an
employee in a week
16. country: describes the origin country of an employee
17. target: Based on the given data your model will have to classify a person into high
income / low income.

The approcahes taken are :
1. Logistic regression
2. Logistic Regression with Step AIC and VIF to regularise and also remove collinearity.
3. Logistic Rgeression with Ridge and Lasso regularisation.
