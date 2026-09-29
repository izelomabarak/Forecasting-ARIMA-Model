# Overview

This software is an ARIMA forecasting model made in R. It can use CSV files or user input to generate a list of future results. The user selects how many future results they want, and the software follows ARIMA principles to create an approximate prediction of future values. The first part of the software calculates the differences and determines whether the data is stationary, increasing, or variable. If the data is stationary, the software executes the stationary solution. If the data is variable, it first creates a focused list, a variance list, and two matrices. It then calculates the variance and creates the covariance list. After that, it uses the covariance and variance to calculate persistence and approximate persistence. The previous matrices are then used to obtain the most accurate possible vector coefficients, then creates an error test to simulate errors using the coefficients. The results of the errors are used to obtain values for V and X, which are used to obtain an approximate value of theta. This value is sent through multiple loops to improve its accuracy. Three functions are especially important during this process: errors_operator, which simulates the errors using the new theta; SSE_operator, which determines the SSE value for that theta; and theta_precision, which uses loops to search for an improved theta value and obtain the most accurate value possible. Finally, the new theta value is used to obtain the future predictions.

I chose this project for two main reasons. First, prediction models are extremely useful software tools with a great variety of applications. Understanding how an important model such as ARIMA works will help me become more prepared to create this type of software in the future. Second, this project allowed me to use a large part of the functionality of R and understand how different R features can be used together in a larger project.

Here you can find a software demonstration video where I explain how to use the software and how it works.

[Software Demo Video](https://youtu.be/IspMDUbcLN8)

# Development Environment

I used Visual Studio Code, a free code editor, with the R extension installed. I also installed the R programming language on my computer to execute and test the software.

I used the basic functionality and built-in functions of R. The software did not require external libraries.

# Useful Websites

- [R-project](https://www.r-project.org/)
- [IBM-What are the ARIMA models?](https://www.ibm.com/mx-es/think/topics/arima-model)

# Future Work

- Add a verification system to prevent users from entering lists with fewer than six elements and ensure that the numerical input is a numeric list.
- Add the ability to create a graph showing the predicted values after the forecasting process is completed.
- Add the ability to create CSV files where the predictions can be stored.