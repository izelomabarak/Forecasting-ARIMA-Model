# The interface for start and manage the imput of the user
ARIMA_interface <- function() {
    print("You will use a file or enter the dates and number manualy (file/manualy):" )
    letter_mode <- readline("")
    letter_mode <- mode_verificator(letter_mode)
    if (letter_mode == "file") {
        print("Introduce the name of the file (must be a CSV file): ")
        name <- readline("")
        print("Introduce the number of future periods to predict(note, must be a integrer number and each period is more unpresise): ")
        times <- readline("")
        data_file <- read.csv(name)
        numbers <- as.numeric(data_file[, 2])
        times <- as.integer(times)
        ARIMA_Function(numbers, times)
        print("Whit this data")
        print(data_file)
    } else {
        print("Introduce the numbers separated for spaces like X X X X and must be more that 6 numbers: ")
        numbers <- readline(" ")
        print("Introduce the dates separated for spaces like X X X X(note, this will be only presise if the dates have se same gap of time): ")
        dates <- readline(" ")
        print("Introduce the number of future periods to predict(note, must be a integrer number and each period is more unpresise): ")
        times <- readline(" ")
        numbers <- as.numeric(strsplit(numbers, " ")[[1]])
        dates <- strsplit(dates, " ")[[1]]
        times <- as.integer(times)
        ARIMA_Function(numbers, times)
        data_imput <- data.frame(dates = c(), numbers = c())
        dates_index <- as.integer(length(dates))
        for ( i in 1:dates_index) {
            data_imput <- rbind(data_imput, data.frame(dates = dates[i], numbers = numbers[i]))
        }
        print("Whit this data")
        print(data_imput)
    }
    print("You will use again the ARIMA software (Y/N): ")
    letter <- readline(" ")
    letter <- N_Y_verificator(letter)
    if (letter == "Y") {
        ARIMA_interface()
    } 
    print("Ok, see you lather")
}
#The ARIMA function
ARIMA_Function <- function(numbers, times) {
    #The first part for guet the difernces and determinate if is stationary or not
    predict_list <- c()
    index <- as.integer(length(numbers))
    list <- c()
    stationary <- TRUE
    difference <- 0
    for ( i in 1:(index - 1)) {
        value <- numbers[[i+1]] - numbers[[i]]
        list <- append(list, value)
    }
    list_index <- as.integer(length(list))
    for (i in 1:(list_index - 1)) {
        if ( list[[i]] != list[[i+1]] ) {
            stationary <- FALSE
        }
    }
    if (stationary == TRUE) {
        #The solution if is stationary
        predict_number <- numbers[[index]] + list[[1]]
        predict_list <- append(predict_list, predict_number)
        difference <- list[[1]]
        if (times > 1) {
            for (i in 2:times) {
                predict_list_index <- as.integer(length(predict_list))
                predict_number <- predict_list[[predict_list_index]] + difference
                predict_list <- append(predict_list, predict_number)
            }
        }
    } else {
        #The solution if is variable, firts crate a focused list, a varience list and 2 matrix
        mean <- mean(list)
        focused_list <- c()
        focused_list_variance <- c()
        matrix_X <- matrix(numeric(), ncol = 3, nrow = 0)
        vector_V <- matrix(numeric(), ncol = 1, nrow = 0)
        for (i in 1:list_index) {
            focused_value <- list[[i]] - mean
            focused_list <- append(focused_list, focused_value)
            focused_list_variance <- append(focused_list_variance, focused_value * focused_value)
            if (i >= 3) {
                matrix_X <- rbind(matrix_X, c(1, list[[i-1]], list[[i-2]]))
                vector_V <- rbind(vector_V, c(list[[i]]))
            }
        }
        #Guet the varieance and create the covariance list
        variance <- mean(focused_list_variance)
        covariance_list <- c()
        focused_list_index <- as.integer(length(focused_list))
        for (i in 1:(focused_list_index - 1) ) {
            covariance_value <- focused_list[[i]] * focused_list[[i+1]]
            covariance_list <- append(covariance_list, covariance_value)
        }
        #Get the covariance and whit the varienace get the persistence and them the aproximated persisitent
        covariance <- sum(covariance_list) / focused_list_index
        persistence <- covariance / variance
        constant <- mean * (1 - persistence)
        #Use the previus matrix for get the most acuarate vector coeficients
        vector_coefficients <- solve(t(matrix_X) %*% matrix_X) %*% t(matrix_X) %*% vector_V
        constant_vector <- vector_coefficients[1,1]
        coefficients_1 <- vector_coefficients[2,1]
        coefficients_2 <- vector_coefficients[3,1]
        #Create a errors prove for simulate the errors with the help of the coefficients 
        provisional_errors <- c(0, 0)
        for (i in 3:list_index) {
            error <- list[[i]] - (constant_vector + (coefficients_1 * list[[i-1]]) + (coefficients_2 * list[[i-2]]))
            provisional_errors <- append(provisional_errors, error)
        }
        #Guet the values of V and X in base of the errors
        V <- 0
        X <- 0
        for(i in 3:focused_list_index) {
            V <- V + (provisional_errors[[i-1]] * focused_list[[i]])
            X <- X + (provisional_errors[[i-1]] ^ 2)
        }
        #Optain and secure the most acuarate podible value of theta
        theta <- V / X
        if (theta >= 1.01) {
           theta <- 1.00
        }
        if (theta <= -1.01) {
           theta <- -1.00
        }
        #Start the loops with the theta value for see is theres posible improvement of the presicion 
        #Here we build the reusale functions called errors_operator, SSE_operator and theta_presicion
        errors <- errors_operator(list, list_index, constant, persistence, theta)
        errors_index <- as.integer(length(errors))
        SSE <- SSE_operator(errors, errors_index)
        theta_minus_possible <- TRUE
        theta_sum_possible <- TRUE
        final_theta <- theta_presicion(list, list_index, constant, persistence, SSE, theta, theta_minus_possible, theta_sum_possible)
        #After all of that the new theta value is used to guet the future values
        difference <- constant + (persistence * list[[list_index]]) + (final_theta * errors[[errors_index]])
        predict_number <- numbers[[index]] + difference
        predict_list <- append(predict_list, predict_number)
        if (times > 1) {
            for (i in 2:times) {
                predict_list_index <- as.integer(length(predict_list))
                predict_number <- predict_list[[predict_list_index]] + difference
                predict_list <- append(predict_list, predict_number)
            }
        }
    }
    formula <- 1 + 2i
    print(paste("The formula was",  formula , "were 1 is the previus number and 2i is the diference, the diference is", difference))
    print("The predictions are")
    print(predict_list)
}
#Simulate the errors now whit theta
errors_operator <- function(list, list_index, constant, persistence, theta) {
    errors <- c(0)
    for (i in 2:list_index) {
        error <- list[[i]] - (constant + (persistence * list[[i-1]]) + (theta * errors[[i-1]]))
        errors <- append(errors, error)
    }
    return(errors)
}
#Determinathes the value of the SSE
SSE_operator <- function(errors, errors_index) {
    SSE <- 0
    for(i in 1:errors_index) {
        SSE <- SSE + (errors[[i]] * errors[[i]])
    }
    return(SSE)
}
#Loop function for search to improve the theta value for guet the most acuarated value posible
theta_presicion <- function(list, list_index, constant, persistence, SSE, theta, theta_minus_possible, theta_sum_possible) {
    if (theta_minus_possible == TRUE && theta_sum_possible == TRUE) {
        theta_minus <- theta - .01
        theta_sum <- theta + .01
        if (theta_sum >= 1.01) {
            theta_sum <- 1.00
        }
        if (theta_minus <= -1.01) {
            theta_minus <- -1.00
        }
        errors_minus <- errors_operator(list, list_index, constant, persistence, theta_minus)
        errors_sum <- errors_operator(list, list_index, constant, persistence, theta_sum)
        SSE_minus <- SSE_operator(errors_minus, length(errors_minus))
        SSE_sum <- SSE_operator(errors_sum, length(errors_sum))
        if (SSE_minus < SSE_sum) {
            if (SSE_minus < SSE) {
                theta_sum_possible <- FALSE
                return(theta_presicion(list, list_index, constant, persistence, SSE_minus, theta_minus, theta_minus_possible, theta_sum_possible))
            }   else {
            return(theta)
            }
        }  else if (SSE_sum < SSE_minus) {
            if (SSE_sum < SSE) {
                theta_minus_possible <- FALSE
                return(theta_presicion(list, list_index, constant, persistence, SSE_sum, theta_sum, theta_minus_possible, theta_sum_possible))
            }   else {
            return(theta)
            }
        }   else {
            return(theta)
        }
    } else if (theta_minus_possible == TRUE && theta_sum_possible == FALSE) {
        theta_minus <- theta - .01
        if (theta_minus <= -1.01) {
            theta_minus <- -1.00
        }
        errors <- errors_operator(list, list_index, constant, persistence, theta_minus)
        SSE_minus <- SSE_operator(errors, length(errors))
        if (SSE_minus < SSE) {
            return(theta_presicion(list, list_index, constant, persistence, SSE_minus, theta_minus, theta_minus_possible, theta_sum_possible))
        } else {
            return(theta)
        }
    } else {
        theta_sum <- theta + .01
        if (theta_sum >= 1.01) {
            theta_sum <- 1.00
        }
        errors <- errors_operator(list, list_index, constant, persistence, theta_sum)
        SSE_sum <- SSE_operator(errors, length(errors))
        if (SSE_sum < SSE) {
            return(theta_presicion(list, list_index, constant, persistence, SSE_sum, theta_sum, theta_minus_possible, theta_sum_possible))
        } else {
            return(theta)
        }
    }
}
#Verificathes that the correct option was seleted in modes
mode_verificator <- function(letter) {
    if (letter != "file" && letter != "manualy") {
        letter <- readline("Introduce please file for select the file option or manualy for the manualy option: ")
        return(mode_verificator(letter))
    }   else {
       return(letter)
    }
}
#Verificathes that the correct option was seleted in try again
N_Y_verificator <- function(letter) {
    if (letter != "N" && letter != "Y") {
        letter <- readline("Introduce please Y for yes or N for not: ")
        return(N_Y_verificator(letter))
    }   else {
       return(letter)
    }
}
#Start the function
ARIMA_interface()
