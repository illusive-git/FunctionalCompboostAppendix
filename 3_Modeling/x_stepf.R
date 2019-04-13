stepf = function(input_x, input_y, fun, ... ){
    
    if(!is.numeric(input_x) && is.numeric(input_y)){
        stop("stepf needs numeric inputs")
    }

    if(length(input_x) != length(input_y)){
        stop("x and y must be of same length.")
    }

    if(tryCatch( {fun(input_y, ... ); FALSE}, error = function(x) return(TRUE))){
        stop("Data and function not compatible")
    }

    if(is.unsorted(input_x)){
        warning("X was not sorted.")
        input_x = input_x[order(input_x)]
        input_y = input_y[order(input_x)]
    } 

    # Construct object
    stepf.object = structure(list(x = input_x, y = input_y, len = length(input_x), fun = fun,
                                  fun.args = list(...)),
                             class = "stepf")
}

predict.stepf = function(newdata,stepf.object){
    prediction = vapply(X = newdata, FUN.VALUE = 0, FUN = function(x,stepfun.object){
      # find closest upper limit in data to newdata
      above = head(which(x < stepf.object$x), n=1) 
     
      # value is above all others
      if(length(above) == 0){
          return(tail(stepf.object$y, n=1))
      } 
      
      # value is below all others
      if(above == 1){
          return(stepf.object$y[1])
      }
      
      # value is between two values
      pred = stepf.object$fun(c(stepf.object$y[above-1],stepf.object$y[above]))
    })
    return(prediction)
} 

#x1 = c(1,5,10)
#y1 = c(4,-2,99)

#testf = stepf(input_x = x1,input_y = y1,fun = mean)
#predict(testf, newdata = c(0.5,2,5,15))

#testf = stepf(input_x = x1,input_y = y1,fun = median)
#predict(testf, newdata = c(0.5,2,5,15))


