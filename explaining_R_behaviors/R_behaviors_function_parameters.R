# Leslie Huang
# 2/15/18

# What order should default and optional parameters in functions be in? When do they have to be named?

# Take this simple function that prints the sum of x,y by default (and does nothing if z = FALSE)
my_function <- function(x, y, z = TRUE) {
  if (z) {
    print(x - y)
  }
}

# x and y are required arguments. z is optional (i.e. it has a default)
my_function(10, 2) 
my_function(10, 2, z = TRUE) # same result; z is not required here

# You don't have to name x, y, or z when you pass them to the function in this order:
my_function (10, 2, FALSE) # here we are overriding the default value for z

# Did you want to pass y to the function first?
my_function(y = 3, x = 7)
my_function(y = 3, 7) # and you can even name just one of the required arguments

# Now, take this simple (bad) function:
ambiguous_function <- function(x, z = TRUE, y) {
  if (z) {
    print(x - y)
  }
}
# The only difference was the order that the parameters are listed

# It works when we name the parameters
ambiguous_function(1, z = TRUE, 10)
ambiguous_function(1, FALSE, 5) # should return nothing

# Why does this throw an error?
ambiguous_function(1, 5)

# ...while this does not:
my_function(10, 5)

# ...and this is evaluated correctly as well
ambiguous_function(z = FALSE, y = 4, x = 1)

# in the function call ambiguous_function(1, 5) , 2 unidentified arguments were passed in, and R doesn't know that you want the default value for z and you want to pass 5 to y
# However you can pass in the arguments in any order if they are named:
ambiguous_function(z = FALSE, y = 4, x = 1)

# The following is also OK because you have named y, and z takes on its default value
ambiguous_function(1, y = 5)
# You can pass in 1 instead of x = 1 because the argument is in the correct position

# What have we learned?

# When writing functions, you should always put non-optional parameters before optional parameters!

# When calling functions, identify parameters if you want to pass them to the function out of order!