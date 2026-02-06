# Define a function
def fib(x)
    if x < 3 then
        1
    else
        fib(x-1) + fib(x-2);

# Call it
fib(10);