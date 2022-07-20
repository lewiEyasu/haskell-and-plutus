#!/bin/bash  
git add .   
echo "Task Number"  
# Take input without defining variable    
read TASKNUMBER
echo "File"
# Take input without defining variable
read FILENAME


# Print the input value
git commit -m "solve task $TASKNUMBER in Higher_order_functions $FILENAME"
git push origin -u main

