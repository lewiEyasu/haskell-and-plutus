#!/bin/bash  
git add .   
echo "Add committ"  
# Take input without defining variable    
read  
# Print the input value
git commit -m " $REPLY"
git push origin -u main

