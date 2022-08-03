from unittest import result
import numpy as np
import pandas as pd

a = np.array([[0,3],[1,-1],[2,1],[5,2]])
b = np.array([[1,2,1,2],[4,1,-1,-4]])
a.dot(b)
print( np.trace(b.dot(a)))
print(np.trace(a))
print(np.trace(b))
print(len(a))

x = [[70, 1.8], [80, 1.9], [150, 1.7]]
def cal (w,h):
    return w / (h **2)

for i in x:
    w,h = i
    bmi = cal(h,w)

print(type(cal(65.0,1.76)))    
di =  {i for i in range(0, 5) if i % 3 == 0 or i % 3 != 1}
print(di)

class Person():
    def __init__(self, name, age):
        self.name = name
        self.age = age
    def get_person_info(self):
        return f"{self.name} is {self.age} years old."

for i in "abcd"[::-1]:
    print (i)     

memory = {}
def memoize_factorial(f):
    def inner(num):
        if num not in memory:
            memory[num] = f(num)
        return memory[num]
    return inner

@memoize_factorial
def facto(num):
    if num == 1:
        return 1
    else:
        return num * facto(num-1)
print(facto(3), type(facto(3)))    

def hide_name(pattern: str):
    def append_to_name(name):
        placeholder = []
        name_index = 0
        for i in range(2 * len(name)):
            if i % 2 == 0 :
                placeholder.append(name[name_index])
                name_index += 1
            else:
                placeholder.append(pattern)
        return "".join(placeholder)
    return append_to_name

hider = hide_name('&*#$@')
print(hider('10Ac'))

def display(**kwargs):
    for i in kwargs:
        print(kwargs[i])

display(name = "john", age=30, city= "mom")        
s1 = {3,4}
s2 = {1, 2}
s3 = set()
i = 0
j = 0
for i in s1:
    for j in s2:
        s3.add((i,j))
        i+=1
        j+=1
print(s3)        

def mk(x):
    def mk1():
        print("Decorated")
        x()
    return mk1
def mk2():
    print("Ordinary")

p = mk(mk2)
p()

def interger_generator (limit):
    n = 0
    while n < limit:
        n+=1
        yield n
result = interger_generator(10)
print(list(result),type(result))        

df = pd.array([])