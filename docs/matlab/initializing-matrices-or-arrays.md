---
metaTitle: "MATLAB - Initializing Matrices or arrays"
description: "Creating a matrix of 0s, Creating a matrix of 1s, Creating an identity matrix"
---

# Initializing Matrices or arrays




## Creating a matrix of 0s


```matlab
z1 = zeros(5); % Create a 5-by-5 matrix of zeroes
z2 = zeros(2,3); % Create a 2-by-3 matrix

```



## Creating a matrix of 1s


```matlab
o1 = ones(5); % Create a 5-by-5 matrix of ones
o2 = ones(1,3); % Create a 1-by-3 matrix / vector of size 3

```



## Creating an identity matrix


```matlab
i1 = eye(3); % Create a 3-by-3 identity matrix
i2 = eye(5,6); % Create a 5-by-6 identity matrix

```



#### Syntax


- Z = zeros(sz,datatype,arraytype)
- X = ones(sz,datatype)
- I = eye(sz,datatype)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|sz|n (for an n x n matrix)
|sz|n, m (for an n x m matrix)
|sz|m,n,...,k (for an m-by-n-by-...-by-k matrix)
|datatype|'double' (default), 'single', 'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32', 'int64', or 'uint64'
|arraytype|'distributed'
|arraytype|'codistributed'
|arraytype|'gpuArray'



#### Remarks


These functions will create a matrix of doubles, by default.

