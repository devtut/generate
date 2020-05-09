---
metaTitle: "MATLAB - Introduction to MEX API"
description: "Check number of inputs/outputs in a C++ MEX-file, Input a string, modify it in C, and output it, Passing a struct by field names, Pass a 3D matrix from MATLAB to C"
---

# Introduction to MEX API



## Check number of inputs/outputs in a C++ MEX-file


In this example we will write a basic program that checks the number of inputs and outputs passed to a MEX-function.

As a starting point, we need to create a C++ file implementing the "MEX gateway". This is the function executed when the file is called from MATLAB.

### testinputs.cpp

```matlab
// MathWorks provided header file
#include "mex.h"

// gateway function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // This function will error if number of inputs its not 3 or 4
    // This function will error if number of outputs is more than 1

    // Check inputs:
    if (nrhs < 3 || nrhs > 4) {
        mexErrMsgIdAndTxt("Testinputs:ErrorIdIn",
            "Invalid number of inputs to MEX file.");
    }

    // Check outputs:
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("Testinputs:ErrorIdOut",
                "Invalid number of outputs to MEX file.");
    }
}

```

First, we include the `mex.h` header which contains definitions of all the required functions and data types to work with the MEX API. Then we implement the function `mexFunction` as shown, where its signature must not change, independent of the inputs/outputs actually used. The function parameters are as follows:

- `nlhs`: Number of outputs requested.
- `*plhs[]`: Array containing all the outputs in MEX API format.
- `nrhs`: Number of inputs passed.
- `*prhs[]`: Array containing all the inputs in MEX API format.

Next, we check the number of inputs/outputs arguments, and if the validation fails, an error is thrown using `mexErrMsgIdAndTxt` function (it expects `somename:iD` format identifier, a simple "ID" won't work).

Once the file is compiled as `mex testinputs.cpp`, the function can be called in MATLAB as:

```matlab
>> testinputs(2,3)
Error using testinputs. Invalid number of inputs to MEX file.

>> testinputs(2,3,5)

>> [~,~] = testinputs(2,3,3)
Error using testinputs. Invalid number of outputs to MEX file.

```



## Input a string, modify it in C, and output it


In this example, we illustrate string manipulation in MATLAB MEX. We will create a MEX-function that accepts a string as input from MATLAB, copy the data into C-string, modify it and convert it back to `mxArray` returned to the MATLAB side.

The main objective of this example is to show how strings can be converted to C/C++ from MATLAB and vice versa.

### stringIO.cpp

```matlab
#include "mex.h"
#include <cstring>

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // check number of arguments
    if (nrhs != 1 || nlhs > 1) {
        mexErrMsgIdAndTxt("StringIO:WrongNumArgs", "Wrong number of arguments.");
    }

    // check if input is a string
    if (mxIsChar(prhs[0])) {
        mexErrMsgIdAndTxt("StringIO:TypeError", "Input is not a string");
    }

    // copy characters data from mxArray to a C-style string (null-terminated)
    char *str = mxArrayToString(prhs[0]);

    // manipulate the string in some way
    if (strcmp("theOneString", str) == 0) {
        str[0] = 'T';  // capitalize first letter
    } else {
        str[0] = ' ';  // do something else?
    }

    // return the new modified string
    plhs[0] = mxCreateString(str);

    // free allocated memory
    mxFree(str);
}

```

The relevant functions in this example are:

- `mxIsChar` to test if an `mxArray` is of `mxCHAR` type.
- `mxArrayToString` to copy the data of a `mxArray` string to a `char *` buffer.
- `mxCreateString` to create an `mxArray` string from a `char*`.

As a side note, if you only want to read the string, and not modify it, remember to declare it as `const char*` for speed and robustness.

Finally, once compiled we can call it from MATLAB as:

```matlab
>> mex stringIO.cpp

>> strOut = stringIO('theOneString')
strOut = 
TheOneString

>> strOut = stringIO('somethingelse')
strOut=
omethingelse

```



## Passing a struct by field names


This example illustrates how to read various-type struct entries from MATLAB, and pass it to C equivalent type variables.

While it is possible and easy to figure out from the example how to load fields by numbers, it is here achieved via comparing the field names to strings. Thus the struct fields can be addressed by their field names and variables in it can be read by C.

### structIn.c

```matlab
#include "mex.h"
#include <string.h> // strcmp


void mexFunction (int nlhs, mxArray *plhs[],
                  int nrhs, const mxArray *prhs[])
{
  // helpers
  double* double_ptr;
  unsigned int i; // loop variable
  
  // to be read variables
  bool optimal;
  int randomseed;
  unsigned int desiredNodes;

  if (!mxIsStruct(prhs[0])) {
    mexErrMsgTxt("First argument has to be a parameter struct!");
  }
  for (i=0; i<mxGetNumberOfFields(prhs[0]); i++) {
    if (0==strcmp(mxGetFieldNameByNumber(prhs[0],i),"randomseed")) {
      mxArray *p = mxGetFieldByNumber(prhs[0],0,i);
      randomseed = *mxGetPr(p);
    }
    if (0==strcmp(mxGetFieldNameByNumber(prhs[0],i),"optimal")) {
      mxArray *p = mxGetFieldByNumber(prhs[0],0,i);
      optimal = (bool)*mxGetPr(p);
    }
    if (0==strcmp(mxGetFieldNameByNumber(prhs[0],i),"numNodes")) {
      mxArray *p = mxGetFieldByNumber(prhs[0],0,i);
      desiredNodes = *mxGetPr(p);
    }
  }
}

```

The loop over `i` runs over every field in the given struct, while the `if(0==strcmp)`-parts compare the matlab field's name to the given string. If it is a match, the corresponding value is extracted to a C variable.



## Pass a 3D matrix from MATLAB to C


In this example we illustrate how to take a double real-type 3D matrix from MATLAB, and pass it to a C `double*` array.

The main objectives of this example are showing how to obtain data from MATLAB MEX arrays and to highlight some small details in matrix storage and handling.

### matrixIn.cpp

```matlab
#include "mex.h"

void mexFunction(int  nlhs , mxArray *plhs[],
        int nrhs, mxArray const *prhs[]){
   // check amount of inputs
   if (nrhs!=1) {
        mexErrMsgIdAndTxt("matrixIn:InvalidInput", "Invalid number of inputs to MEX file.");
    }
   
   // check type of input
   if( !mxIsDouble(prhs[0]) || mxIsComplex(prhs[0])){
        mexErrMsgIdAndTxt("matrixIn:InvalidType",  "Input matrix must be a double, non-complex array.");
   }
   
   // extract the data
   double const * const matrixAux= static_cast<double const *>(mxGetData(prhs[0]));

   // Get matrix size
   const mwSize *sizeInputMatrix= mxGetDimensions(prhs[0]);

   // allocate array in C. Note: its 1D array, not 3D even if our input is 3D
   double*  matrixInC= (double*)malloc(sizeInputMatrix[0] *sizeInputMatrix[1] *sizeInputMatrix[2]* sizeof(double));

  
   // MATLAB is column major, not row major (as C). We need to reorder the numbers
   // Basically permutes dimensions   

   // NOTE: the ordering of the loops is optimized for fastest memory access! 
   // This improves the speed in about 300% 

    const int size0 = sizeInputMatrix[0]; // Const makes compiler optimization kick in
    const int size1 = sizeInputMatrix[1];
    const int size2 = sizeInputMatrix[2];
    
    for (int j = 0; j < size2; j++)
    {
        int jOffset = j*size0*size1; // this saves re-computation time
        for (int k = 0; k < size0; k++)
        {
            int kOffset = k*size1; // this saves re-computation time
            for (int i = 0; i < size1; i++)
            {
                int iOffset = i*size0; 
                matrixInC[i + jOffset + kOffset] = matrixAux[iOffset + jOffset + k];
            }
        }
    }

    // we are done!

    // Use your C matrix here

    // free memory
    free(matrixInC);
    return;
}

```

The relevant concepts to be aware of:

<li>
MATLAB matrices are all 1D in memory, no matter how many dimensions they have when used in MATLAB. This is also true for most (if not all) main matrix representation in C/C++ libraries, as allows optimization and faster execution.
</li>
<li>
You need to explicitly copy matrices from MATLAB to C in a loop.
</li>
<li>
MATLAB matrices are stored in column major order, as in Fortran, but C/C++ and most modern languages are row major. It is important to permute the input matrix , or else the data will look completely different.
</li>

The relevant function in this example are:

- `mxIsDouble` checks if input is `double` type.
- `mxIsComplex` checks if input is real or imaginary.
- `mxGetData` returns a pointer to the real data in the input array. `NULL` if there is no real data.
- `mxGetDimensions` returns an pointer to a `mwSize` array, with the size of the dimension in each index.

