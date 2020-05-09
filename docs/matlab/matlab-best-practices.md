---
metaTitle: "MATLAB - MATLAB Best Practices"
description: "Indent code properly, Keep lines short, Use assert, Avoid loops, Block Comment Operator, Create Unique Name for Temporary File, Use validateattributes"
---

# MATLAB Best Practices



## Indent code properly


Proper indentation gives not only the aesthetic look but also increases the readability of the code.

For example, consider the following code:

```matlab
%no need to understand the code, just give it a look
n = 2;
bf = false;
while n>1
for ii = 1:n
for jj = 1:n
if ii+jj>30
bf = true;
break
end
end
if bf
break 
end
end
if bf
break 
end
n = n + 1;
end

```

As you can see, you need to give a careful look to see which loop and `if` statements are ending where.
<br> With smart indentation, you'll get this look:

```matlab
n = 2;
bf = false;
while n>1
    for ii = 1:n
        for jj = 1:n
            if ii+jj>30
                bf = true;
                break
            end
        end
        if bf
            break
        end
    end
    if bf
        break
    end
    n = n + 1;
end

```

This clearly indicates the starting and ending of loops/`if` statement.

You can do smart indentation by:
<br> **•** selecting all your code (<kbd>Ctrl</kbd>+<kbd>A</kbd>)
<br> **•** and then pressing <kbd>Ctrl</kbd>+<kbd>I</kbd> or clicking [<img src="http://i.stack.imgur.com/PVQ0b.jpg" alt="SmartIndentIcon" />](http://i.stack.imgur.com/PVQ0b.jpg) from edit bar.
[<img src="http://i.stack.imgur.com/8N7uJ.jpg" alt="Editor" />](http://i.stack.imgur.com/8N7uJ.jpg)



## Keep lines short


Use the continuation character (ellipsis) `...` to continue long statement.

**Example:**

```matlab
MyFunc( parameter1,parameter2,parameter3,parameter4, parameter5, parameter6,parameter7, parameter8, parameter9)

```

can be replaced by:

```matlab
MyFunc( parameter1, ...
        parameter2, ...
        parameter3, ...
        parameter4, ...
        parameter5, ...
        parameter6, ...
        parameter7, ...
        parameter8, ...
        parameter9)

```



## Use assert


Matlab allows some very trivial mistakes to go by silently, which might cause an error to be raised much later in the run - making debugging hard.
If you **assume** something about your variables, **validate** it.

```matlab
function out1 = get_cell_value_at_index(scalar1,cell2)
assert(isscalar(scalar1),'1st input must be a scalar')
assert(iscell(cell2),'2nd input must be a cell array')

assert(numel(cell2) >= scalar1),'2nd input must have more elements than the value of the 1st input')
assert(~isempty(cell2{scalar1}),'2nd input at location is empty')

out1 = cell2{scalar1};

```



## Avoid loops


Most of the time, loops are computationally expensive with Matlab. Your code will be orders of magnitudes faster if you use vectorization. It also often makes your code more modular, easily modifiable, and easier to debug. The major downside is that you have to take time to plan the data structures, and dimension errors are easier to come by.

### Examples

Don't write

```matlab
for t=0:0.1:2*pi
    R(end+1)=cos(t);
end

```

but

```matlab
t=0:0.1:2*pi;
R=cos(t)

```

Don't write

```matlab
for i=1:n
    for j=1:m
        c(i,j)=a(i)+2*b(j);
    end
end

```

But something similar to

```matlab
c=repmat(a.',1,m)+2*repmat(b,n,1)

```

For more details, see [vectorization](https://stackoverflow.com/documentation/matlab/750/vectorization)



## Block Comment Operator


It is a good practice to add comments that describe the code. It is helpful for others and even for the coder when returned later.
A single line can be commented using the `%` symbol or using the shortkey `Ctrl+R`.
To uncomment a previously commented line remove the `%` symbol or use shortkey `Crtl+T`.

While commenting a block of code can be done by adding a `%` symbol at the beginning of each line, newer versions of MATLAB (after 2015a) let you use the **Block Comment Operator** `%{ code %}`.
This operator increases the readability of the code. It can be used for both code commenting and function help documentation.
The Block can be **folded** and **unfolded** to increase the readability of the code.

[<img src="https://i.stack.imgur.com/x8MtW.png" alt="enter image description here" />](https://i.stack.imgur.com/x8MtW.png)

As it can be seen the `%{` and `%}` operators must appear alone on the lines. Do not include any other text on these lines.

```matlab
function y = myFunction(x)
%{
myFunction  Binary Singleton Expansion Function
y = myFunction(x) applies the element-by-element binary operation
specified by the function handle FUNC to arrays A and B, with implicit
expansion enabled.
%}

%%  Compute z(x, y) = x.*sin(y) on a grid:
% x = 1:10;
y = x.';

%{
z = zeros(numel(x),numel(y));
for ii=1:numel(x)
    for jj=1:numel(y)
        z(ii,jj) = x(ii)*sin(y(jj));
    end
end
%}

z = bsxfun(@(x, y) x.*sin(y), x, y);
y = y + z;

end

```



## Create Unique Name for Temporary File


While coding a script or a function, it can be the case that one or more than one
temporary file be needed in order to, for example, store some data.

In order to avoid overwriting an existing file or to shadow a MATLAB function
the [tempname](https://uk.mathworks.com/help/matlab/ref/tempname.html) function
can be used to generate a **unique name** for a temporary file in the system temporary folder.

```matlab
my_temp_file=tempname

```

The filename is generated without the extension; it can be added by concatenating
the desired extension to the name generated by `tempname`

```matlab
my_temp_file_with_ext=[tempname '.txt']

```

The locaton of the system temporary folder can be retrieved by caling the
[tempdir](https://uk.mathworks.com/help/matlab/ref/tempdir.html) function.

If, during the execution of the function / script, the temporary file is no longer
needed, it can be deleted by using the function [delete](https://uk.mathworks.com/help/matlab/ref/delete.html)

Since `delete` does not ask for confirmation, it might be useful to set `on` the
option to move the file to be deleted in the `recycle` folder.

This can be done by using the function [recycle](https://uk.mathworks.com/help/matlab/ref/recycle.html) this
way:

```matlab
recycle('on')

```

In the following example, a possible usage of the functions `tempname`, `delete` and
`recycle` is proposed.

```matlab
%
% Create some example data
%
theta=0:.1:2*pi;
x=cos(theta);
y=sin(theta);
%
% Generate the temporary filename
%
my_temp_file=[tempname '.mat'];
%
% Split the filename (path, name, extension) and display them in a message box
[tmp_file_path,tmp_file_name, tmp_file_ext]=fileparts(my_temp_file)
uiwait(msgbox(sprintf('Path= %s\nName= %s\nExt= %s', ...
              tmp_file_path,tmp_file_name,tmp_file_ext),'TEMPORARY FILE'))
%
% Save the varaibles in a temporary file
%
save(my_temp_file,'x','y','theta')
%
% Load the varaibles from the temporary file
%
load(my_temp_file)
%
% Set the reclycle option on
%
recycle('on')
%
% Delete the temporary file
%
delete(my_temp_file)

```

**Caveat**

The temporary filename is generated by using the `java.util.UUID.randomUUID` method
([randomUUID](https://docs.oracle.com/javase/7/docs/api/java/util/UUID.html)).

If MATLAB is run without JVM, the temporary filename is generated by using<br />
`matlab.internal.timing.timing` based on the CPU counter and time. In this case
the temporary filename is not guaranteed to be unique.



## Use validateattributes


The function [validateattributes](https://uk.mathworks.com/help/matlab/ref/validateattributes.html)
can be used to validate an array against a set of specifications

It can be therefore used to validate the input provided to a function.

In the following example, the function `test_validateattributes` requires three
input

`function test_validateattributes(input_1,input_2,input_3)`

The input specification are:

<li>
array_1:
<ul>
- class: double
- size: [3,2]
- values: elements must be not NaN

char_array:

- class: char
- value: the string must not be empty

array_3

- class: double
- size: [5 1]
- values: elements must be real

To validate the three input, the function `validateattributes` can be called
with the following syntax:

```matlab
validateattributes(A,classes,attributes,funcName,varName,argIndex)

```

where:

- `A` is the array to be vaidated
- `classes`: is the `type` of the array (e. g. `single`, `double`, `logical`)
<li>`attributes`: are the atrributes the input array has to match (e. g. `[3,2], size`
to specify the size of the array, `nonnan` to specify that the array shall not have NaN values)</li>
<li>`funcName`: is the name of the function in which the validation occurs.
This argument is used in the generation of the error message (if any)</li>
- `varName`: is the name of the array under validation. This argument is used in the generation of the error message (if any)
<li>`argIndex`: is the position of the inpurt array in the list of input.
This argument is used in the generation of the error message (if any)</li>

In case one or more than one input does not match the specification, an error
message is generated.

In case of more than one invalid input, the validation stops when the first mismatch is found.

This is `function test_validateattributes` in which the input validation
has been implemented.

Since the function requires three input, a first check on the number of input
provided is performed using the fnction [nargin](https://uk.mathworks.com/help/matlab/ref/nargin.html).

```matlab
function test_validateattributes(array_1,char_array_1,array_3)
%
% Check for the number of expected input: if the number of input is less
% than the require, the function exits with an error message
%
if(nargin ~= 3)
   error('Error: TEST_VALIDATEATTRIBUTES requires 3 input, found %d',nargin)
end
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of the expected characteristics of the first input %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% INPUT #1 name (only used in the generation of the error message)
%
input_1_name='array_1';
%
% INPUT #1 position (only used in the generation of the error message)
%
input_1_position=1;
%
% Expected CLASS of the first input MUST BE "double"
%
input_1_class={'double'};
%
% Expected ATTRIBUTES of the first input
%   SIZE: MUST BE [3,2]
%
input_1_size_attribute='size';
input_1_size=[3,2];
%
%   VALUE CHECK: the element MUST BE NOT NaN
%
input_1_value_type='nonnan';
%
% Build the INPUT 1 attributes
%
input_1_attributes={input_1_size_attribute,input_1_size, ...
                    input_1_value_type};
%
% CHECK THE VALIDITY OF THE FIRST INPUT
%
validateattributes(array_1, ...
                   input_1_class,input_1_attributes,'', ...
                   input_1_name,input_1_position);

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of the expected characteristics of the second input %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% INPUT #1 name (only used in the generation of the error message)
%
input_2_name='char_array_1';
%
% INPUT #2 position (only used in the generation of the error message)
%
input_2_position=2;
%
% Expected CLASS of the first input MUST BE "string"
%
input_2_class={'char'};
%
%   VALUE CHECK: the element must be not NaN
%
input_2_size_attribute='nonempty';
%
% Build the INPUT 2 attributes
%
input_2_attributes={input_2_size_attribute};
%
% CHECK THE VALIDITY OF THE SECOND INPUT
%
validateattributes(char_array_1, ...
                   input_2_class,input_2_attributes,'', ...
                   input_2_name,input_2_position);
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of the expected characteristics of the third input %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% INPUT #3 name (only used in the generation of the error message)
%
input_3_name='array_3';
%
% INPUT #3 position (only used in the generation of the error message)
%
input_3_position=3;
%
% Expected CLASS of the first input MUST BE "double"
%
input_3_class={'double'};
%
% Expected ATTRIBUTES of the first input
%   SIZE: must be [5]
input_3_size_attribute='size';
input_3_size=[5 1];
%   VALUE CHECK: the elements must be real
input_3_value_type='real';
%
% Build the INPUT 3 attributes
%
input_3_attributes={input_3_size_attribute,input_3_size, ...
                    input_3_value_type};
%
% CHECK THE VALIDITY OF THE FIRST INPUT
%
validateattributes(array_3, ...
                   input_3_class,input_3_attributes,'', ...
                   input_3_name,input_3_position);

disp('All the three input are OK')

```

The following script can be used to test the implementation of the validation procedure.

It generate the three input required and, randomly, it makes them not valid.

```matlab
%
% Generate the first input
%
n_rows=randi([2 3],1);
n_cols=2;
input_1=randi([20 30],n_rows,n_cols);
%
% Generate the second input
%
if(rand > 0.5)
   input_2='This is a string';
else
   input_2='';
end
%
% Generate the third input
%
input_3=acos(rand(5,1)*1.2);
%
% Call the test_validateattributes function with the above generated input
%
input_1
input_2
input_3
%
test_validateattributes(input_1,input_2,input_3)

```

These are a couple of examples of wrong input detected by the `validateattributes` function:

Wrong input

```matlab
input_1 =

    23    22
    26    28

input_2 =

     ''

input_3 =

   0.0000 + 0.4455i
   1.2420 + 0.0000i
   0.4063 + 0.0000i
   1.3424 + 0.0000i
   1.2186 + 0.0000i

Error using test_validateattributes (line 44)
Expected input number 1, array_1, to be of size 3x2 when it is actually
size 2x2.

```

Wrong input

```matlab
input_1 =

    22    24
    21    25
    26    27

input_2 =

This is a string

input_3 =

   1.1371 + 0.0000i
   0.6528 + 0.0000i
   1.0479 + 0.0000i
   0.0000 + 0.1435i
   0.0316 + 0.0000i

Error using test_validateattributes (line 109)
Expected input number 3, array_3, to be real.

```

Valid input

```matlab
input_1 =

    20    25
    25    28
    24    23

input_2 =

This is a string

input_3 =

    0.9696
    1.5279
    1.3581
    0.5234
    0.9665

All the three input are OK

```



#### Remarks


This topic displays best practices that the community has learned over time.

