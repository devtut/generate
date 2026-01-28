---
metaTitle: "MATLAB - Reading large files"
description: "textscan, Date and time strings to numeric array fast"
---

# Reading large files




## textscan


Assume you have formatted data in a large text file or string, e.g.

```matlab
Data,2015-09-16,15:41:52;781,780.000000,0.0034,2.2345
Data,2015-09-16,15:41:52;791,790.000000,0.1255,96.5948
Data,2015-09-16,15:41:52;801,800.000000,1.5123,0.0043

```

one may use `textscan` to read this quite fast. To do so, get a file identifier of the text file with `fopen`:

```matlab
fid = fopen('path/to/myfile');

```

Assume for the data in this example, we want to ignore the first column "Data", read the date and time as strings, and read the rest of the columns as doubles, i.e.

```

Data  ,  2015-09-16  , 15:41:52;801  , 800.000000  , 1.5123  ,  0.0043
ignore      string         string         double      double     double

```

To do this, call:

```matlab
data = textscan(fid,'%*s %s %s %f %f %f','Delimiter',',');

```

The asterisk in `%*s` means "ignore this column". `%s` means "interpret as a string". `%f` means "interpret as doubles (floats)". Finally, `'Delimiter',','` states that all commas should be interpreted as the delimiter between each column.

To sum up:

```matlab
fid = fopen('path/to/myfile');
data = textscan(fid,'%*s %s %s %f %f %f','Delimiter',',');

```

`data` now contains a cell array with each column in a cell.



## Date and time strings to numeric array fast


Converting date and time strings to numeric arrays can be done with `datenum`, though it may take as much as half the time of reading a large data file.

Consider the data in example **Textscan**. By, again, using textscan and interpret date and time as integers, they can rapidly be converted into a numeric array.

I.e. a line in the example data would be interpreted as:

```

Data , 2015  - 09  -  16  ,  15  :  41  :  52  ;  801 , 800.000000 , 1.5123 , 0.0043
ignore double double double double double double double    double     double   double

```

which will be read as:

```matlab
fid = fopen('path/to/myfile');
data = textscan(fid,'%*s %f %f %f %f %f %f %f %f %f %f','Delimiter',',-:;');
fclose(fid);

```

Now:

```matlab
y = data{1};          % year
m = data{2};          % month
d = data{3};          % day
H = data{4};          % hours
M = data{5};          % minutes
S = data{6};          % seconds
F = data{7};          % milliseconds

% Translation from month to days
ms = [0,31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]; 

n = length(y);        % Number of elements
Time = zeros(n,1);    % Declare numeric time array

% Algorithm for calculating numeric time array
for k = 1:n
    Time(k) = y(k)*365 + ms(m(k)) + d(k) + floor(y(k)/4)...
              - floor(y(k)/100) + floor(y(k)/400) + (mod(y(k),4)~=0)...
              - (mod(y(k),100)~=0) + (mod(y(k),400)~=0)...
              + (H(k)*3600 + M(k)*60 + S(k) + F(k)/1000)/86400 + 1;
end

```

Using `datenum` on 566,678 elements required 6.626570 seconds, whilst the method above required 0.048334 seconds, i.e. 0.73% of the time for `datenum` or ~137 times faster.

