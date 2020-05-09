---
metaTitle: "MATLAB - Multithreading"
description: "Using parfor to parallelize a loop, When to use parfor, Executing commands in parallel using a Single Program, Multiple Data (SPMD) statement, Using the batch command to do various computations in parallel"
---

# Multithreading




## Using parfor to parallelize a loop


You can use [`parfor`](http://www.mathworks.com/help/distcomp/parfor.html) to execute the iterations of a loop in parallel:

Example:

```matlab
poolobj = parpool(2);       % Open a parallel pool with 2 workers 

s = 0;                      % Performing some parallel Computations
parfor i=0:9
    s = s + 1;
end
disp(s)                     % Outputs '10'

delete(poolobj);            % Close the parallel pool

```

Note: `parfor` cannot be nested directly. For `parfor` nesting use a function in fisrt `parfor` and add second `parfor` in that function.

Example:

```matlab
parfor i = 1:n
[op] = fun_name(ip);
end

function [op] = fun_name(ip)
parfor j = 1:length(ip)
% Some Computation
end

```



## When to use parfor


Basically, [`parfor`](http://mathworks.com/help/distcomp/parfor.html) is recommended in two cases: lots of iterations in your loop (i.e., like `1e10`), or if each iteration takes a very long time (e.g., `eig(magic(1e4))`). In the second case you might want to consider using [`spmd`](http://mathworks.com/help/distcomp/spmd.html) . The reason `parfor` is slower than a [`for`](http://mathworks.com/help/distcomp/for.html) loop for short ranges or fast iterations is the overhead needed to manage all workers correctly, as opposed to just doing the calculation.

Also a lot of functions have [implicit multi-threading built-in](http://mathworks.com/products/parallel-computing/builtin-parallel-support.html), making a `parfor` loop not more efficient, when using these functions, than a serial `for` loop, since all cores are already being used. `parfor` will actually be a detriment in this case, since it has the allocation overhead, whilst being as parallel as the function you are trying to use.

Consider the following example to see the behaviour of `for` as opposed to that of `parfor`. First open the parallel pool if you've not already done so:

```matlab
gcp; % Opens a parallel pool using your current settings

```

Then execute a couple of large loops:

```matlab
n = 1000; % Iteration number
EigenValues = cell(n,1); % Prepare to store the data
Time = zeros(n,1);
for ii = 1:n
tic
    EigenValues{ii,1} = eig(magic(1e3)); % Might want to lower the magic if it takes too long
Time(ii,1) = toc; % Collect time after each iteration
end

figure; % Create a plot of results
plot(1:n,t)
title 'Time per iteration'
ylabel 'Time [s]'
xlabel 'Iteration number[-]';

```

Then do the same with `parfor` instead of `for`. You will notice that the average time per iteration goes up. Do realise however that the `parfor` used all available workers, thus the total time (`sum(Time)`) has to be divided by the number of cores in your computer.

So, whilst the time to do each separate iteration goes up using `parfor` with respect to using `for`, the total time goes down considerably.



## Executing commands in parallel using a "Single Program, Multiple Data" (SPMD) statement


Unlike a parallel for-loop (`parfor`), which takes the iterations of a loop and distributes them among multiple threads, a single program, multiple data (`spmd`) statement takes a series of commands and distributes them to **all** the threads, so that each thread performs the command and stores the results.  Consider this:

```matlab
poolobj = parpool(2);    % open a parallel pool with two workers

spmd
    q = rand(3);         % each thread generates a unique 3x3 array of random numbers
end

q{1}             % q is called like a cell vector
q{2}             % the values stored in each thread may be accessed by their index

delete(poolobj)  % if the pool is closed, then the data in q will no longer be accessible

```

It is important to note that each thread may be accessed during the `spmd` block by its thread index (also called lab index, or `labindex`):

```matlab
poolobj = parpool(2);        % open a parallel pool with two workers

spmd
    q = rand(labindex + 1);  % each thread generates a unique array of random numbers
end

size(q{1})                   % the size of q{1} is 2x2
size(q{2})                   % the size of q{2} is 3x3

delete(poolobj)              % q is no longer accessible

```

In both examples, `q` is a [composite object](https://www.mathworks.com/help/distcomp/composite.composite.html), which may be initialized with the command `q = Composite()`.  It is important to note that composite objects are only accessible while the pool is running.



## Using the batch command to do various computations in parallel


To use multi-threading in MATLAB one can use the `batch` command. Note that you must have the Parallel Computing toolbox installed.

For a time-consuming script, for example,

```matlab
for ii=1:1e8
   A(ii)=sin(ii*2*pi/1e8);
end

```

to run it in batch mode one would use the following:

```matlab
job=batch("da")

```

which is enables MATLAB to run in batch mode and makes it possible to use MATLAB in the meantime to do other things, such as add more batch processes.

To retrieve the results after finishing the job and load the array `A` into the workspace:

```matlab
load(job, 'A')

```

Finally, open the "monitor job gui" from **Home** → **Environment** → **Parallel** → **Monitor jobs** and delete the job through:

```matlab
delete(job)

```

To load a function for batch processing, simply use this statement where `fcn` is the function name, `N` is number of output arrays and `x1`, `...`, `xn` are input arrays:

```

j=batch(fcn, N, {x1, x2, ..., xn})

```

