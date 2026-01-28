---
metaTitle: "MATLAB - Programming Utilities"
description: "Simple timer in MATLAB"
---

# Programming Utilities



## Simple timer in MATLAB


The following is a timer that fires at a fixed interval. It's timeout is defined by `Period` and it invokes a callback defined by `Timerfcn` upon timeout.

```matlab
t = timer;
t.TasksToExecute = Inf;
t.Period = 0.01; % timeout value in seconds
t.TimerFcn = @(myTimerObj, thisEvent)disp('hello'); % timer callback function
t.ExecutionMode = 'fixedRate';
start(t)
pause(inf);

```

