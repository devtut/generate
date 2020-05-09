---
metaTitle: "MATLAB - MATLAB User Interfaces"
description: "Passing Data Around User Interface, Making a button in your UI that pauses callback execution, Passing data around using the handles structure, Performance Issues when Passing Data Around User Interface"
---

# MATLAB User Interfaces




## Passing Data Around User Interface


Most advanced user interfaces require the user to be able to pass information between the various functions which make up a user interface. MATLAB has a number of different methods to do so.

### [`guidata`](http://www.mathworks.com/help/matlab/ref/guidata.html)

MATLAB's own [GUI Development Environment (GUIDE)](http://www.mathworks.com/help/matlab/creating_guis/guide-tools-summary.html) prefers to use a `struct` named `handles` to pass data between callbacks. This `struct` contains all of the graphics handles to the various UI components as well as user-specified data. If you aren't using a GUIDE-created callback which automatically passes `handles`, you can retrieve the current value using [`guidata`](http://www.mathworks.com/help/matlab/ref/guidata.html)

```matlab
% hObject is a graphics handle to any UI component in your GUI
handles = guidata(hObject);

```

If you want to modify a value stored in this data structure, you can modify but then you must store it back within the `hObject` for the changes to be visible by other callbacks. You can store it by specifying a second input argument to [`guidata`](http://www.mathworks.com/help/matlab/ref/guidata.html).

```matlab
% Update the value
handles.myValue = 2;

% Save changes
guidata(hObject, handles)

```

The value of `hObject` doesn't matter as long as it is a UI component **within the same `figure`** because ultimately the data is stored within the figure containing `hObject`.

**Best for:**

<li>Storing the `handles` structure, in which you can store all the
handles of your GUI components.</li>
- Storing "small" other variables which need to be accessed by most callbacks.

**Not recommended for**:

<li>Storing large variables which do not have to be accessed by all
callbacks and sub-functions (use `setappdata`/`getappdata` for
these).</li>

### [`setappdata`](http://www.mathworks.com/help/matlab/ref/setappdata.html)/[`getappdata`](http://www.mathworks.com/help/matlab/ref/getappdata.html)

Similar to the `guidata` approach, you can use [`setappdata`](http://www.mathworks.com/help/matlab/ref/setappdata.html) and [`getappdata`](http://www.mathworks.com/help/matlab/ref/getappdata.html) to store and retrieve values from within a graphics handle. The advantage of using these methods is that you can retrieve **only the value you want** rather than an entire `struct` containing **all** stored data. It is similar to a key/value store.

To store data within a graphics object

```matlab
% Create some data you would like to store
myvalue = 2

% Store it using the key 'mykey'
setappdata(hObject, 'mykey', myvalue)

```

And to retrieve that same value from within a different callback

```matlab
value = getappdata(hObject, 'mykey');

```

**Note:** If no value was stored prior to calling `getappdata`, it will return an empty array (`[]`).

Similar to `guidata`, the data is stored in the figure that contains `hObject`.

**Best for:**

<li>Storing large variables which do not have to be accessed by all
callbacks and sub-functions.</li>

### [`UserData`](http://uk.mathworks.com/help/matlab/creating_guis/share-data-among-callbacks.html#bt9p4qp)

Every graphics handle has a special property, `UserData` which can contain any data you wish. It could contain a cell array, a `struct`, or even a scalar. You can take advantage of this property and store any data you wish to be associated with a given graphics handle in this field. You can save and retrieve the value using the standard `get`/`set` methods for graphics objects or dot notation if you're using R2014b or newer.

```matlab
% Create some data to store
mydata = {1, 2, 3};

% Store it within the UserData property
set(hObject, 'UserData', mydata)

% Of if you're using R2014b or newer:
% hObject.UserData = mydata;

```

Then from within another callback, you can retrieve this data:

```matlab
their_data = get(hObject, 'UserData');

% Or if you're using R2014b or newer:
% their_data = hObject.UserData;

```

**Best for:**

- Storing variables with a limited scope (variables which are likely to be used only by the object in which they are stored, or objects having a direct relationship to it).

### [Nested Functions](http://uk.mathworks.com/help/matlab/creating_guis/share-data-among-callbacks.html#bt9p4s2)

In MATLAB, a nested function can read and modify any variable defined in the parent function. In this way, if you specify a callback to be a nested function, it can retrieve and modify any data stored in the main function.

```matlab
function mygui()    
    hButton = uicontrol('String', 'Click Me', 'Callback', @callback);

    % Create a counter to keep track of the number of times the button is clicked
    nClicks = 0;

    % Callback function is nested and can therefore read and modify nClicks
    function callback(source, event)
        % Increment the number of clicks
        nClicks = nClicks + 1;

        % Print the number of clicks so far
        fprintf('Number of clicks: %d\n', nClicks);
    end
end

```

**Best for:**

- Small, simple GUIs. (for quick prototyping, to not have to implement the `guidata` and/or `set/getappdata` methods).

**Not recommended for**:

<li>
Medium, large or complex GUIs.
</li>
<li>
GUI created with `GUIDE`.
</li>

### Explicit input arguments

If you need to send data to a callback function and don't need to modify the data within the callback, you can always consider passing the data to the callback using a carefully crafted callback definition.

You could use an anonymous function which adds inputs

```matlab
% Create some data to send to mycallback
data = [1, 2, 3];

% Pass data as a third input to mycallback
set(hObject, 'Callback', @(source, event)mycallback(source, event, data))

```

Or you could use the cell array syntax to specify a callback, again specifying additional inputs.

```matlab
set(hObject, 'Callback', {@mycallback, data})

```

**Best for:**

- When the callback needs `data` to perform some operations but the `data` variable does not need to be modified and saved in a new state.



## Making a button in your UI that pauses callback execution


Sometimes we'd like to pause code execution to inspect the state of the application (see [Debugging](http://stackoverflow.com/documentation/matlab/1045/debugging#t=201702221619471013666)). When running code through the MATLAB editor, this can be done using the "Pause" button in the UI or by pressing <kbd>Ctrl</kbd>+<kbd>c</kbd> (on Windows). However, when a computation was initiated from a GUI (via the callback of some `uicontrol`), this method does not work anymore, and the callback should be **interrupted** via another callback. Below is a demonstration of this principle:

```matlab
function interruptibleUI
dbclear in interruptibleUI % reset breakpoints in this file
figure('Position',[400,500,329,160]); 

uicontrol('Style', 'pushbutton',...
          'String', 'Compute',...
          'Position', [24 55 131 63],...
          'Callback', @longComputation,...
          'Interruptible','on'); % 'on' by default anyway
      
uicontrol('Style', 'pushbutton',...
          'String', 'Pause #1',...
          'Position', [180 87 131 63],...
          'Callback', @interrupt1);       

uicontrol('Style', 'pushbutton',...
          'String', 'Pause #2',...
          'Position', [180 12 131 63],...
          'Callback', @interrupt2);    

end

function longComputation(src,event)
  superSecretVar = rand(1);
  pause(15);
  print('done!'); % we'll use this to determine if code kept running "in the background".
end

function interrupt1(src,event) % depending on where you want to stop
  dbstop in interruptibleUI at 27 % will stop after print('done!');
  dbstop in interruptibleUI at 32 % will stop after **this** line.
end

function interrupt2(src,event) % method 2
  keyboard;
  dbup; % this will need to be executed manually once the code stops on the previous line.
end

```

To make sure you understand this example do the following:

1. Paste the above code into a new file called and save it as `interruptibleUI.m`, such that the code starts on the **very first line** of the file (this is important for the 1st method to work).
1. Run the script.
1. Click <kbd>Compute</kbd> and shortly afterwards click either <kbd>Pause #1</kbd> or on <kbd>Pause #2</kbd>.
1. Make sure you can find the value of `superSecretVar`.



## Passing data around using the "handles" structure


This is an example of a basic GUI with two buttons that change a value stored in the GUI's `handles` structure.

```matlab
function gui_passing_data()
    % A basic GUI with two buttons to show a simple use of the 'handles'
    % structure in GUI building

    %  Create a new figure.
    f = figure();

    % Retrieve the handles structure
    handles = guidata(f);

    % Store the figure handle
    handles.figure = f;

    % Create an edit box and two buttons (plus and minus), 
    % and store their handles for future use
    handles.hedit  = uicontrol('Style','edit','Position',[10,200,60,20] , 'Enable', 'Inactive');

    handles.hbutton_plus  = uicontrol('Style','pushbutton','String','+',...
               'Position',[80,200,60,20] , 'Callback' , @ButtonPress);

    handles.hbutton_minus  = uicontrol('Style','pushbutton','String','-',...
               'Position',[150,200,60,20] , 'Callback' , @ButtonPress);

    % Define an initial value, store it in the handles structure and show
    % it in the Edit box
    handles.value = 1;
    set(handles.hedit , 'String' , num2str(handles.value))

    % Store handles
    guidata(f, handles);



function  ButtonPress(hObject, eventdata)
    % A button was pressed
    % Retrieve the handles
    handles = guidata(hObject);

    % Determine which button was pressed; hObject is the calling object
    switch(get(hObject , 'String'))
        case '+'
            % Add 1 to the value
            handles.value = handles.value + 1;
            set(handles.hedit , 'String', num2str(handles.value))
        case '-'
            % Substract 1 from the value
            handles.value = handles.value - 1;
    end

    % Display the new value
    set(handles.hedit , 'String', num2str(handles.value))

    % Store handles
    guidata(hObject, handles);

```

To test the example, save it in a file called `gui_passing_data.m` and launch it with F5.
Please note that in such a simple case, you would not even need to store the value in the handles structure because you could directly access it from the edit box's `String` property.



## Performance Issues when Passing Data Around User Interface


Two main techniques allow passing data between GUI functions and Callbacks: setappdata/getappdata and guidata ([read more about it](http://uk.mathworks.com/help/matlab/creating_guis/share-data-among-callbacks.html)). The former should be used for larger variables as it is more time efficient. The following example tests the two methods' efficiency.

A GUI with a simple button is created and a large variable (10000x10000 double) is stored both with guidata and with setappdata. The button reloads and stores back the variable using the two methods while timing their execution. The running time and percentage improvement using setappdata are displayed in the command window.

```matlab
function gui_passing_data_performance()
    % A basic GUI with a button to show performance difference between
    % guidata and setappdata

    %  Create a new figure.
    f = figure('Units' , 'normalized');

    % Retrieve the handles structure
    handles = guidata(f);

    % Store the figure handle
    handles.figure = f;

    handles.hbutton = uicontrol('Style','pushbutton','String','Calculate','units','normalized',...
               'Position',[0.4 , 0.45 , 0.2 , 0.1] , 'Callback' , @ButtonPress);

    % Create an uninteresting large array
    data = zeros(10000);

    % Store it in appdata
    setappdata(handles.figure , 'data' , data);

    % Store it in handles
    handles.data = data;

    % Save handles
    guidata(f, handles);



function  ButtonPress(hObject, eventdata)

    % Calculate the time difference when using guidata and appdata
    t_handles = timeit(@use_handles);
    t_appdata = timeit(@use_appdata);

    % Absolute and percentage difference
    t_diff = t_handles - t_appdata;
    t_perc = round(t_diff / t_handles * 100);

    disp(['Difference: ' num2str(t_diff) ' ms / ' num2str(t_perc) ' %'])




function  use_appdata()  

    % Retrieve the data from appdata
    data = getappdata(gcf , 'data');

    % Do something with data %

    % Store the value again
    setappdata(gcf , 'data' , data);


function use_handles()

    % Retrieve the data from handles
    handles = guidata(gcf);
    data = handles.data;

    % Do something with data %

    % Store it back in the handles
    handles.data = data;
    guidata(gcf, handles);

```

On my Xeon W3530@2.80 GHz I get `Difference: 0.00018957 ms / 73 %`, thus using getappdata/setappdata I get a performance improvement of 73%!
Note that the result does not change if a 10x10 double variable is used, however, result will change if `handles` contains many fields with large data.

