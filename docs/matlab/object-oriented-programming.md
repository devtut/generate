---
metaTitle: "MATLAB - Object-Oriented Programming"
description: "Value vs Handle classes, Defining a class, Constructors, Inheriting from classes and abstract classes"
---

# Object-Oriented Programming




## Value vs Handle classes


Classes in MATLAB are divided into two major categories: value classes and handle classes. The major difference is that when copying an instance of a value class, the underlying data is copied to the new instance, while for handle classes the new instance points to the original data and changing values in new instance changes them in the original. A class can be defined as a handle by inheriting from the `handle` class.

```matlab
classdef valueClass
    properties
        data
    end
end

```

and

```matlab
classdef handleClass < handle
    properties
        data
    end
end

```

then

```matlab
>> v1 = valueClass;
>> v1.data = 5;
>> v2 = v1;
>> v2.data = 7;
>> v1.data
ans =
     5

>> h1 = handleClass;
>> h1.data = 5;
>> h2 = h1;
>> h2.data = 7;
>> h1.data
ans =
     7

```



## Defining a class


A class can be defined using `classdef` in an `.m` file with the same name as the class. The file can contain the `classdef`...`end` block and local functions for use within class methods.

The most general MATLAB class definition has the following structure:

```matlab
classdef (ClassAttribute = expression, ...) ClassName < ParentClass1 & ParentClass2 & ...

   properties (PropertyAttributes) 
      PropertyName
   end 

   methods (MethodAttributes) 
      function obj = methodName(obj,arg2,...)
         ...
      end
   end

   events (EventAttributes) 
      EventName
   end

   enumeration
      EnumName
   end

end

```

MATLAB Documentation: [Class attributes](http://www.mathworks.com/help/matlab/matlab_oop/class-attributes.html), [Property attributes](http://www.mathworks.com/help/matlab/matlab_oop/property-attributes.html), [Method attributes](http://www.mathworks.com/help/matlab/matlab_oop/method-attributes.html), [Event attributes](http://www.mathworks.com/help/matlab/matlab_oop/event-attributes.html), [Enumeration class restrictions](http://www.mathworks.com/help/matlab/matlab_oop/enumeration-class-restrictions.html).

### Example class:

A class called `Car` can be defined in file `Car.m` as

```matlab
classdef Car < handle % handle class so properties persist
    properties
        make
        model
        mileage = 0;
    end

    methods
        function obj = Car(make, model)
            obj.make = make;
            obj.model = model;
        end
        function drive(obj, milesDriven)
            obj.mileage = obj.mileage + milesDriven;
        end
    end
end

```

Note that the constructor is a method with the same name as the class.
<A constructor is a special method of a class or structure in object-oriented programming that initializes an object of that type. A constructor is an instance method that usually has the same name as the class, and can be used to set the values of the members of an object, either to default or to user-defined values.>

An instance of this class can be created by calling the constructor;

```matlab
>> myCar = Car('Ford', 'Mustang'); //creating an instance of car class 

```

Calling the `drive` method will increment the mileage

```matlab
>> myCar.mileage 
    
    ans = 
            0

>> myCar.drive(450);

>> myCar.mileage
    
   ans = 
            450

```



## Constructors


A [constructor](http://uk.mathworks.com/help/matlab/matlab_oop/class-constructor-methods.html?requestedDomain=www.mathworks.com) is a special method in a class that is called when an instance of an object is created. It is a regular MATLAB function that accepts input parameters but it also must follow certain [rules](http://uk.mathworks.com/help/matlab/matlab_oop/class-constructor-methods.html?requestedDomain=www.mathworks.com).

Constructors are not required as MATLAB creates a default one. In practice, however, this is a place to define a state of an object. For example, properties can be restricted by specifying [attributes](http://uk.mathworks.com/help/matlab/matlab_oop/property-attributes.html). Then, a constructor can [initalize](http://uk.mathworks.com/help/matlab/matlab_oop/specifying-properties.html#brqy3km-10) such properties by default or user defined values which in fact can sent by input parameters of a constructor.

**Calling a constructor of a simple class**

This is a simple class `Person`.

```matlab
classdef Person
    properties
        name
        surname
        address
    end
    
    methods
        function obj = Person(name,surname,address)
            obj.name = name;
            obj.surname = surname;
            obj.address = address;
        end
    end
end

```

The name of a constructor is the same the name of a class. Consequently, constructors are called by the name of its class. A class `Person` can be created as follows:

```matlab
>> p = Person('John','Smith','London')
p = 
  Person with properties:

       name: 'John'
    surname: 'Smith'
    address: 'London'

```

**Calling a constructor of a child class**

Classes can be inherited from parent classes if the share common properties or methods. When a class is inherited from another, it is likely that a constructor of a parent class has to be called.

A class `Member` inherits from a class `Person` because `Member` uses the same properties as the class Person but it also adds `payment` to its definition.

```matlab
classdef Member < Person
    properties
        payment
    end

    methods
        function obj = Member(name,surname,address,payment)
            obj = obj@Person(name,surname,address);
            obj.payment = payment;
        end
    end
end

```

Similarly to the class `Person`, `Member` is created by calling its constructor:

```matlab
>> m = Member('Adam','Woodcock','Manchester',20)
m = 
  Member with properties:

    payment: 20
       name: 'Adam'
    surname: 'Woodcock'
    address: 'Manchester'

```

A constructor of `Person` requires three input parameters. `Member` must respect this fact and therefore call a constructor of the class `Person` with three parameters. It is fulfilled by the line:

```matlab
obj = obj@Person(name,surname,address);

```

The example above shows the case when a child class needs information for its parent class. This is why a constructor of `Member` requires four parameters: three for its parent class and one for itself.



## Inheriting from classes and abstract classes


Disclaimer: the examples presented here are only for the purpose of showing the use of abstract classes and inheritance and may not necessarily be of a practical use. Also, there is no sich thing as polymorphic in MATLAB and therefore the use of abstract classes is limited. This example is to show who to create a class, inherit from another class and apply an abstract class to define a common interface.

The use of abstract classes is rather limited in MATLAB but it still can come useful on a couple of occasions.

Let's say we want a message logger. We might create a class similar to the one below:

```matlab
classdef ScreenLogger
    properties(Access=protected)
        scrh;
    end
    
    methods
        function obj = ScreenLogger(screenhandler)
            obj.scrh = screenhandler;
        end
        
        function LogMessage(obj, varargin)
            if ~isempty(varargin)
                varargin{1} = num2str(varargin{1});
                fprintf(obj.scrh, '%s\n', sprintf(varargin{:}));
            end
        end
    end
end

```

**Properties and methods**

In short, properties hold a state of an object whilst methods are like interface and define actions on objects.

The property `scrh` is protected. This is why it must be initialized in a constructor. There are other methods (getters) to access this property but it is out of cope of this example. Properties and methods can be access via a variable that holds a reference to an object by using dot notation followed by a name of a method or a property:

```matlab
mylogger = ScreenLogger(1);                         % OK
mylogger.LogMessage('My %s %d message', 'very', 1); % OK
mylogger.scrh = 2;                                  % ERROR!!! Access denied

```

Properties and methods can be public, private, or protected. In this case, protected means that I will be able to access to `scrh` from an inherited class but not from outside. By default all properties and methods are public. Therefore `LogMessage()` can freely be used outside the class definition. Also `LogMessage` defines an interface meaning this is what we must call when we want an object to log our custom messages.

**Application**

Let's say I have a script where I utilize my logger:

```matlab
clc;
% ... a code
logger = ScreenLogger(1);
% ... a code
logger.LogMessage('something');
% ... a code
logger.LogMessage('something');
% ... a code
logger.LogMessage('something');
% ... a code
logger.LogMessage('something');

```

If I have multiple places where I use the same logger and then want to change it to something more sophisticated, such as write a message in a file, I would have to create another object:

```matlab
classdef DeepLogger
    properties(SetAccess=protected)
        FileName
    end
    methods
        function obj = DeepLogger(filename)
            obj.FileName = filename;
        end
        
        function LogMessage(obj, varargin)
            if ~isempty(varargin)
                varargin{1} = num2str(varargin{1});
                fid = fopen(obj.fullfname, 'a+t');
                fprintf(fid, '%s\n', sprintf(varargin{:}));
                fclose(fid);
            end
        end
    end 
end

```

and just change one line of a code into this:

```matlab
clc;
% ... a code
logger = DeepLogger('mymessages.log');

```

The above method will simply open a file, append a message at the end of the file and close it. At the moment, to be consistent with my interface, I need to remember that the name of a method is `LogMessage()` but it could equally be anything else. MATLAB can force developper to stick to the same name by using abstract classes. Let's say we define a common interface for any logger:

```matlab
classdef MessageLogger
    methods(Abstract=true)
        LogMessage(obj, varargin);
    end
end

```

Now, if both `ScreenLogger` and `DeepLogger` inherit from this class, MATLAB will generate an error if `LogMessage()` is not defined. Abstract classes help to build similar classes which can use the same interface.

For the sake of this exmaple, I will make slightly different change. I am going to assume that DeepLogger will do both logging message on a screen and in a file at the same time. Because `ScreenLogger` already log messages on screen, I am going to inherit `DeepLogger` from the `ScreenLoggger` to avoid repetition. `ScreenLogger` doesn't change at all apart from the first line:

```matlab
classdef ScreenLogger < MessageLogger
// the rest of previous code 

```

However, `DeepLogger` needs more changes in the `LogMessage` method:

```matlab
classdef DeepLogger < MessageLogger & ScreenLogger
    properties(SetAccess=protected)
        FileName
        Path
    end
    methods
        function obj = DeepLogger(screenhandler, filename)
            [path,filen,ext] = fileparts(filename);
            obj.FileName = [filen ext];
            pbj.Path     = pathn;
            obj = obj@ScreenLogger(screenhandler);
        end
        function LogMessage(obj, varargin)
            if ~isempty(varargin)
                varargin{1} = num2str(varargin{1});
                LogMessage@ScreenLogger(obj, varargin{:});
                fid = fopen(obj.fullfname, 'a+t');
                fprintf(fid, '%s\n', sprintf(varargin{:}));
                fclose(fid);
            end
        end
    end
end

```

Firstly, I simply initialize properties in the constructor. Secondly, because this class inherits from `ScreenLogger` I have to initialize this parrent object as well. This line is even more important because `ScreenLogger` constructor requires one parameter to initalize its own object. This line:

```matlab
obj = obj@ScreenLogger(screenhandler);

```

simply says "call the consructor of ScreenLogger and initalize it with a screen handler". It is worth noting here that I have defined `scrh` as protected. Therefore, I could equally access this property from `DeepLogger`. If the property was defined as private. The only way to intialize it would be using consuctor.

Another change is in section `methods`. Again to avoid repetition, I call `LogMessage()` from a parent class to log a message on a screen. If I had to change anything to make improvements in screen logging, now I have to do it in one place. The rest code is the same as it is a part of `DeepLogger`.

Because this class also inherits from an abstract class `MessageLogger` I had to make sure that `LogMessage()` inside `DeepLogger` is also defined. Inheritting from `MessageLogger` is a little bit tricky here. I think it cases redefinition of `LogMessage` mandatory--my guess.

In terms of the code where the a logger is applied, thanks to a common interface in classes, I can rest assure ther changin this one line in the whole code would not make any issues. The same messages will be log on screen as before but additionally the code will write such messages to a file.

```matlab
clc;
% ... a code
logger = DeepLogger(1, 'mylogfile.log');
% ... a code
logger.LogMessage('something');
% ... a code
logger.LogMessage('something');
% ... a code
logger.LogMessage('something');
% ... a code
logger.LogMessage('something');

```

I hope these examples explained the use of classes, the use of inheritance, and the use of abstract classes.

PS. The solution for the above problem is one of many. Another solution, less complex, would be to make `ScreenLoger` to be a component of another logger like `FileLogger` etc. `ScreenLogger` would be held in one of the properties. Its `LogMessage` would simply call `LogMessage` of the `ScreenLogger` and show text on a screen. I have chosen more complex approach to rather show how classes work in MATLAB. The example code below:

```matlab
classdef DeepLogger < MessageLogger
    properties(SetAccess=protected)
        FileName
        Path
        ScrLogger
    end
    methods
        function obj = DeepLogger(screenhandler, filename)
            [path,filen,ext] = fileparts(filename);
            obj.FileName     = [filen ext];
            obj.Path         = pathn;
            obj.ScrLogger    = ScreenLogger(screenhandler);
        end
        function LogMessage(obj, varargin)
            if ~isempty(varargin)
                varargin{1} = num2str(varargin{1});
                obj.LogMessage(obj.ScrLogger, varargin{:}); % <-------- thechange here
                fid = fopen(obj.fullfname, 'a+t');
                fprintf(fid, '%s\n', sprintf(varargin{:}));
                fclose(fid);
            end
        end
    end
end

```

