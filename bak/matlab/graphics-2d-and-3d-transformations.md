---
metaTitle: "MATLAB - Graphics: 2D and 3D Transformations"
description: "2D Transformations"
---

# Graphics: 2D and 3D Transformations



## 2D Transformations


In this Example we are going to take a sqaure shaped line plotted using `line` and perform transformations on it. Then we are going to use the same tranformations but in different order and see how it influences the results.

First we open a figure and set some initial parameters (square point coordinates and transformation parameters)

```matlab
%Open figure and create axis
Figureh=figure('NumberTitle','off','Name','Transformation Example',...
    'Position',[200 200 700 700]); %bg is set to red so we know that we can only see the axes
Axesh=axes('XLim',[-8 8],'YLim',[-8,8]);

 %Initializing Variables
    square=[-0.5 -0.5;-0.5 0.5;0.5 0.5;0.5 -0.5]; %represented by its vertices
    Sx=0.5;
    Sy=2;
    Tx=2;
    Ty=2;
    teta=pi/4;

```

Next we construct the transformation matrices (scale, rotation and translation):

```matlab
%Generate Transformation Matrix
S=makehgtform('scale',[Sx Sy 1]);
R=makehgtform('zrotate',teta);
T=makehgtform('translate',[Tx Ty 0]);

```

Next we plot the blue suare:

```matlab
%% Plotting the original Blue Square
OriginalSQ=line([square(:,1);square(1,1)],[square(:,2);square(1,2)],'Color','b','LineWidth',3);
grid on;    % Applying grid on the figure
hold all;   % Holding all Following  graphs to current axes

```

Next we will plot it again in a different color (red) and apply the transformations:

```matlab
%% Plotting the Red Square
%Calculate rectangle vertices
HrectTRS=T*R*S;
RedSQ=line([square(:,1);square(1,1)],[square(:,2);square(1,2)],'Color','r','LineWidth',3);
%transformation of the axes
AxesTransformation=hgtransform('Parent',gca,'matrix',HrectTRS);
%seting the line to be a child of transformed axes
set(RedSQ,'Parent',AxesTransformation);

```

The result should look like this:

[<img src="https://i.stack.imgur.com/6z4VH.jpg" alt="The original blue square and the transformed red square " />](https://i.stack.imgur.com/6z4VH.jpg)

Now lets see what happens when we change the transformation order:

```matlab
%% Plotting the Green Square
HrectRST=R*S*T;
GreenSQ=line([square(:,1);square(1,1)],[square(:,2);square(1,2)],'Color','g','LineWidth',3);
AxesTransformation=hgtransform('Parent',gca,'matrix',HrectRST);
set(GreenSQ,'Parent',AxesTransformation);

%% Plotting the Yellow Square
HrectSRT=S*R*T;
YellowSQ=line([square(:,1);square(1,1)],[square(:,2);square(1,2)],'Color','y','LineWidth',3);
AxesTransformation=hgtransform('Parent',gca,'matrix',HrectSRT);
set(YellowSQ,'Parent',AxesTransformation);

```

[<img src="https://i.stack.imgur.com/EDXTj.png" alt="The transformed squares " />](https://i.stack.imgur.com/EDXTj.png)

