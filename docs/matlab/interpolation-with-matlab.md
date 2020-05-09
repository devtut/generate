---
metaTitle: "MATLAB - Interpolation with MATLAB"
description: "Piecewise interpolation 2 dimensional, Piecewise interpolation 1 dimensional, Polynomial interpolation"
---

# Interpolation with MATLAB



## Piecewise interpolation 2 dimensional


We initialize the data:

```matlab
[X,Y] = meshgrid(1:2:10);
Z = X.*cos(Y) - Y.*sin(X);

```

The surface looks like the following.
[<img src="http://i.stack.imgur.com/rxpHa.jpg" alt="interp2-data" />](http://i.stack.imgur.com/rxpHa.jpg)

Now we set the points where we want to interpolate:

```matlab
[Vx,Vy] = meshgrid(1:0.25:10); 

```

We now can perform nearest interpolation,

```matlab
Vz = interp2(X,Y,Z,Vx,Vy,'nearest');

```

[<img src="http://i.stack.imgur.com/GBLcC.jpg" alt="interp2-nearest" />](http://i.stack.imgur.com/GBLcC.jpg)

linear interpolation,

```matlab
Vz = interp2(X,Y,Z,Vx,Vy,'linear');

```

[<img src="http://i.stack.imgur.com/cs6fu.jpg" alt="interp2-linear" />](http://i.stack.imgur.com/cs6fu.jpg)

cubic interpolation

```matlab
Vz = interp2(X,Y,Z,Vx,Vy,'cubic');

```

or spline interpolation:

```matlab
Vz = interp2(X,Y,Z,Vx,Vy,'spline');

```

[<img src="http://i.stack.imgur.com/hLwar.jpg" alt="interp2-spline" />](http://i.stack.imgur.com/hLwar.jpg)



## Piecewise interpolation 1 dimensional


We will use the following data:

```matlab
x = 1:5:50;
y = randi([-10 10],1,10);

```

[<img src="http://i.stack.imgur.com/yNZaj.jpg" alt="interp1-data" />](http://i.stack.imgur.com/yNZaj.jpg)

Hereby `x` and `y` are the coordinates of the data points and `z` are the points we need information about.

```matlab
z = 0:0.25:50;

```

One way to find the y-values of z is piecewise linear interpolation.

```matlab
z_y = interp1(x,y,z,'linear');

```

[<img src="http://i.stack.imgur.com/EM68o.jpg" alt="interp1-linear" />](http://i.stack.imgur.com/EM68o.jpg)

Hereby one calculates the line between two adjacent points and gets `z_y` by assuming that the point would be an element of those lines.

`interp1` provides other options too like nearest interpolation,

```matlab
z_y = interp1(x,y,z, 'nearest');

```

[<img src="http://i.stack.imgur.com/YMwU4.jpg" alt="interp1-nearst" />](http://i.stack.imgur.com/YMwU4.jpg)

next interpolation,

```matlab
z_y = interp1(x,y,z, 'next');

```

[<img src="http://i.stack.imgur.com/l4lvh.jpg" alt="interp1-next" />](http://i.stack.imgur.com/l4lvh.jpg)

previous interpolation,

```matlab
z_y = interp1(x,y,z, 'previous');

```

[<img src="http://i.stack.imgur.com/V9B3j.jpg" alt="interp1-previous" />](http://i.stack.imgur.com/V9B3j.jpg)

Shape-preserving piecewise cubic interpolation,

```matlab
z_y = interp1(x,y,z, 'pchip');

```

[<img src="http://i.stack.imgur.com/3tEJ3.jpg" alt="interp1-pchip" />](http://i.stack.imgur.com/3tEJ3.jpg)

cubic convolution,
z_y = interp1(x,y,z, 'v5cubic');

[<img src="http://i.stack.imgur.com/5RQi5.jpg" alt="interp1-v5cubic" />](http://i.stack.imgur.com/5RQi5.jpg)

and spline interpolation

```matlab
z_y = interp1(x,y,z, 'spline');

```

[<img src="http://i.stack.imgur.com/MQtBM.jpg" alt="interp1-spline" />](http://i.stack.imgur.com/MQtBM.jpg)

Hereby are nearst, next and previous interpolation piecewise constant interpolations.



## Polynomial interpolation


We initialize the data we want to interpolate:

```matlab
x = 0:0.5:10;
y = sin(x/2);

```

This means the underlying function for the data in the interval [0,10] is sinusoidal. Now the coefficients of the approximating polynómials are being calculated:

```matlab
p1 = polyfit(x,y,1);
p2 = polyfit(x,y,2);
p3 = polyfit(x,y,3);
p5 = polyfit(x,y,5);
p10 = polyfit(x,y,10);

```

Hereby is `x` the x-value and `y` the y-value of our data points and the third number is the order/degree of the polynomial. We now set the grid we want to compute our interpolating function on:

```matlab
zx = 0:0.1:10;

```

and calculate the y-values:

```matlab
zy1 = polyval(p1,zx);
zy2 = polyval(p2,zx);
zy3 = polyval(p3,zx);
zy5 = polyval(p5,zx);
zy10 = polyval(p10,zx);

```

One can see that the approximation error for the sample gets smaller when the degree of the polynomial increases.

[<img src="http://i.stack.imgur.com/N7txY.jpg" alt="poly1-3" />](http://i.stack.imgur.com/N7txY.jpg)

While the approximation of the straight line in this example has larger errors the order 3 polynomial approximates the sinus function in this intervall relatively good.

[<img src="http://i.stack.imgur.com/mpB2l.jpg" alt="poly5+10" />](http://i.stack.imgur.com/mpB2l.jpg)

The interpolation with order 5 and order 10 polynomials has almost no apprroximation error.

However if we consider the out of sample performance one sees that too high orders tend to overfit and therefore perform badly out of sample. We set

```matlab
zx = -10:0.1:40;
p10 = polyfit(X,Y,10);
p20 = polyfit(X,Y,20);

```

and

```matlab
zy10 = polyval(p10,zx);
zy20 = polyval(p20,zx);

```

If we take a look at the plot we see that the out of sample performance is best for the order 1

[<img src="http://i.stack.imgur.com/ULMbB.jpg" alt="outOfSample1-3" />](http://i.stack.imgur.com/ULMbB.jpg)

and keeps getting worse with increasing degree.

[<img src="http://i.stack.imgur.com/qUluT.jpg" alt="enter image description here" />](http://i.stack.imgur.com/qUluT.jpg)



#### Syntax


1. zy = interp1(x,y);
1. zy = interp1(x,y,'method');
1. zy = interp1(x,y,'method','extrapolation');
1. zy = interp1(x,y,zx);
1. zy = interp1(x,y,zx,'method');
1. zy = interp1(x,y,zx,'method','extrapolation');

