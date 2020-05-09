---
metaTitle: "MATLAB - Integration"
description: "Integral, integral2, integral3"
---

# Integration



## Integral, integral2, integral3


**1 dimensional**

To integrate a one dimensional function

```matlab
f = @(x) sin(x).^3 + 1;

```

within the range

```matlab
xmin = 2;
xmax = 8;

```

one can call the function

```matlab
q = integral(f,xmin,xmax);

```

it's also possible to set boundarys for relative and absolute errors

```matlab
q = integral(f,xmin,xmax, 'RelTol',10e-6, 'AbsTol',10-4);

```

**2 dimensional**

If one wants to integrate a two dimensional function

```matlab
f = @(x,y) sin(x).^y ;

```

within the range

```matlab
xmin = 2;
xmax = 8;
ymin = 1;
ymax = 4;

```

one calls the function

```matlab
q = integral2(f,xmin,xmax,ymin,ymax);

```

Like in the other case it's possible to limit the tolerances

```matlab
q = integral2(f,xmin,xmax,ymin,ymax, 'RelTol',10e-6, 'AbsTol',10-4);

```

**3 dimensional**

Integrating a three dimensional function

```matlab
f = @(x,y,z) sin(x).^y - cos(z) ;

```

within the range

```matlab
xmin = 2;
xmax = 8;
ymin = 1;
ymax = 4;
zmin = 6;
zmax = 13;

```

is performed by calling

```matlab
q = integral3(f,xmin,xmax,ymin,ymax, zmin, zmax);

```

Again it's possible to limit the tolerances

```matlab
q = integral3(f,xmin,xmax,ymin,ymax, zmin, zmax, 'RelTol',10e-6, 'AbsTol',10-4);

```

