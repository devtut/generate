---
metaTitle: "Line Algorithm"
description: "Bresenham Line Drawing Algorithm"
---

# Line Algorithm


Line drawing is accomplished by calculating intermediate positions along the line path between
two specified endpoint positions. An output device is then directed to fill in these positions
between the endpoints.



## Bresenham Line Drawing Algorithm


Background Theory:
Bresenham’s Line Drawing Algorithm is an efficient and accurate raster line generating algorithm developed by Bresenham. It involves only integer calculation so it is accurate and fast. It can also be extended to display circles another curves.

In Bresenham line drawing algorithm:<br>

For Slope |m|<1:<br>
Either value of x is increased<br>
OR both x and y is increased using decision parameter.  <br>

For Slope |m|>1:<br>
Either value of y is increased<br>
OR both x and y is increased using decision parameter.  <br>

**Algorithm for slope |m|<1:<br>**

<li>
Input two end points (x1,y1) and (x2,y2) of the line.
</li>
<li>
Plot the first point (x1,y1).
</li>
<li>
<p>Calculate<br>
Delx =| x2 – x1 |
<br>    Dely = | y2 – y1 |</p>
</li>
<li>
<p>Obtain the initial decision parameter as <br>
P = 2 * dely – delx</p>
</li>
<li>
<p>For I = 0 to delx in step of 1<br><br>
If p < 0 then<br>
X1 = x1 + 1<br>
Pot(x1,y1)<br>
P = p+ 2<em>dely<br><br>
Else<br>
X1 = x1 + 1<br>
Y1 = y1 + 1<br>
Plot(x1,y1)<br>
P = p + 2</em>dely – 2 * delx<br><br>
End if<br><br>
End for<br></p>
</li>
<li>
END
</li>

**Source Code:**

```cpp
/* A C program to implement Bresenham line drawing algorithm for |m|<1 */
#include<stdio.h>
#include<conio.h>
#include<graphics.h>
#include<math.h>

int main()
{    
 int gdriver=DETECT,gmode;
 int x1,y1,x2,y2,delx,dely,p,i;
 initgraph(&gdriver,&gmode,"c:\\TC\\BGI");

printf("Enter the intial points: ");
scanf("%d",&x1);
scanf("%d",&y1);
printf("Enter the end points: ");
scanf("%d",&x2);
scanf("%d",&y2);

putpixel(x1,y1,RED);

delx=fabs(x2-x1);
dely=fabs(y2-y1);
p=(2*dely)-delx;
for(i=0;i<delx;i++){
if(p<0)
{
 x1=x1+1;
 putpixel(x1,y1,RED);
 p=p+(2*dely);
}
else
{
 x1=x1+1;
 y1=y1+1;
 putpixel(x1,y1,RED);
 p=p+(2*dely)-(2*delx);
}
}
 getch();
 closegraph();
 return 0;
}

```

**Algorithm for slope |m|>1:**

1. Input two end points (x1,y1) and (x2,y2) of the line.
1. Plot the first point (x1,y1).
<li>Calculate<br>
Delx =| x2 – x1 |<br>
Dely = | y2 – y1 |</li>
<li>Obtain the initial decision parameter as
<br>P = 2 * delx – dely</li>
<li>For I = 0 to dely in step of 1<br><br>
If p < 0 then<br>
y1 = y1 + 1<br>
Pot(x1,y1)<br>
P = p+ 2<em>delx<br><br>
Else<br>
X1 = x1 + 1<br>
Y1 = y1 + 1<br>
Plot(x1,y1)<br>
P = p + 2</em>delx – 2 * dely<br><br>
End if<br><br>
End for<br><br></li>
1. END

**Source Code:**

```cpp
/* A C program to implement Bresenham line drawing algorithm for |m|>1 */
#include<stdio.h>
#include<conio.h>
#include<graphics.h>
#include<math.h>
int main()
{
int gdriver=DETECT,gmode;
int x1,y1,x2,y2,delx,dely,p,i;
initgraph(&gdriver,&gmode,"c:\\TC\\BGI");
printf("Enter the intial points: ");
scanf("%d",&x1);
scanf("%d",&y1);
printf("Enter the end points: ");
scanf("%d",&x2);
scanf("%d",&y2);
putpixel(x1,y1,RED);
delx=fabs(x2-x1);
dely=fabs(y2-y1);
p=(2*delx)-dely;
for(i=0;i<delx;i++){
if(p<0)
{
y1=y1+1;
putpixel(x1,y1,RED);
p=p+(2*delx);
}
else
{
x1=x1+1;
y1=y1+1;
putpixel(x1,y1,RED);
p=p+(2*delx)-(2*dely);
}
}
getch();
closegraph();
 return 0;
}

```

