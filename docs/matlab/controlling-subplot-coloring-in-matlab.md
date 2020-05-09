---
metaTitle: "MATLAB - Controlling Subplot coloring in Matlab"
description: "How it's done"
---

# Controlling Subplot coloring in Matlab


As I was struggling with this more than once, and the web isn't really clear on what to do, I decided to take what's out there, adding some of my own in order to explain how to create subplots which have one colorbar and they are scaled according to it.

I have tested this using latest Matlab but I'm pretty sure it'll work in older versions.



## How it's done


This is a simple code creating 6 3d-subplots and in the end syncing the color displayed in each of them.

```matlab
c_fin = [0,0];
[X,Y] = meshgrid(1:0.1:10,1:0.1:10);

figure; hold on;
for i = 1 : 6
    Z(:,:,i) = i * (sin(X) + cos(Y));

    ax(i) = subplot(3,2,i); hold on; grid on;
    surf(X, Y, Z(:,:,i));
    view(-26,30);
    colormap('jet');
    ca = caxis;
    c_fin = [min(c_fin(1),ca(1)), max(c_fin(2),ca(2))];
end

%%you can stop here to see how it looks before we color-manipulate

c = colorbar('eastoutside');
c.Label.String = 'Units';
set(c, 'Position', [0.9, 0.11, 0.03, 0.815]); %%you may want to play with these values
pause(2); %%need this to allow the last image to resize itself before changing its axes
for i = 1 : 6
    pos=get(ax(i), 'Position');
    axes(ax(i));
    set(ax(i), 'Position', [pos(1) pos(2) 0.85*pos(3) pos(4)]);
    set(ax(i),'Clim', c_fin); %%this is where the magic happens
end

```



#### Remarks


The only thing that you need to work out by yourself is the positioning of the colorbar (if you want to display it at all). this will depend on the number of graphs you have, and the bar's orientation.

Position and size is defined using 4 parameters - x_start, y_start, x_width, y_width. The plot is usually scaled to normalized units so that the bottom-left corner corresponds with (0,0) and the top-right to (1,1).

