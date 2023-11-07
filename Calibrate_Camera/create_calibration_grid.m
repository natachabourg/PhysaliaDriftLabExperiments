% From the image with the lines Grid4Calib.png, input lines, find intersections, then put
% them in a nice matrix with the right order
% Author: Amandine Schaeffer 14 June 2023

clear all
close all

%Input grid lines: first one way, then other way
[xs, ys] = snake_read('Grid4calib.png')

% From the image with the lines, input lines, find intersections, then put
% them in a nice matrix with the right order
% Amandine 14 June 2023

clear all
close all

%Input grid lines: first one way, then other way
[xs, ys] = snake_read('Grid4calib.png')


xs_len = length(xs)
nb_lines1 = input('How many points in lines done first ( vertical or horizontal)') 
% Most likely 24

figure()
plot(xs,ys,'o');
hold on;

% Find intersection vertical and horizontal lines
for i = 1:2:size(xs, 1)
    plot([xs(i) xs(i+1)], [ys(i) ys(i+1)], 'r-');
    hold on;
    [x_int,y_int] = line_intersection([xs(i),ys(i),xs(i+1),ys(i+1)],[xs(i),ys(i),xs(i+1),ys(i+1)]) %returns the intersection [x_int,y_int] of two lines.
end

L1 = [xs(1:nb_lines1)';ys(1:nb_lines1)'];
L2 = [xs(nb_lines1+1:end)';ys(nb_lines1+1:end)'];
P = InterX(L1,L2) 
plot(P(1,:),P(2,:),'ko')

save('data_coord_grid_bad_order')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now need to reorder all points from the grid

% load('data_coord_grid_bad_order.mat')
xq =P(1,:);
yq = P(2,:);

figure()
plot(xq,yq, 'bp')                                          % Plot All Points
hold on

Final_gridpoints_order_x=[];
Final_gridpoints_order_y=[];

% enter polygons to define each vertical line and reorder from ascending y
% within each
for i = 1:nb_lines1/2

    print('Enter polygon for one line')
    [pol_x1, pol_y1] = ginput(1)
    [pol_x2, pol_y2] = ginput(1)
    [pol_x3, pol_y3] = ginput(1)
    [pol_x4, pol_y4] = ginput(1)
    
    xv = [pol_x1, pol_x2, pol_x3, pol_x4, pol_x1];                             % Polygon X-Coordinates
    yv = [pol_y1, pol_y2, pol_y3, pol_y4, pol_y1];                             % Polygon Y-Coordinates
    
    [in,on] = inpolygon(xq,yq, xv,yv);                          % Logical Matrix
    inon = in | on;                                             % Combine ‘in’ And ‘on’
    idx = find(inon(:));                                        % Linear Indices Of ‘inon’ Points
    xcoord = xq(idx);                                           % X-Coordinates Of ‘inon’ Points
    ycoord = yq(idx);                                           % Y-Coordinates Of ‘inon’ Points
    
    plot(xv, yv, '-r')                                          % Plot Polygon
    plot(xcoord, ycoord, 'gp')                                  % Overplot ‘inon’ Points

    A = [xcoord;ycoord];
    Final_gridpoints_order_line = sortrows(A',2)'
    scatter(Final_gridpoints_order_line(1,:),Final_gridpoints_order_line(2,:))

    Final_gridpoints_order_x = [Final_gridpoints_order_x;Final_gridpoints_order_line(1,:)]
    Final_gridpoints_order_y = [Final_gridpoints_order_y;Final_gridpoints_order_line(2,:)]
end

% Result is 2 matrices, one with x coord, one with y coord
figure()
scatter(Final_gridpoints_order_x,Final_gridpoints_order_y)

save('data_coord_grid_good_order')
save('data_coord_grid_matrix_camera.mat','Final_gridpoints_order_x','Final_gridpoints_order_y')% 
