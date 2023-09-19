CollParam = 50

Z = load( "zField.txt" )';
X = load( "xField.txt" )';
Y = load( "yField.txt" )';
Matrix = Z;
RowParam = size( Matrix, 1);
pagesCnt = size( Matrix, 2)/CollParam;

Zreshaped = permute(reshape (Matrix, RowParam, CollParam,pagesCnt),[1,2,3]);

 clf;
 colormap ("default");
 
x = linspace (1, CollParam,CollParam);
y = linspace (1, RowParam, RowParam  );
z = zeros(RowParam,CollParam);


[xx, yy] = meshgrid (x, y);

 #set (h, "maxheadsize", 0.5);

 
 hold off;
 title ("quiver3 ");

 p = 3
 for  j = 1:pagesCnt/p
 #h = quiver3 (xx, yy, z, 0, 0.01, Zreshaped(:,:,j), 'AutoScale','off',...
  #          'MaxHeadSize',0.1,
   #         'LineWidth',1);
    mesh (xx, yy, Zreshaped(:,:,j*p),"EdgeColor", "b");
    axis ([0 RowParam  0 CollParam -1 1])
    
    set(gca, "linewidth", 20, "fontsize", 40)
    grid on 
    pause(0.1) 
    hold on 

    clf 

 
end 
 
 clf
 
 
