zRowsParam = 20

Z = load( "zField.txt" )';
X = load( "xField.txt" )';
Y = load( "yField.txt" )';

zColumns = size( Z, 1);
pagesCnt = size( Z, 2)/zRowsParam;

Zreshaped = permute(reshape (Z, zRowsParam, zColumns,pagesCnt),[2,1,3]);

 clf;
 colormap ("default");
 
x = linspace (1, zColumns, zColumns );
y = linspace (1, zRowsParam, zRowsParam );
z = zeros(zRowsParam,zColumns);


[xx, yy] = meshgrid (x, y);

 #set (h, "maxheadsize", 0.5);

 
 hold off;
 title ("quiver3 ");

 p = 1
 for  j = 1:pagesCnt/p
 #h = quiver3 (xx, yy, z, 0, 0.01, Zreshaped(:,:,j), 'AutoScale','off',...
  #          'MaxHeadSize',0.1,
   #         'LineWidth',1);
    mesh (xx, yy, Zreshaped(:,:,j*p),"EdgeColor", "b");
    axis ([0 zColumns  0 zRowsParam -0.5 1])
    
    set(gca, "linewidth", 20, "fontsize", 40)
    grid on 
    pause(0.1) 
    hold on 

    clf 

 
end 
 
 clf
 
 
