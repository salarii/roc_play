zRowsParam = 2

Z = load( "zField.txt" );
X = load( "xField.txt" );
Y = load( "yField.txt" );

zColumns = size( Z, 2)
Zreshaped = reshape (Z,zRowsParam, zColumns,size( Z, 1)/zRowsParam )

 clf;
 colormap ("default");
zColumns = 4
zRowsParam = 5
 
x = linspace (1, zColumns, zColumns )
y = linspace (1, zRowsParam, zRowsParam )
z = zeros(zRowsParam,zColumns)
ones(zRowsParam,zColumns)

[xx, yy] = meshgrid (x, y)
 h = quiver3 (xx, yy, z, 0, 0.01, ones(zRowsParam,zColumns), 'AutoScale','off',...
            'MaxHeadSize',0.1,
            'LineWidth',1);
 #set (h, "maxheadsize", 0.5);
set(gca, "linewidth", 10, "fontsize", 40)
 
 hold off;
 title ("quiver3 ");

