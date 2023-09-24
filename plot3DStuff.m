%  for now size x size  assumed

size  = 3

Print = "X1"  %  X1 Y1 Z1 X2 Y2 Z2
dim  =  1
  
if (Print == "X1")
  Matrix = load( "xField1.txt" )';
  colCnt = size;
  stackCnt = size + 1;
elseif (Print == "Y1")
  Matrix = load( "yField1.txt" )';  
  colCnt = size + 1;
  stackCnt = size + 1;
elseif (Print == "Z1")
  Matrix = load( "zField1.txt" )';
  colCnt = size + 1;
  stackCnt = size;
elseif (Print == "X2")
  Matrix = load( "xField2.txt" )';
  colCnt = size + 1;
  stackCnt = size;
elseif (Print == "Y2")
  Matrix = load( "yField2.txt" )';
  colCnt = size;
  stackCnt = size;
elseif (Print == "Z2")  
  Matrix = load( "zField2.txt" )';
  colCnt = size;
  stackCnt = size + 1;
endif

rowCnt = size( Matrix, 1);
pgCnt = size( Matrix, 2)/CollParam;

if (Print == "X1"  or Print == "X2"  or )
  reshaped = permute(reshape( A(dim,:,:, : ), colCnt, stackCnt,pgCnt),[2,1,3]);
  
  x = linspace (1, colCnt,colCnt);
  y = linspace (1, stackCnt, stackCnt  );
  z = zeros(stackCnt, colCnt);
elseif (Print == "Y1" or Print == "Y2") 
  reshaped = reshape( A(:,dim,:, : ), rowCnt, stackCnt, pgCnt);
  x = linspace (1, rowCnt,rowCnt);
  y = linspace (1, stackCnt, stackCnt  );
  z = zeros(stackCnt, rowCnt);
elseif (Print == "Z1" or Print == "Z2")
  reshaped = reshape( A(:,dim,:, : ), rowCnt, colCnt, pgCnt);
  
  x = linspace (1, rowCnt,rowCnt);
  y = linspace (1, colCnt, colCnt  );
  z = zeros(colCnt, rowCnt);
endif

clf;
colormap ("default");
 
x = linspace (1, CollParam,CollParam);
y = linspace (1, RowParam, RowParam  );
z = zeros(RowParam,CollParam);


[xx, yy] = meshgrid (x, y);

 #set (h, "maxheadsize", 0.5);

 
 hold off;
 title (" plot 3D ");

 p = 3
 for  j = 1:pagesCnt/p

    mesh (xx, yy, reshaped(:,:,j*p),"EdgeColor", "b");
    axis ([0 RowParam  0 CollParam -1 1])
    
    set(gca, "linewidth", 20, "fontsize", 40)
    grid on 
    pause(0.1) 
    hold on 

    clf 

 
end 
 
 clf
 
 %A  = reshape(1:54,3,3,3,2)
 %  1
%permute(reshape( A(1,:,:, : ) , 3,3,2),[2,1,3])
%  2
%reshape( A(:,1,:, : ) , 3,3,2)
%  3
%reshape( A(:,:,1, : ) , 3,3,2)

 #h = quiver3 (xx, yy, z, 0, 0.01, Zreshaped(:,:,j), 'AutoScale','off',...
  #          'MaxHeadSize',0.1,
   #         'LineWidth',1);