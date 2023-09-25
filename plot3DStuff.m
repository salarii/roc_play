
mainSize  = 3

Print = "X1";  %  X1 Y1 Z1 X2 Y2 Z2
dim  =  2;
  
data = prepare3DStuff(Print, dim, mainSize);
 
 
 clf;
colormap ("default");
 hold off;
 title (" plot 3D ");

pgCnt =  size(data.reshaped,3);
 
 p = 1
 for  j = 1:pgCnt/p
    f1= figure (1);
    mesh (data.xx, data.yy, data.reshaped(:,:,j*p),"EdgeColor", "b");
    axis ([0 size(data.xx,2)  0 size(data.yy,1) -1 1])
    
    set(gca, "linewidth", 20, "fontsize", 40)
    grid on 

    hold on 
    f2 = figure (2);
    mesh (data.xx, data.yy, data.reshaped(:,:,j*p),"EdgeColor", "b");
    axis ([0 size(x,2)  0 size(y,2) -1 1])
    
    set(gca, "linewidth", 20, "fontsize", 40)
    grid on 

    hold on 
    pause(10) 
    clf (f1)
    clf (f2)

 
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