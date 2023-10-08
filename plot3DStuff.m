%graphics_toolkit("gnuplot")
mainSize  = 20;

 %  X1 Y1 Z1 X2 Y2 Z2
 %  X Y Z

 field1 =  prepare3DVector( "Field1", mainSize, 8:1:12,8:1:12,8:1:12);
 field2 =  prepare3DVector( "Field2", mainSize, 8:1:12,8:1:12,8:1:12);
 
%fieldX = prepare3DStuff("X2", "X", 1, mainSize);
%fieldY = prepare3DStuff("Y2", "X", 1, mainSize);
%fieldZ = prepare3DStuff("Z2", "X", 1, mainSize); 
 
 clf;
colormap ("default");
 hold off;


 %set(h,'MaxHeadSize',1,'AutoScale','on', 'AutoScaleFactor', 2)
 %pgCnt =  size(fieldX.reshaped,3);
 
 
 %field1.vecX = adjust_matrix(field1.vecX);
 %field1.vecY = adjust_matrix(field1.vecY);
 %field1.vecZ = adjust_matrix(field1.vecZ);
 
% field2.vecX = adjust_matrix(field2.vecX);
 %field2.vecY = adjust_matrix(field2.vecY);
 %field2.vecZ = adjust_matrix(field2.vecZ); 
 
 %max(max(max (max (field2.vecX))))
 field2.vecX = restrictSize(field2.vecX,0.02);
 field2.vecY = restrictSize(field2.vecY,0.02);
 field2.vecZ = restrictSize(field2.vecZ,0.02);

 field1.vecX = restrictSize(field1.vecX,0.02);
 field1.vecY = restrictSize(field1.vecY,0.02);
 field1.vecZ = restrictSize(field1.vecZ,0.02);
 
 Z1 = 1
 Z2 = 1
 pgCnt =  size(field2.vecX,4);
 
 
fontSize  = 30;
lineSize  =7;

mod = 30;%0.1;
p = 1
%for  j = 1:pgCnt/p
 j =0
 while (1)
   j = j + 1;
   if  Z1 == 1 
   
        f1= figure (1);
       
        axX = field1.xx;
        axY = field1.yy;
        axZ = field1.zz; 
        vecX1 = field1.vecX(:,:,:,j*p) *mod;
        vecY1 = field1.vecY(:,:,:,j*p) *mod; 
        vecZ1 = field1.vecZ(:,:,:,j*p) *mod;
        
       
        
        if  (j == 1) 
          h = quiver3 ( axX, axY, axZ, vecX1, vecY1, vecZ1,'ShowArrowHead','on','linewidth',7,"udatasource", "vecX1","vdatasource", "vecY1","wdatasource", "vecZ1");
          %axis ([0 size(fieldX.xx,2)  0 size(fieldX.yy,1) -1 1]);
          set(h,'MaxHeadSize',0.2,'AutoScale','off', 'AutoScaleFactor', 1)
          
          set(gca, "linewidth", lineSize, "fontsize", fontSize);
          title (" Field ");
          grid on 
        endif 
        
        refreshdata ();
    endif
    
   if  Z2 == 1 
   
      f2= figure (2);
     
      axX = field2.xx;
      axY = field2.yy;
      axZ = field2.zz; 
      vecX2 = field2.vecX(:,:,:,j*p) *mod;
      vecY2 = field2.vecY(:,:,:,j*p) *mod; 
      vecZ2 = field2.vecZ(:,:,:,j*p) *mod;
      
     
      
      if  (j == 1) 
        h = quiver3 ( axX, axY, axZ, vecX2, vecY2, vecZ2,'ShowArrowHead','on','linewidth',5,"udatasource", "vecX2","vdatasource", "vecY2","wdatasource", "vecZ2");
        %axis ([0 size(fieldX.xx,2)  0 size(fieldX.yy,1) -1 1]);
        set(h,'MaxHeadSize',0.2,'AutoScale','off', 'AutoScaleFactor', 1)
        
        set(gca, "linewidth", lineSize, "fontsize", fontSize);
        title (" Field ");
        grid on 
      endif 
      
      refreshdata ();   
    endif
    %f1= figure (1);
    %ZX = fieldX.reshaped(:,:,j*p);
    %if  (j == 1) 
    %  mesh (fieldX.xx, fieldX.yy, ZX,"EdgeColor", "b","zdatasource", "ZX");
    %  axis ([0 size(fieldX.xx,2)  0 size(fieldX.yy,1) -1 1]);
    %  set(gca, "linewidth", lineSize, "fontsize", fontSize);
    %  title (" X field ");
    %  grid on 
    %endif 
    %refreshdata ();
    %f2 = figure (2);
    %ZY = fieldY.reshaped(:,:,j*p);
    %if  (j == 1)
    %   mesh (fieldY.xx, fieldY.yy, ZY,"EdgeColor", "b","zdatasource", "ZX");
    %  axis ([0 size(fieldY.xx,2)  0 size(fieldY.yy,1) -1 1]);
    %  title (" Y field ");
    %  set(gca, "linewidth", lineSize, "fontsize", fontSize);
    %  grid on 
    %endif 
    %refreshdata ();
    %f3 = figure (3);
    %ZZ = fieldZ.reshaped(:,:,j*p);
    %if  (j == 1) 
    %  mesh (fieldZ.xx, fieldZ.yy, ZZ,"EdgeColor", "b","zdatasource", "ZZ");
    %  title (" Z field ");
    %  axis ([0 size(fieldZ.xx,2)  0 size(fieldZ.yy,1) -1 1]);
    %  set(gca, "linewidth", lineSize, "fontsize", fontSize);
    %endif 
    %refreshdata ();
     if  (j == 300)
        j = 0;
       endif 
     
     %if (j > 1000 )
     %  break;
     % endif
    pause(0.3) 

end 
 
 %clf
 
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