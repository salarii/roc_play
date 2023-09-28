%graphics_toolkit("gnuplot")
mainSize  = 3

 %  X1 Y1 Z1 X2 Y2 Z2
 %  X Y Z

 
fieldX = prepare3DStuff("X2", "X", 1, mainSize);
fieldY = prepare3DStuff("Y2", "X", 1, mainSize);
fieldZ = prepare3DStuff("Z2", "X", 1, mainSize); 
 
 clf;
colormap ("default");
 hold off;


pgCnt =  size(fieldX.reshaped,3);
 
 fontSize  = 5;
 lineSize  =5;
 
 p = 1
 for  j = 1:pgCnt/p
   
   
    f1= figure (1);
    
    ZX = fieldX.reshaped(:,:,j*p);
    if  (j == 1) 
      
      mesh (fieldX.xx, fieldX.yy, ZX,"EdgeColor", "b","zdatasource", "ZX");

      axis ([0 size(fieldX.xx,2)  0 size(fieldX.yy,1) -1 1]);
      set(gca, "linewidth", lineSize, "fontsize", fontSize);

      title (" X field ");
      grid on 
    endif 
    refreshdata ();
    %clf (f1)
    

    f2 = figure (2);
    
    ZY = fieldY.reshaped(:,:,j*p);
    if  (j == 1)
       mesh (fieldY.xx, fieldY.yy, ZY,"EdgeColor", "b","zdatasource", "ZX");
      axis ([0 size(fieldY.xx,2)  0 size(fieldY.yy,1) -1 1]);
      title (" Y field ");
      set(gca, "linewidth", lineSize, "fontsize", fontSize);
      grid on 
    endif 
    
    
    %clf (f2)
    
    refreshdata ();
    %hold on 
    
    f3 = figure (3);
    ZZ = fieldZ.reshaped(:,:,j*p);
    if  (j == 1) 

      mesh (fieldZ.xx, fieldZ.yy, ZZ,"EdgeColor", "b","zdatasource", "ZZ");
      
      %grid on 
      title (" Z field ");

      axis ([0 size(fieldZ.xx,2)  0 size(fieldZ.yy,1) -1 1]);
        
      set(gca, "linewidth", lineSize, "fontsize", fontSize);

    endif 

    refreshdata ();
    %hold on 
    pause(0.2) 

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