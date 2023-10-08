function toPlot = prepare3DVector(Pick, mainSize, arrayX,arrayY,arrayZ)

%  for now mainSize x mainSize  assumed

if (Pick == "Field1")
  Matrix = load( "xField1.txt" )';
  rowCnt = mainSize + 1;
  stackCnt = mainSize + 1;
  
  colCnt = size( Matrix, 1);
  pgCnt = size( Matrix, 2)/(rowCnt*stackCnt);
  Matrix = permute( reshape(Matrix,colCnt,rowCnt,stackCnt,pgCnt),[2,1,3,4]);
  
  Matrix = (Matrix(1:(end-1),:,:,:) + Matrix(2:end,:,:,:))/2;
  Matrix = (Matrix(:,:,1:(end-1),:) + Matrix(:,:,2:end,:))/2;
  resX = Matrix;

  
  Matrix = load( "yField1.txt" )';  
  rowCnt = mainSize;
  stackCnt = mainSize + 1;

  colCnt = size( Matrix, 1);
  pgCnt = size( Matrix, 2)/(rowCnt*stackCnt);
  Matrix = permute( reshape(Matrix,colCnt,rowCnt,stackCnt,pgCnt),[2,1,3,4]);
  
  Matrix = (Matrix(:,1:(end-1),:,:) + Matrix(:,2:end,:,:))/2;
  Matrix = (Matrix(:,:,1:(end-1),:) + Matrix(:,:,2:end,:))/2;
  resY = Matrix;
  
  Matrix = load( "zField1.txt" )';
  rowCnt = mainSize + 1;
  stackCnt = mainSize;
  
  colCnt = size( Matrix, 1);
  pgCnt = size( Matrix, 2)/(rowCnt*stackCnt);
  Matrix = permute( reshape(Matrix,colCnt,rowCnt,stackCnt,pgCnt),[2,1,3,4]);

  Matrix = (Matrix(1:(end-1),:,:,:) + Matrix(2:end,:,:,:))/2;
  Matrix = (Matrix(:,1:(end-1),:,:) + Matrix(:,2:end,:,:))/2;

  resZ = Matrix;
  
  
elseif (Pick == "Field2")
  Matrix = load( "xField2.txt" )';
  rowCnt = mainSize;
  stackCnt = mainSize;

  colCnt = size( Matrix, 1);
  pgCnt = size( Matrix, 2)/(rowCnt*stackCnt);
  Matrix = permute( reshape(Matrix,colCnt,rowCnt,stackCnt,pgCnt),[2,1,3,4]);
  
  Matrix = (Matrix(:,1:(end-1),:,:) + Matrix(:,2:end,:,:))/2;
  
  resX = Matrix;
  
  Matrix = load( "yField2.txt" )';
  rowCnt = mainSize + 1;
  stackCnt = mainSize;

  colCnt = size( Matrix, 1);
  pgCnt = size( Matrix, 2)/(rowCnt*stackCnt);
  Matrix = permute( reshape(Matrix,colCnt,rowCnt,stackCnt,pgCnt),[2,1,3,4]);
  
  Matrix = (Matrix(1:(end-1),:,:,:) + Matrix(2:end,:,:,:))/2;
  
   resY = Matrix;
  
  Matrix = load( "zField2.txt" )';
  rowCnt = mainSize;
  stackCnt = mainSize + 1;
  
  colCnt = size( Matrix, 1);
  pgCnt = size( Matrix, 2)/(rowCnt*stackCnt);
  Matrix = permute( reshape(Matrix,colCnt,rowCnt,stackCnt,pgCnt),[2,1,3,4]);
  
  Matrix = (Matrix(:,:,1:(end-1),:) + Matrix(:,:,2:end,:))/2;
  resZ = Matrix;
endif


% decimate 
resX  = resX(arrayX,arrayY,arrayZ,:);
resY  = resY(arrayX,arrayY,arrayZ,:);
resZ  = resZ(arrayX,arrayY,arrayZ,:);
%resX  = resX(1:decim:end,1:decim:end,1:decim:end,1:decim:end);
%resY  = resY(1:decim:end,1:decim:end,1:decim:end,1:decim:end);
%resZ  = resZ(1:decim:end,1:decim:end,1:decim:end,1:decim:end);


x = linspace (1, size(resX,2),size(resX,2));
y = linspace (1, size(resX,1), size(resX,1)  );
z = linspace (1, size(resX,3), size(resX,3)  );


[xx, yy, zz] = meshgrid (x, y,z);


toPlot = struct ("xx", [], "yy", [], "zz", [],"vecX", [],"vecY",[],"vecZ", []);

toPlot.xx =xx;
toPlot.yy = yy;
toPlot.zz = zz;
toPlot.vecX = resX;
toPlot.vecY = resY;
toPlot.vecZ = resZ;

endfunction