function toPlot = prepare3DStuff(Pick, Print, dim, mainSize )

%  for now mainSize x mainSize  assumed

if (Pick == "X1")
  Matrix = load( "xField1.txt" )';
  rowCnt = mainSize + 1;
  stackCnt = mainSize + 1;
elseif (Pick == "Y1")
  Matrix = load( "yField1.txt" )';  
  rowCnt = mainSize;
  stackCnt = mainSize + 1;
elseif (Pick == "Z1")
  Matrix = load( "zField1.txt" )';
  rowCnt = mainSize + 1;
  stackCnt = mainSize;
elseif (Pick == "X2")
  Matrix = load( "xField2.txt" )';
  rowCnt = mainSize;
  stackCnt = mainSize;
elseif (Pick == "Y2")
  Matrix = load( "yField2.txt" )';
  rowCnt = mainSize + 1;
  stackCnt = mainSize;
elseif (Pick == "Z2")  
  Matrix = load( "zField2.txt" )';
  rowCnt = mainSize;
  stackCnt = mainSize + 1;
endif



colCnt = size( Matrix, 1);
pgCnt = size( Matrix, 2)/(rowCnt*stackCnt);
Matrix = permute( reshape(Matrix,colCnt,rowCnt,stackCnt,pgCnt),[2,1,3,4]);

if (Print == "X"   )
  reshaped = reshape( Matrix(:,dim,:, : ), rowCnt, stackCnt, pgCnt);
  x = linspace (1, stackCnt, stackCnt);
  y = linspace (1, rowCnt, rowCnt);
  z = zeros(stackCnt, rowCnt);
elseif (Print == "Y" ) 
  reshaped = permute(reshape( Matrix(dim,:,:, : ), colCnt, stackCnt,pgCnt),[2,1,3]);
  x = linspace (1, colCnt,colCnt);
  y = linspace (1, stackCnt, stackCnt  );
  z = zeros(stackCnt, colCnt);
elseif (Print == "Z" )
  reshaped = reshape( Matrix(:,:,dim, : ), rowCnt, colCnt, pgCnt);
  x = linspace (1, colCnt,colCnt);
  y = linspace (1, rowCnt, rowCnt  );
  z = zeros(rowCnt, colCnt);
endif


[xx, yy] = meshgrid (x, y);

toPlot = struct ("xx", [], "yy", [],"reshaped", []);

toPlot.xx =xx;
toPlot.yy = yy;
toPlot.reshaped = reshaped;
endfunction