function toPlot = prepare3DStuff( Print ,  dim, mainSize )

%  for now mainSize x mainSize  assumed


  
if (Print == "X1")
  Matrix = load( "xField1.txt" )';
  colCnt = mainSize + 1;
  stackCnt = mainSize + 1;
elseif (Print == "Y1")
  Matrix = load( "yField1.txt" )';  
  colCnt = mainSize;
  stackCnt = mainSize + 1;
elseif (Print == "Z1")
  Matrix = load( "zField1.txt" )';
  colCnt = mainSize + 1;
  stackCnt = mainSize;
elseif (Print == "X2")
  Matrix = load( "xField2.txt" )';
  colCnt = mainSize;
  stackCnt = mainSize;
elseif (Print == "Y2")
  Matrix = load( "yField2.txt" )';
  colCnt = mainSize + 1;
  stackCnt = mainSize;
elseif (Print == "Z2")  
  Matrix = load( "zField2.txt" )';
  colCnt = mainSize;
  stackCnt = mainSize + 1;
endif

clearrowCnt = size( Matrix, 1);
pgCnt = size( Matrix, 2)/(colCnt*stackCnt);

if (Print == "X1"  || Print == "X2" )
  reshaped = permute(reshape( Matrix(dim,:,:, : ), colCnt, stackCnt,pgCnt),[2,1,3]);
  
  x = linspace (1, colCnt,colCnt);
  y = linspace (1, stackCnt, stackCnt  );
  z = zeros(stackCnt, colCnt);
elseif (Print == "Y1" || Print == "Y2") 
  reshaped = reshape( Matrix(:,dim,:, : ), rowCnt, stackCnt, pgCnt);
  x = linspace (1, rowCnt,rowCnt);
  y = linspace (1, stackCnt, stackCnt  );
  z = zeros(stackCnt, rowCnt);
elseif (Print == "Z1" || Print == "Z2")
  reshaped = reshape( Matrix(:,dim,:, : ), rowCnt, colCnt, pgCnt);
  
  x = linspace (1, rowCnt,rowCnt);
  y = linspace (1, colCnt, colCnt  );
  z = zeros(colCnt, rowCnt);
endif


[xx, yy] = meshgrid (x, y);

toPlot = struct ("xx", [], "yy", [],"reshaped", []);

toPlot.xx =xx;
toPlot.yy = yy;
toPlot.reshaped = reshaped;
endfunction