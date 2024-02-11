% Initialize dataMatrix and parameters
dataMatrix = load( "data.txt" );

columnIndex = 0;
LineWidth = 5; % Line width for the plot
fontSize = 30; % Font size for text elements
axisLineWidth = 2; % Thickness of the axes

% Check if a specific column is requested
if columnIndex > 0
    if columnIndex > size(dataMatrix, 2) || columnIndex <= 1
        error('Invalid column index. Please choose a column index greater than 1 and within the matrix dimension.');
    end
    figure; % Create a new figure for the specific plot
    plot(dataMatrix(:,1), dataMatrix(:,columnIndex), 'LineWidth', LineWidth);
    title(sprintf('Detailed Plot of Column %d', columnIndex), 'FontSize', fontSize);
    xlabel('Argument', 'FontSize', fontSize);
    ylabel('Value', 'FontSize', fontSize);
    set(gca, 'FontSize', fontSize, 'LineWidth', axisLineWidth); % Set font size and axis line width
else
    % Plot all columns against the first column in separate subplots
    n = size(dataMatrix, 2) - 1; % Number of plots needed, excluding the first column

    % Calculate number of rows and cols for subplot
    numRows = ceil(sqrt(n));
    numCols = ceil(n / numRows);

    figure; % Create a new figure for the subplot
    for i = 1:n
        subplot(numRows, numCols, i);
        plot(dataMatrix(:,1), dataMatrix(:,i+1), 'LineWidth', LineWidth);
        title(sprintf('Plot %d', i), 'FontSize', fontSize);
        xlabel('Argument', 'FontSize', fontSize);
        ylabel('Value', 'FontSize', fontSize);
        set(gca, 'FontSize', fontSize, 'LineWidth', axisLineWidth); % Set font size and axis line width for each subplot
    end
end

B = [1.00  1.00  2.33  0.33 ;
0.00  1.00  1.44  0.11 ; 
0.00  0.00  1.00  0.25  ;
0.00  0.00  0.00  1.00 ]
B = [1; 2 ; 7i]
 R=[3, 4, 1; -4, 1, 8; -1  , -3, 5] 
  inv (R) * B 
  
  
  

   A = [1 1 ; 1 0]
  %  eigenvalues 
  gamma = [1.618  0; 0  -0.618]
  %  vector  
  [ -0.618  1; 1  -1.618 ]
  [ 1.618  1; 1  0.618 ] 
 
  S =  [1.62  -0.62 ; 1  1  ]
 
  % check 
   A * [-0.62 ;  1]  =  -0.61 *    [-0.62 ;  1]
   %  calc  c  
   
   c= inv(S)*[1 ; 0]
 
 %  
   
  



 

    1  2 3 5 8 13 21  34
    