out = load( "out.txt" );

b =  4;
out = out(b:b:end,:);

t = 1:1: size( out, 2);
y = sin(t);

for  j = 1:size(out,1) 
    plot(t,out(j,:) )
    sizeX = size(out,2);
    axis([1 sizeX -10 10])   

    grid on 
    pause(0.1) 
    hold on 

    clf 

 
end 