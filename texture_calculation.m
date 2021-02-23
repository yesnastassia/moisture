function homogeneity= texture_calculation(input_image, qty)
%% a function to calculate the homogeneity for "qty" user input plots
    homogeneity=zeros(qty,1);

    for n=1:qty
        imshow(input_image)
        [x,y]=ginput(2);
        x=int64(x);
        y=int64(y);
        %%is is higher moving right, y is higher moving down
        %%this function requires you input the upper left corner and then
        %%the lower right
        calc=graycoprops(input_image(x(1):x(2),y(1):y(2)),'homogeneity');
        homogeneity(n)=calc.Homogeneity;
    end
end
