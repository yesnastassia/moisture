m=120;
NIR_vals=zeros(m,2);

NIR_AM = imread('InkedNIR_AM_nooutliers.jpg');
NIR_vals(:,1)=texture_calculation(NIR_AM,m);

NIR_PM = imread('InkedNIR_PM_nooutliers.jpg');
NIR_vals(:,2)=texture_calculation(NIR_PM,m);

writematrix(NIR_vals,'texture_vals.csv') 