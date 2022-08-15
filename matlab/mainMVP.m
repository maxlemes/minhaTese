clf;
clc;
clear all;

global Cov v0 n;

% read the number of files in folder
dRet = dir('dRet');
m = length(dRet)-2;

% delete old files
delete '../R/data-raw/pesos/mvp/*'

for i = 1:m

  [Cov] = data(i);

  X = algoMVP(i); 

  X = X./(repelem(1,n)*X')

	if i < 10

		filename = strcat('../R/data-raw/pesos/mvp/mvp0',num2str(i),'.csv');
	else
	    
		filename = strcat('../R/data-raw/pesos/mvp/mvp',num2str(i),'.csv');
	end

	csvwrite(filename, X);
end

