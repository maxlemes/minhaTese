clf;
clc;
clear all;

global Cov n; %define Cov, read n

% read the number of files in folder
dStdev = dir('dStdev');
m = length(dStdev)-3;

% delete old files
delete '../R/data-raw/pesos/rpp/*'


for i = 1:m

  [Cov] = data(i);

  X = repelem(1/n,n);	

  [X,f,time,outiter,nfev,info] = sgpInexact(X);

  X = X./(repelem(1,n)*X');


  if i < 10

    filename = strcat('../R/data-raw/pesos/rpp/rpp0',num2str(i),'.csv');

    else
    
    filename = strcat('../R/data-raw/pesos/rpp/rpp',num2str(i),'.csv');

  end

  csvwrite(filename, X);

  V = (Cov*X')';

  sigma = X*Cov*X';
  sigma = sqrt(sigma);

  volatility = X.*V/sigma;

  sig=repelem(sigma/n,n);

  erro = norm(sig - volatility);
end

