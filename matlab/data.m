
function [Cov] = data(i)

	global n v0; %define n and v0

	%------------------ Returns --------------------
	% read folder files
	dRet = dir('dRet');

	% selecting desired file
	filename = strcat('dRet','/',dRet(i+2).name);

	% read vector file
	Ret = csvread(filename, 1);

	%------------------ Volatility -----------------

	% read folder files
	dStdev = dir('dStdev');

	% selecting desired file
	filename = strcat('dStdev','/',dStdev(i+3).name);

	% read vector file
	vol = csvread(filename, 1);

	%------------ Covariance Matrix -----------------

	dCov= dir('dCov');

	filename = strcat('dCov','/',dCov(i+3).name);

	Cov = csvread(filename, 1,0);

	%------------ Initial Value  ---------------------

	n = length(vol);

	v0 = 0.01;
end



