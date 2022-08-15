% Test od fmincon non-linear case

function [X] = algoMVP(i)

	global Cov n;

	% Define your function
	fun = @(X)(X * Cov * X');

	% There are no linear constraints, so set those arguments to [].
	A = [];
	b = [];
	Aeq = repelem(1,n);
	beq = [1];

	% Choose an initial point satisfying all the constraints.
	X0 = repelem(1/n,n);
	%x0=eye(n,1)./n';

	% Look within the region.
	lb = repelem(0,n);
	ub = repelem(1.00001,n);

	% non linear constrations
	nonlcon = []; 

	options = optimoptions('fmincon','Display','iter','Algorithm','sqp');

	X = fmincon(fun,X0,A,b,Aeq,beq,lb,ub,nonlcon,options);
end