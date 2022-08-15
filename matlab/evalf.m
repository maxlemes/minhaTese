function [f] = evalf(X)

	n = length(X);

	B = repelem(1/n,n);

	f = - B * log(X)';

	% f =0.5 * norm( A * x - B,'fro')^2;
end