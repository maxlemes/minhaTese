function [g] = evalg(X)

	n = length(X);

	B = repelem(1/n,n);

	g = -B./X;

	% f =@(x)( 0.5 * norm( A * x - B,'fro')^2);
end