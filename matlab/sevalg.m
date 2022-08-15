function [G,flag] = sevalg(X) 

	n = length(X);

	flag = 0;

	[G] = evalg(X);

    for i = 1:n
        [isnum] = IsANumber( G(i) );

        if ( isnum == 0 )
            flag = -1;
            disp('WARNING: There is an element whose value may be +Inf, -Inf or NaN')
            disp('in the gradient of the objective computed by the user-supplied')
            disp('subroutine evalg.')
        end
    end
end
