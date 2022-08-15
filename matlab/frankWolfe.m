function [W,outiter,info, gopt, phi] = frankWolfe(alpha,X,G,gamma);

  % X is a point inside C
  % alpha is the step size in direction of G
  % G is the gradient vector
  % gamma is a projection paramenter in ]0,1]


  % set linear constraints of C, so set those arguments to [].
  A = [];
  b = [];
  Aeq = [];% repelem(1,n);
  beq = []; %[1];

  % set non-linear constrations in file constraints.m
  nonlcon = @nlconstraints;

  n = length(X);

  % Look within the region.
  lb = repelem(0.001,n);
  ub = repelem(1,n);

  % set your options
  options = optimoptions('fmincon','Display','iter','Algorithm','sqp','StepTolerance', 1e-14);

  % Set the point V
  V = X - alpha * G;

  % Set initial W
  W = X;

  % Define  linear FW function
  fun = @(z)(dot(W-V,z-W));

  % Parameters:
  maxoutiter = 1000*n;

  % Counters
  outiter = 0;

  %---------------------------------------------------------------------  
  %     Main loop
  %---------------------------------------------------------------------     

  while (1)
      
    % Compute Z and gopt
    % solve the non-linear problem.
    Z = fmincon(fun,W,A,b,Aeq,beq,lb,ub,nonlcon,options);
   

    gopt = sum(sum((W - V) .* (Z - W)));
      
    % Define phi  
    phi = gamma * norm(W - X,'fro' )^2;
         
    %  Test convergence
    if (outiter > 0 && - gopt <= phi )
        info = 0;
        return
    end
      
    if ( - gopt <= 10^(-8) )
        info = 1;
        return
    end

    % Test whether the number of iterations is exhausted
      if (outiter >= maxoutiter)
         info = 2;
         return
      end

    %-------------------------------------------------------------------
    %     Iteration
    %-------------------------------------------------------------------

      % Increment outiter
      outiter = outiter + 1;

      % Compute alpha
      stp = min(1.0 , - gopt / norm(Z - W,'fro' )^2 );

      % Update W
      W = W + stp * (Z - W);      
    %-------------------------------------------------------------------
    %     Iterate
    %-------------------------------------------------------------------    
  end
  %--------------------------------------------------------------------- 
  %     End of main loop
  %---------------------------------------------------------------------
end

