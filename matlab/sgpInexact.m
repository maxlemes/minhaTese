function [X,f,time,outiter,nfev,info] = sgpInexact(X)

    % Start timing
    tic;

    % Initial Parameters
    tol        = 10^(-6);  % tolerance of Step 1
    ftol       = 10^(-4);  % sigma in Step 0
    maxoutiter = 1000;


    stpmin     = 10^(-10);

    sigma0    = 0.5;
    sigma1     = 0.1;   % bw in Step 0
    sigma2     = 0.9;   % wb in Step 0

    alphamin   = 10^(-10);
    alphamax   = 10^(10);

    k          = 1;

    stopit = 2;

    gamma = 0.4999;


    % Counters
    outiter = 0;
    nfev    = 0;
    FWit    = 0;
    NumEig  = 0;

    % Compute the function value
    [f,flag] = sevalf(X);
    nfev = nfev + 1;

    if (flag ~= 0)
        info = -1;
        time = toc;
        return
    end

    % Compute the gradient
    [G,flag] = sevalg(X);

    if ( flag ~= 0 )
        info = -1;
        time = toc;
        return
    end

    % Define alpha
    normG = norm(G,'fro');
    alpha = min(alphamax, max(alphamin, 1.0 / normG));


    while (1)  
        
        % Project X - alpha * G onto the feasible set
        [W,outiterProj,infoProj] = frankWolfe(alpha,X,G,gamma);

        
        % Define the search direction
        d = W - X;
        
        normd = max(max(abs(d)));
        
        % Print information
        if (mod(outiter,10) == 0)
            fprintf('\n')
            fprintf('%-5s   %-8s   %2s     %-6s  %-3s    %-6s  %-8s %-8s\n', 'it','f','IS','nfev','k','||d||','|x-xprev|/|xprev|')
        end
        if (outiter == 0)
            fprintf('%5d   %5.2e   %2d   %6d   %3d   %6d   %8.2e     %1s\n', outiter,f,infoProj,nfev,k,normd,'-')
        else
            fprintf('%5d   %5.2e   %2d   %6d   %3d   %6d   %8.2e     %8.2e\n',  outiter,f,infoProj,nfev,k,normd,normXXprev)
        end

        % --------------------------------
        % Stopping criteria
        % --------------------------------
        
        % Test (unconstrained) optimality    
        %     if ( normG <= tol )
        %         info = 0;
        %         time = toc;
        %         
        %         % Print information
        %         
        %         fprintf('\n')
        %         fprintf('Solution was found.\n')
        %         fprintf('CPU time(s): %.1f \n',time)
        %         return
        %     end
        
        % Test whether the norm of d is too small   
        if ((infoProj == 0 || infoProj == 1) && normd <= tol)
            info = 1;
            time = toc;
            
            % Print information        
            fprintf('\n')
            fprintf('Solution was found.\n')
            fprintf('CPU time(s): %.1f \n',time)
            return
        end
        
        % Check the progress on the iterates    
        %     if ( outiter > 0 && normXXprev <= tol  )
        %         stop = stop + 1;
        %     else
        %         stop = 0;
        %     end
        %     
        %    if ( outiter > 0 && stop == stopit )
        %         
        %         info = 3;
        %         time = toc;
        %         
        %         % Print information
        %         
        %         fprintf('\n')
        %         fprintf('Solution was found.\n')
        %         fprintf('CPU time(s): %.1f \n',time)
        %         return
        %     end
        
        % Test whether the number of iterations is exhausted   
        if (outiter == maxoutiter)
            info = 4;
            time = toc;
            
            % Print information       
            fprintf('\n')
            fprintf('The number of maximum iterations was reached.\n')
            fprintf('CPU time(s): %.1f \n',time)
            
            return
        end

        % --------------------------------
        
        % Increment outiter    
        outiter = outiter + 1;
        
        % Compute the step size   
        gtd = G(:)' * d(:);
        
        stp = 1.0;
        
        while (1)
            
            Xtrial = X + stp * d;
            
            [ftrial,flag] = sevalf(Xtrial);
            nfev = nfev + 1;
            
            if (flag ~= 0)
                info = -1;
                time = toc;
                return
            end

            if (ftrial <= f + ftol * stp * gtd)
                break
            end
            
            if (stp <= stpmin) 
                break
                disp('Warning: stp = stpmin in the backtracking procedure')
            end
    			
            stpq = ((gtd / ((f - ftrial) / stp + gtd)) / 2.0) * stp;

            if (stpq >= sigma1 * stp && stpq <= sigma2 * stp )
                stp = stpq;
            else
                stp = stp / 2.0;
            end
            
        end
        
        % Update X
        Xprev = X;
        X = Xtrial;

        
        % Compute norm(X,Xprev)/norm(Xprev) 
        normXXprev = norm(X - Xprev,'fro')/norm(Xprev,'fro');
        
        % Compute the function value
        f = ftrial;

        % Compute the gradient
        Gprev = G;

        [G,flag] = sevalg(X);
        
        if ( flag ~= 0 )
            info = -1;
            time = toc;
            return
        end

        normG = norm(G,'fro');
        

        % Define alpha
        s = X - Xprev;
        y = G - Gprev;


        
        a = s(:)' * s(:);
        b = s(:)' * y(:);

        
        if (b <= 10^(-12) )
            alpha = alphamax;
        else
            alpha = min(alphamax, max(alphamin, a / b ) );
        end  
    end    
end