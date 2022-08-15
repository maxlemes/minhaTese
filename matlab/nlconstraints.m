% Copyright 2021 Max Lemes

function [c, ceq] = nlconstraints(X)
  global Cov v0

  % Variance less than v0
  c = X * Cov * X' - v0;

  ceq = [];
end
