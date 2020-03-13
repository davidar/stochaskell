function varargout = gaussian(x, params)
  
  if nargin == 0    
    varargout(1) = { @gibbs_gaussian };
    return;
  end
  
  if ~isfield(params, 'cholSigma')
    cholSigma = chol(params.Sigma);
  else
    cholSigma = params.cholSigma;
  end

  if isempty(x)
    
    % Return samples
    for i=1:nargout
      varargout(i) = {cholSigma' * randn(size(params.mean)) + params.mean};
    end      
  else
    
    % Return the log probability of x.
    varargout(1) = {normpdfln(x, params.mean, cholSigma)};
    
    % Perhaps also return the derivative.
    if nargout > 1
      
      varargout(2) = {-solve_triu(cholSigma, ...
                                  solve_tril(cholSigma',x-params.mean))};
      
    end
    
  end

function new_params = gibbs_gaussian(params, prior, data)
  new_params = params;
  
  N     = rows(data);
  xbar  = mean(data,1)';
  dS    = data - repmat(xbar', [N 1]);
  S     = (dS'*dS)/N;
   
  kappa_n = prior.kappa + N;
  nu_n    = prior.nu    + N;
  mu_n    = (prior.kappa*prior.mu + N*xbar)/kappa_n;
  S_n     = prior.S + N*S + ((prior.kappa*N)/(prior.kappa+N)) ...
            * (xbar - prior.mu)*(xbar-prior.mu)';
    
  new_Sigma            = iwishrnd(S_n, nu_n);
  new_params.cholSigma = chol(new_Sigma);
  new_params.mean      = (new_params.cholSigma'/kappa_n)*randn(size(xbar)) ...
      + mu_n;
