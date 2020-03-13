function state = poisson_step(X, A0, A1, mcmc, varargin)

% Basic information
debug  = 0;
D      = cols(X);
N      = rows(X);
volume = prod(A1-A0);

if nargin == 5
  
  % Use the state we have been given.
  state = varargin{1};

  % Calculate the covariance function and cholesky decomp.
  chol_cov = chol(feval(state.gp_covfunc{:}, ...
                        [ state.gp_loghp ; state.gp_logjit ], ...
                        [ X ; state.rej_locs ]));

  M = rows(state.rej_locs);

else
  
  % Get the initialization parameters.
  init = cell2struct({varargin{2:2:end}}, {varargin{1:2:end}}, 2);
  
  % Initialize the GP parameters.
  state.gp_covfunc    = {'covSum', {init.gp_func, 'covNoise'}};
  state.gp_loghp      = init.gp_init;
  state.gp_prior      = init.gp_prior;
  state.gp_logjit     = log(init.gp_jitter);
  state.gp_mean       = 0;
  state.gp_mean_prior = init.gp_mean_prior;
  
  % Initialize the bounding lambda.
  state.lambda_max_prior = init.lambda_max_prior;
  state.lambda_max       = init.lambda_max_init;
    
  % Initialize the rejection sets.
  if 1
    state.rej_locs = [];
    state.rej_func = [];  
    M = 0;
  else
    initial = N;
    state.rej_locs = (rand([D initial]).*repmat(A1-A0, [1 initial]) ...
                      + repmat(A0, [1 initial]))';
    M = initial;
  end
  
  % Calculate the covariance function and cholesky decomp.
  chol_cov = chol(feval(state.gp_covfunc{:}, ...
                        [ state.gp_loghp ; state.gp_logjit ], ...
                        [ X ; state.rej_locs ]));
  
  % Sample the function initially from the GP.
  func            = chol_cov'*randn([N+M 1]) + state.gp_mean;
  state.data_func = func(1:N);
  state.rej_func  = func(N+1:end);
  
end


% Modify locations of the rejections.
update_rejections( mcmc.rej_proposal );

% Modify the GP hyperparameters with HMC.
update_gphp( mcmc.gphp_tau, mcmc.gphp_epsilon );

% Modify the GP mean.
%update_gp_mean();

% Modify the function values with HMC.
update_function( mcmc.func_tau, mcmc.func_epsilon );

% Propose inserting or removing rejections.
for i=1:mcmc.rej_attempts
  p = insert_prob(M,N);
  if rand() < p
    insert_rejection();
  elseif M > 0
    remove_rejection();
  end
end

% Modify the bounding lambda.
update_lambda_max();

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function update_rejections( prop_func )
if M > 0
  % Loop over the rejections.
  for m=1:M
    
    % Rejection sample a new location inside the bounding box.
    % This is like "two-step Metropolis" to make things faster.
    while 1
      new_X = prop_func([], state.rej_locs(m,:)')';
      if all(new_X' > A0) && all(new_X' < A1)
        break;
      end
    end
    
    % Find the predictive distribution at this location.
    [kappa cross_k] = feval(state.gp_covfunc{:}, ...
                            [state.gp_loghp ; state.gp_logjit], ...
                            [X ; state.rej_locs], ...
                            new_X);
    cross_solve = solve_tril(chol_cov', cross_k);
    mean_solve  = solve_tril(chol_cov', ...
                             [state.data_func ; state.rej_func] ...
                             - state.gp_mean);
    pred_mean   = cross_solve' * mean_solve + state.gp_mean;
    pred_var    = kappa - cross_solve' * cross_solve;
  
    % Draw the function at this new location.
    new_func = sqrt(pred_var)*randn() + pred_mean;
    
    
    % Evaluate the ratio of proposal distributions.
    log_prop_ratio = prop_func(state.rej_locs(m,:)', new_X') ...
        - prop_func(new_X', state.rej_locs(m,:)');
    
    % Evaluate the likelihood ratios.
    log_lh_ratio = log(1 + exp(state.rej_func(m))) ...
        - log(1+exp(new_func));
    
    % Accept or reject this move.
    if log(rand()) < (log_prop_ratio + log_lh_ratio)
      
      % Store these new values.
      state.rej_locs(m,:) = new_X;
      state.rej_func(m)   = new_func;
      
      % Recompute the Cholesky decomposition.  This could almost
      % certainly be done faster with a rank-one update.  The GP
      % hyperparameter sampling is the really slow thing right now,
      % though.
      chol_cov = chol(feval(state.gp_covfunc{:}, ...
                      [ state.gp_loghp ; state.gp_logjit ], ...
                      [ X ; state.rej_locs ]));
    end
    
  end
end
end % update_rejections

function update_function( tau, epsilon )

% Transform the function to its whitened counterpart.
white_func = solve_tril(chol_cov', ...
                        [state.data_func ; state.rej_func] - ...
                        state.gp_mean);

if debug
  % Always good to verify gradients.
  checkgrad(white_func, 1e-4)
end

% Get the initial energy.
init_E = energy(white_func);

% Sample momenta.
P = randn(size(white_func));

% Calculate the initial Hamiltonian.
init_H = init_E + P'*P/2;

% Calculate the gradient
G = gradient(white_func);

% Store the initialization, for distance calculation.
init_wf = white_func;

% Take tau steps.
for t=1:tau
  
  % Take a half-step in the momenta.
  P = P - epsilon * G / 2;
  
  % Take a step in the parameter space.
  white_func = white_func + epsilon * P;
  
  if debug
    % Calculate the distance and report.  
    distance = sqrt(sum((white_func-init_wf).^2));
    fprintf('%d - Distance: %f\n', t, distance);
  end
  
  % Reevaluate the gradient.
  G = gradient(white_func);
  
  % Take another half-step in the momenta.
  P = P - epsilon * G / 2;
end

% Evaluate the new energy.
new_E = energy(white_func);

% Calculate the new Hamiltonian.
new_H = new_E + P'*P/2;

if debug
  fprintf('Old Ham: %f  New Ham: %f  Prob: %f\n', init_H, new_H, ...
          exp(init_H - new_H));
end

% Decide whether to accept or reject.
if rand() < exp(init_H - new_H)
  % Update the overall state.
  func            = chol_cov' * white_func + state.gp_mean;
  state.data_func = func(1:N);
  state.rej_func  = func(N+1:end);
end

function E = energy(white_func)

% Unwhiten the data.
func      = chol_cov' * white_func + state.gp_mean;
data_func = func(1:N);
rej_func  = func(N+1:end);

% Calculate the log prior probability under the GP.
lp_gp = -0.5*white_func'*white_func;

% Calculate the log probability associated with the data.
lp_data = -sum(log(1 + exp(-data_func)));

% Calculate the log probability associated with the rejections.
lp_rej = -sum(log(1 + exp(rej_func)));

E = -(lp_gp + lp_data + lp_rej);    
end % update_function:energy

function G = gradient(white_func)

% Unwhiten the data.
func      = chol_cov' * white_func + state.gp_mean;
data_func = func(1:N);
rej_func  = func(N+1:end);

% Get the gradient of the log prior under the GP.
dlp_gp = - white_func;

% Get the gradient of the accepted data.
dlp_data = 1 ./ (1 + exp(data_func));

% Get the gradient of the rejections.
dlp_rej = - 1 ./ (1 + exp(-rej_func));

G = - (dlp_gp + chol_cov*[ dlp_data ; dlp_rej ]);
end % update_function:gradient
  
function norm = checkgrad(white_func, epsilon)
norm = 0;
grad = gradient(white_func);
for i=1:rows(white_func)      
  white_func(i) = white_func(i) - 0.5*epsilon;
  E0 = energy(white_func);
  white_func(i) = white_func(i) + epsilon;
  E1 = energy(white_func);
  white_func(i) = white_func(i) - 0.5*epsilon;
  
  diff_grad = (E1 - E0)/epsilon;
  
  norm = norm + abs(diff_grad - grad(i));
  
  fprintf('%f vs %f\n', diff_grad, grad(i));
end
end % update_function:checkgrad
end % update_function

function update_gphp(tau, epsilon) 
x = [ X ; state.rej_locs ];
y = [ state.data_func ; state.rej_func ];
  
% Calculate the negative log marginal likelihood and gradient.
[nlml dnlml] = gpr([state.gp_loghp;state.gp_logjit], ...
                   state.gp_covfunc, x, y);
                 
% Calculate the prior and gradient.
[logprior dlogprior] = state.gp_prior(state.gp_loghp);

% Instantiate momenta.
P = randn(size(state.gp_loghp));

% Calculate the Hamiltonian.
init_H = P'*P/2 + nlml - logprior;

% Calculate the gradient.
G = dnlml(1:end-1) - dlogprior;

% Initialize
loghp = state.gp_loghp;

% Take tau steps.
for t=1:tau
  
  % Take a half-step in momentum space.
  P = P - epsilon * G/2;
  
  % Take a step in parameter space.
  loghp = loghp + epsilon * P;
  
  if debug
    % Calculate the distance.
    distance = sqrt(sum((loghp-state.gp_loghp).^2));
    fprintf('%d - Distance %f\n', t, distance);
  end
  
  % Reevaluate the gradient.
  [nlml dnlml]         = gpr([loghp;state.gp_logjit], ...
                             state.gp_covfunc, x, y);
  [logprior dlogprior] = state.gp_prior(loghp);
  G = dnlml(1:end-1) - dlogprior;
  
  % Take another half-step in momentum space.
  P = P - epsilon * G/2;
end

% Evaluate the new Hamiltonian.
[nlml dnlml]         = gpr([loghp;state.gp_logjit], ...
                           state.gp_covfunc, x, y);
[logprior dlogprior] = state.gp_prior(loghp);

new_H = P'*P/2 + nlml - logprior;

if debug
  fprintf('Old Ham: %f  New Ham: %f  Prob: %f\n', init_H, new_H, ...
          exp(init_H - new_H));
end

% Accept or reject.
if rand() < exp(init_H - new_H)
  
  % Update the state.
  state.gp_loghp = loghp;
  
  % Recompute the Cholesky decomposition.
  chol_cov = chol(feval(state.gp_covfunc{:}, ...
                        [state.gp_loghp;state.gp_logjit], ...
                        [X ; state.rej_locs]));
end

end % update_gphp

function update_gp_mean
% Get the mean of the function.
func_sum = sum([state.data_func;state.rej_func]);

% Get the GP variance (assuming stationarity).
gp_var = feval(state.gp_covfunc{:}, ...
               [state.gp_loghp;state.gp_logjit], ...
               zeros([1 D]));

% Get the conditional posterior parameters.
new_precision = 1/state.gp_mean_prior.sigma^2 + (N+M)/gp_var;
new_mean = ( state.gp_mean_prior.mean / state.gp_mean_prior.sigma^2 ...
             + func_sum/gp_var) / new_precision;

% Draw the new GP mean.
state.gp_mean = sqrt(1/new_precision)*randn() + new_mean;

end % update_gp_mean

function insert_rejection

% Draw uniformly on the space.
new_X = (rand([D 1]).*(A1-A0) + A0)';

% Find the predictive distribution at this location.
[kappa cross_k] = feval(state.gp_covfunc{:}, ...
                        [state.gp_loghp ; state.gp_logjit], ...
                        [X ; state.rej_locs], ...
                        new_X);
cross_solve = solve_tril(chol_cov', cross_k);
mean_solve  = solve_tril(chol_cov', ...
                         [state.data_func ; state.rej_func] ...
                         - state.gp_mean);
pred_mean   = cross_solve' * mean_solve + state.gp_mean;
pred_var    = kappa - cross_solve' * cross_solve;

% Draw the function at this new location.
new_func = sqrt(pred_var)*randn() + pred_mean;

% The proposal ratio: probability of selecting this one for the reverse move.
prop_ratio = (1 - insert_prob(M+1,N))/((M+1)*insert_prob(M,N));

% The ratio for the Poisson likelihood.
poisson_ratio = state.lambda_max*volume;

% The probability of rejecting this point.
rejection_ratio = 1/(1+exp(new_func));

% Determine the acceptance ratio.
accept_ratio = prop_ratio*poisson_ratio*rejection_ratio;

if debug
  fprintf('%d: Insertion Acceptance Ratio: %f x %f x %f = %0.4f\n', ...
          M, prop_ratio, poisson_ratio, rejection_ratio, accept_ratio);
end

% Accept or reject
if rand() < accept_ratio
    
  % Add in this rejection.
  state.rej_locs = [ state.rej_locs ; new_X ];
  state.rej_func = [ state.rej_func ; new_func ];
  
  % Recompute the Cholesky decomposition.  Could use chol_expand to make
  % it faster.
  chol_cov = chol(feval(state.gp_covfunc{:}, ...
                        [state.gp_loghp;state.gp_logjit], ...
                        [X ; state.rej_locs]));
  
  M = M + 1;
end

end % insert_rejection

function remove_rejection

% Select one at random.
index = ceil(rand()*M);
 
% The proposal ratio.
prop_ratio = (M*insert_prob(M-1,N))/(1-insert_prob(M,N));

% The Poisson likelihood.
poisson_ratio = 1/(state.lambda_max*volume);

% The inverse probability of rejecting this point.
rejection_ratio = 1 + exp(state.rej_func(index));

% Evaluate the acceptance ratio.
accept_ratio = prop_ratio*poisson_ratio*rejection_ratio;

if debug
  fprintf('%d: Rejection Acceptance Ratio: %f x %f x %f = %0.4f\n', ...
          M, prop_ratio, poisson_ratio, rejection_ratio, accept_ratio);
end

if rand() < accept_ratio
      
  state.rej_locs(index,:) = [];
  state.rej_func(index)   = [];
  
  % Recompute the Cholesky decomposition.
  chol_cov = chol(feval(state.gp_covfunc{:}, ...
                        [state.gp_loghp;state.gp_logjit], ...
                        [X ; state.rej_locs]));
  
  M = M - 1;
end
end % remove_rejection

function update_lambda_max
% Get the conditional posterior parameters.
new_alpha = state.lambda_max_prior.alpha + M + N;
new_beta  = state.lambda_max_prior.beta  + volume;

% Resample using the (non-inverse) scale parameterization.
state.lambda_max = gamrnd(new_alpha, 1/new_beta);

end % update_lambda_max

function p = insert_prob(m, n)
p = 0.5;
%p = normcdf((m+n - state.lambda_max*volume)/sqrt(state.lambda_max*volume));
end % insert_prob

  function r=rows(X) 
    r = size(X,1);
  end

  function c=cols(X)
    c = size(X,2);
  end
end % poisson_step
