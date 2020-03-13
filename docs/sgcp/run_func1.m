clear;

addpath('gpml');
addpath('lightspeed');

load('func1.mat');
T = 50;

if 1
  seed = 0;
  randn('state', seed);
  rand('state', seed);
end

max_samples = 50000;
num_burnin  = 5000;
t           = linspace(0, T, 200)';

% Things to store.
lambdas      = zeros([size(t,1) max_samples]);
lambda_maxes = zeros([max_samples 1]);
gp_hypers    = zeros([2 max_samples]);
num_thinned  = zeros([max_samples 1]);

% The prior on the bounding lambda.
lambda_max_prior.alpha = 1;
lambda_max_prior.beta  = 1e-5;

% The GP hyperparameter prior.
gphp_prior_params.mean      = log([10 1]');
gphp_prior_params.cholSigma = diag([ 1 1 ]);

% The GP mean prior.
gp_mean_prior.mean  = 0;
gp_mean_prior.sigma = 10;

% Parameters for MCMC
mcmc_params.rej_attempts    = 50;
mcmc_params.gphp_tau        = 10;
mcmc_params.gphp_epsilon    = 0.0025;
mcmc_params.func_tau        = 100;
mcmc_params.func_epsilon    = 0.01;
mcmc_params.rej_proposal    = ...
    @(x1,x2) gaussian(x1,struct('mean',x2,'cholSigma', T/25));

% The initial state.
state = poisson_step( train, 0, T, mcmc_params, ...
                      'gp_func',  'covSEiso', ...
                      'gp_prior', @(x) gaussian(x, gphp_prior_params), ...
                      'gp_init',  gphp_prior_params.mean, ...
                      'gp_mean_prior', gp_mean_prior, ...
                      'gp_mean_init',  gp_mean_prior.mean, ...
                      'lambda_max_prior', lambda_max_prior, ...
                      'lambda_max_init', size(train,1)/T, ...
                      'gp_jitter', 1e-5);
                       
% Loop
for i=1:max_samples
  
  % Update the state.
  state = poisson_step( train, 0, T, mcmc_params, state);
  
  % Sample the function on a grid for plotting.
  chol_cov      = chol(feval(state.gp_covfunc{:}, ...
                             [state.gp_loghp;state.gp_logjit], ...
                             [train ; state.rej_locs]));
  kappa         = feval(state.gp_covfunc{:}, ...
                        [state.gp_loghp;state.gp_logjit], t);
  [tmp cross_k] = feval(state.gp_covfunc{:}, ...
                        [state.gp_loghp ; state.gp_logjit], ...
                        [train ; state.rej_locs], t);
  cross_solve   = solve_tril(chol_cov', cross_k);
  mean_solve    = solve_tril(chol_cov', ...
                             [state.data_func ; state.rej_func] ...
                             - state.gp_mean);
  pred_mean     = cross_solve' * mean_solve + state.gp_mean;
  pred_chol_cov = chol(kappa - cross_solve' * cross_solve);
  pred_func     = pred_chol_cov'*randn(size(t)) + pred_mean;
  lambda_func   = state.lambda_max./(1+exp(-pred_func));

  % Store things
  lambdas(:,i)    = lambda_func;
  lambda_maxes(i) = state.lambda_max;
  gp_hypers(:,i)  = state.gp_loghp;
  num_thinned(i)  = size(state.rej_func,1);

  if mod(i, 20) == 0
    
    mean_lambda = mean(lambdas(:,i/2:i),2);
    
    mse = trapz(t, (mean_lambda-func1(t)).^2);
    fprintf('%d] MSE: %0.4f\n', i, mse);
  
    plot(t, lambda_func, 'b-', ...
         t, mean_lambda, 'r-', ...
         t, func1(t), 'c-', ...
         train, zeros(size(train)), 'go', ...
         state.rej_locs, zeros(size(state.rej_locs)), 'rx');
    grid;
    legend('Current Sample', 'Predictive Mean', 'Truth', 'Data', 'Thinned', ...
      'Location', 'NorthOutside', 'Orientation', 'horizontal');
    pause(0.1);
  end
end
  
mean_lambda = mean(lambdas(:, num_burnin:end),2);

mse = trapz(t, (mean_lambda-func1(t)).^2);
fprintf('L2: %f\n', mse);

% estimate the log probs of the test data
volume = trapz(t, mean_lambda);
logprobs = zeros([10 1]);
for i=1:10
  
  log_lambdas = log(interp1(t, mean_lambda, test{i}));

  logprobs(i) = sum(log_lambdas) - volume;
  
end
fprintf('Mean Logprob: %f\n', mean(logprobs));

save('poisson1.mat');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
useful = lambdas(:,num_burnin:end);
probs  = zeros([size(useful,2) 1]);
for j=1:size(useful,2)
  
  % Get the area.
  area = trapz(t, useful(:,j));
  
  % Interpolate to the data (whatever).
  logprob = -10*area;
  for i=1:10
    logprob = logprob + sum(log(interp1(t, useful(:,j), test{i})));
  end
  
  probs(j) = logprob;

end
fprintf('Mean Predictive Logprob: %f\n', mean(probs/10));
