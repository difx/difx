% Use MDL detector to make a guess on the number of eigenvalues
% that are above an unknown noise power threshold.
% This can work reasonably but requires more than N/2 of
% eigenvalues are indeed from noise space.

% From http://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=1164557
% evtest=[21.2359 2.1717 1.4279 1.0979 1.0544 0.9432 0.7324];
% subspcrfi_MDLrank(evtest, 100);
% should give: 
%  AIC=1e3*[1.1808 0.1005 0.0714 0.0755 0.0868 0.0932 0.0960], rank=2+1
%  MDL=[590.4 67.2 66.9 80.7 95.5 105.2 110.5], rank=2+1

function [det_Nrfi,sorted_evalues]=subspcrfi_MDLrank(evalues, M_samps)
    v_verbose = 0;

    M = 1024; % how many samples were averaged into the Rxx estimate
    if (nargin==2),
        M = M_samps;
    end
    
    Nsig = max(size(evalues));
    det_Nrfi = Nsig;

    % q interferers in p total signals, q<p
    % lambda{1..k}
    % must be sorted so that lambda(1)>lambda(2)>...>lambda(p)
    % taking geo and artih mean of (p-k) smallest eigenvalues    
    if (size(evalues,2)>size(evalues,1)),
        evalues = transpose(evalues);
    end
    if (evalues(1)<evalues(2)),
        fprintf(1, 'Warning: re-sorting eigenvalues to be in descending order\n');
    end
    evalues = sort(evalues,1,'descend');
    sorted_evalues = evalues;
    
    min_MDL = Inf;
    IClist = zeros(1, Nsig);
    for kk=0:(Nsig-1),
        Q = Nsig - kk;
        arith = sum(evalues((kk+1):end)) / Q;
        geo = prod(evalues((kk+1):end)) ^ (1/Q); % this overflows to NaN very easily!
        mdl = -Q*M*log(geo/arith) + 0.5*kk*(2*Nsig - kk)*log(M);
        IClist(kk+1) = mdl;
        if (mdl < min_MDL),
            min_MDL = mdl; det_Nrfi = kk + 1;
        end
        if (isnan(mdl) && v_verbose),
            fprintf(1, 'Warning: MDL(Nrfi=%d) had geometric mean %f, MDL overflow!\n', nn, geo);
        end
    end
    % IClist
    