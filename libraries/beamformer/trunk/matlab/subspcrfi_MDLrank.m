% Use MDL detector to make a guess on the number of eigenvalues
% that are above an unknown noise power threshold.
% This can work reasonably but requires more than N/2 of
% eigenvalues are indeed from noise space.
%
% From http://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=1164557
% evtest=[21.2359 2.1717 1.4279 1.0979 1.0544 0.9432 0.7324];
% subspcrfi_MDLrank(evtest, 100);
% should give: 
%  AIC=1e3*[1.1808 0.1005 0.0714 0.0755 0.0868 0.0932 0.0960], rank=2+1
%  MDL=[590.4 67.2 66.9 80.7 95.5 105.2 110.5], rank=2+1

function [det_Nrfi,sorted_evalues,MDL_k]=subspcrfi_MDLrank(evalues, M_samps)
    v_verbose = 0;
    Nsig = max(size(evalues));

    M = 3*Nsig; % how many samples were averaged into the Rxx estimate
    if (nargin==2),
        M = M_samps;
    end
    
    % q interferers in p total signals, q<p
    % lambda{1..k}
    % must be sorted so that lambda(1)>lambda(2)>...>lambda(p)
    % taking geo and artih mean of (p-k) smallest eigenvalues    
    if (size(evalues,2)>size(evalues,1)),
        evalues = transpose(evalues);
    end
    sorted_evalues = sort(evalues,1,'descend');
    
    MDL_k = zeros(Nsig, 1);
    for kk=0:(Nsig-1),
        Q = Nsig - kk;
        data = sorted_evalues((kk+1):end);
        arith = sum( data ./ Q);
        geo = prod( data .^ (1/Q));
        
        L = geo/arith; % may reach NaN/Inf quickly!
        
        MDL_k(kk+1) = -Q*M*log(L) + 0.5*kk*(2*Nsig - kk)*log(M);
        
        if (isnan(MDL_k(kk+1)) && v_verbose),
            fprintf(1, 'Warning: MDL(Nrfi=%d) had geometric mean %f, MDL overflow!\n', nn, geo);
        end
    end
    
    [minMDL,minIdx] = min(MDL_k);
    det_Nrfi = minIdx - 1;
    
    