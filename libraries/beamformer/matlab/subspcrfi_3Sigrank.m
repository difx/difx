% Three sigma -thresholded estimate for classifying values in
% a SVD/EVD decomposition eigenvalue list into just noise and
% into significant model parameters.
% Returns estimated rank of corresponding eigenvector matrix.
%
% [N_mean, N_median]=subspcrfi_3Sigma_rank(evalues)
%
% evalues = N x 1 = vector with eigenvalues from EVD or SVD
% N_med = scalar
% N_mea = scalar
%
function [N_mean, N_median]=subspcrfi_3Sigrank(evalues)
    N = max(size(evalues));
    
    ev_mean = mean(evalues);
    ev_stddev = sqrt((sum((evalues-ev_mean).^2))/(N-1)); % ==std(evalues)

    ev_median = median(evalues);
    ev_mad = median(abs(evalues - ev_median)); % ==mad()=median abs deviation

    thresh_mean = ev_mean + 3*ev_stddev;
    thresh_median = ev_median + 3*ev_mad;
        
    N_mean = sum(evalues >= thresh_mean);
    N_median = sum(evalues >= thresh_median);
    
    if (0),
        fprintf(1, 'Mean: m=%f s=%f N=%d  ; Median: m=%f s=%f N=%d\n', ...
                ev_mean, ev_stddev, N_mean, ...
                ev_median, ev_mad, N_median);
        figure(1), clf, hold on, 
        plot(evalues,'g-'), 
        plot(evalues.* (evalues>thresh_median), 'r-.');
        plot(evalues.* (evalues>thresh_mean), 'b-.');
        legend('Original','Bool mask Median','Bool mask Mean');
        input('enter');
    end
    
    