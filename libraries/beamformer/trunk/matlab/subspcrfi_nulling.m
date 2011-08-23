% Use the given eigenvalues and eigenvectors to reconstruct an output
% matrix. At most Nrfi of the largest eigenvalues will be nulled out,
% if they exceed a 3-sigma threshold.
% 
% For any eigenvector matrices that are singular i.e. not invertible,
% the output matrix will be a direct copy from the alldata input matrix.
%
% Input: 
%   alldata = 2 x Nant x Nant x Nch       fall-back data
%   rfi_evalues = 2 x Nant x Nch          eigenvalues
%   rfi_evectors = 2 x Nant x Nant x Nch  eigenvectors
%   Nrfi = scalar                         max count of interferers to null
%
function [nulldata]=subspcrfi_nulling(alldata, rfi_evalues, rfi_evectors, Nrfi)
  Nsets = size(alldata, 1);
  Nant = size(alldata, 2);
  Nch = size(alldata, 4);
  
  v_verbose = 0;
  nulldata = zeros(size(alldata));
  
  for pp=1:Nsets,
    for cc=1:Nch,
        null_evalues = squeeze(rfi_evalues(pp,:,cc));
        null_evectors = squeeze(rfi_evectors(pp,:,:,cc));

        if (cond(null_evectors) == Inf),
            display('Warning: eigenvector matrix is singular! No correction will be done.');
            nulldata(pp,:,:,cc) = alldata(pp,:,:,cc);
            continue;
        end

        % find at most 'Nrfi' dominant eigenvalues that could be nulled
        [det_Nrfi,sorted_evalues] = subspcrfi_MDLrank(null_evalues, 1024);
        fprintf(1, 'Set %d Ch %d MDL=%d\n', pp, cc, det_Nrfi);        

        % no RFI?
        %if (det_Nrfi>Nant/2),
        %    nulldata(pp,:,:,cc) = alldata(pp,:,:,cc);
        %    continue;
        %end
        
        % threshold: exact, estimate, or remove all dominant
        %rfi_thresh = sorted_evalues(min(Nrfi, det_Nrfi)); % exact/clipped
        rfi_thresh = mean(abs(null_evalues)) + 3*mean(std(null_evalues));
        %rfi_thresh = 0; %always remove dominant

        rfi_cnt = 0;
        null_idcs = zeros(1,Nrfi);
        % loop through eigenvalues in descending order
        idx_dir = 1:Nrfi;
        if (null_evalues(1)<null_evalues(end)),
            idx_dir = Nrfi:-1:1;
        end
        for rii=idx_dir,
            [v,vi] = max(abs(null_evalues));
            if (v >= rfi_thresh),
                rfi_cnt = rfi_cnt + 1;
                null_idcs(rfi_cnt) = vi;
                null_evalues(vi) = min(null_evalues); % will be overwritten later by mean
                if (v_verbose),
                    fprintf(1,'Eig %d Ch#%d = %f > thr=%f\n', rfi_cnt, cc, v, rfi_thresh);
                end;
            end
        end

        % compute the median, ignoring to-be-nulled eigenvalues
        shortened = null_evalues;
        shortened(null_idcs(1:rfi_cnt)) = [];
        mm_post = median(shortened); % could also use mean()

        % "null" via replacing by median/mean (real nulling with 0.0f is sub-optimal)
        null_evalues(null_idcs(1:rfi_cnt)) = mm_post;
        null_evalues = diag(null_evalues);

        % finally, reconstruct original matrix 
        % (A = evecs * diag(evalues) * inv(evecs))
        null_mat = null_evectors * null_evalues * inv(null_evectors);
        nulldata(pp,:,:,cc) = null_mat;
    end
  end
