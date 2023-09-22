% Use the given eigenvalues and eigenvectors to reconstruct an output
% matrix. At most Nrfi of the largest eigenvalues will be nulled out,
% if they exceed a 3-sigma threshold.
% 
% For any eigenvector matrices that are singular i.e. not invertible,
% the output matrix will be a direct copy from the alldata input matrix.
%
% If some array elements are not connected or give no signal or their
% data should be ignored for other reasons, use the ignore_mask parameter.
% To use all elements use a blank ignore_mask=[];
%
% [nulldata]=subspcrfi_nulling(alldata, rfi_evalues, rfi_evectors, Nrfi, Mint, ignore_mask)
% Input: 
%   alldata = 2 x Nant x Nant x Nch       fall-back data
%   rfi_evalues = 2 x Nant x Nch          eigenvalues
%   rfi_evectors = 2 x Nant x Nant x Nch  eigenvectors
%   Nrfi = scalar                         max count of interferers to null
%   Mint = scalar                         nr of time-integrated covariances
%   ignore_mask = 1 x Nignore             list of eigenval indices to ignore
%
function [nulldata]=subspcrfi_nulling(alldata, rfi_evalues, rfi_evectors, Nrfi, Mint, ignore_mask)
  Nsets = size(alldata, 1);
  Nant = size(alldata, 2);
  Nch = size(alldata, 4);
  Nrfi_maxclip = Nrfi;
  
  v_verbose = 0;
  nulldata = zeros(size(alldata));
  
  for pp=1:Nsets,
    for cc=1:Nch,
        evalues = squeeze(rfi_evalues(pp,:,cc));
        evectors = squeeze(rfi_evectors(pp,:,:,cc));

        if (cond(evectors) == Inf),
            display('Warning: eigenvector matrix is singular! No correction will be done.');
            nulldata(pp,:,:,cc) = alldata(pp,:,:,cc);
            continue;
        end

        % Note: don't sort original eigenvalues here, otherwise we loose
        % the eigenvalue <=> eigenvector correspondency (would need
        % to re-order SVD or EVD eigenvector matrices accordingly!)
        
        % Use MDL to estimate 'Nrfi' dominant eigenvalues that could
        % be nulled, measured using only the non-ignored eigenvalues
        mdl_evalues = sort(evalues,2,'descend'); % these not used for nulling
        mdl_evalues(ignore_mask) = [];
        [Nrfi_mdl,tmp,MDL_k] = subspcrfi_MDLrank(mdl_evalues, Mint);
        
        % Use thresholding: exact, estimate, or remove all dominant
        [Nrfi_3sig, Nrfi_3mad] = subspcrfi_3Sigrank(abs(mdl_evalues));
        % Nrfi_thresh = Nrfi; % user-specified
        Nrfi_thresh = Nrfi_3sig; % 3-sigma
        %Nrfi_thresh = Nrfi_3mad; % 3-mad, 6-mad or other
        
        % Choose maximum of detected RFI values, clip to user-specified limit
        Nrfi_det = max(Nrfi_mdl, Nrfi_thresh);
        Nrfi = min(Nrfi_det, Nrfi_maxclip);
        fprintf(1, 'Channel %d 3sig=%d mdl=%d fin=%d\n', cc, Nrfi_thresh, Nrfi_mdl, Nrfi);

        % no RFI?
        %if (det_Nrfi>Nant/2),
        %    nulldata(pp,:,:,cc) = alldata(pp,:,:,cc);
        %    continue;
        %end

        % Prepare for nulling
        rfi_cnt = 0;
        null_idcs = zeros(1,Nrfi);
        null_evalues = evalues;
        
        % Loop through eigenvalues in one "find max()" out-of-Nrfi at a time.
        % The loop is used because eigenvalues need not be ordered.
        idx_dir = 1:Nrfi;
        if (null_evalues(1) < null_evalues(end)),
            idx_dir = Nrfi:-1:1;
        end
        for rii=idx_dir,
            [v,vi] = max(abs(null_evalues));
            rfi_cnt = rfi_cnt + 1;
            null_idcs(rfi_cnt) = vi;
            null_evalues(vi) = 0;
            if (v_verbose),
                fprintf(1,'Eig %d Ch#%d = %f > thr=%f\n', rfi_cnt, cc, v, rfi_thresh);
            end;
        end

        % compute the median, ignoring to-be-nulled eigenvalues
        shortened = null_evalues;
        shortened(null_idcs) = [];
        mm_post = median(shortened); % could also use mean()

        % "null" via replacing by median/mean (real nulling with 0.0f is sub-optimal)
        null_evalues(null_idcs) = mm_post;
        null_evalues = diag(null_evalues);

        if 0, %% debug
            % apply threshold to pruned input list of MDL
            thresh_evalues = mdl_evalues;
            thresh_evalues_idx = find(abs(thresh_evalues) >= rfi_thresh);
            thresh_evalues(thresh_evalues_idx) = 0;
            % apply thresh to full input eigenvalue list, null with median
            thresh_nulled = sort(evalues,2,'descend');
            thresh_nulled_idcs = (thresh_nulled >= rfi_thresh);
            thresh_nulled(thresh_nulled_idcs) = mm_post;
            % plotting
            figure(1), clf; subplot(3,1,1), hold on;
            plot(abs(mdl_evalues),'g');
            plot(abs(thresh_evalues),'b-.');
            plot(abs(thresh_nulled),'r.');
            line([Nrfi_mdl+1, Nrfi_mdl+1], ylim()); % line([x1 x2], [y1 y2]);
            legend('Eigenvalues input to MDL', 'Eigenvalues masked to zero by 3sigma', '3sigma nulled eigenvalue result', 'Line=MDL estimate');
            title(['Channel ' num2str(cc)]);
            subplot(3,1,2), hold on;
            thresh_nulled = sort(thresh_nulled,2,'descend');
            null_evalues_sorted = sort(diag(null_evalues),1,'descend');
            plot(abs(thresh_nulled),'g');
            plot(abs(null_evalues_sorted),'b-.');
            legend('Nulled eigenvalue result (after sort)', 'Matlab func eigenvalue result (after sort)');
            title(['Channel ' num2str(cc)]);
            subplot(3,1,3), hold on;
            plot(abs(evalues),'g');
            plot(abs(diag(null_evalues)),'b-.');
            legend('Original eigenvalues (no sorting)', 'Nulled eigenvalues (no sorting)');            
            %figure(2), plot(abs(MDL_k)), title('abs(MDL(k))');
            input('enter');
        end

        % finally, reconstruct original matrix 
        % (A = evecs * diag(evalues) * inv(evecs))
        null_mat = evectors * null_evalues * inv(evectors);
        nulldata(pp,:,:,cc) = null_mat;
    end
  end
