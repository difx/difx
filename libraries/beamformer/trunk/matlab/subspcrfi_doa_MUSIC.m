% Uses MUSIC algorithm to attempt and detect the directions of
% arrival of RFI signals. A signals is RFI if its correpsonding eigenvalue
% is above 3*std(eigenvalues)+mean(eigenvalues).
%
% Input: evalues = Nsets x Nant x Nch           eigenvalues of Rxx
%        evectors = Nsets x Nant x Nant x Nch   corresonding eigvectors
%        channelHz = 1 x Nch                    frequencies in Hertz
%        elemXYZ = 3 x Nant                     element locations in meters
%
% Output: doa = Nsets x Nchannels x K x 2   the 3D direction of arrivals
%                                           where K depends on number of
%                                           found RFI
%
function [doa]=subspcrfi_doa_MUSIC(evalues, evectors, channelHz, elemXYZ)
  Nsets = size(evalues, 1);
  Nant = size(evalues, 2);
  Nch = size(evalues, 3);
  
  doa = zeros(Nsets, Nch, 2);
  for pp=1:Nsets,
    for cc=1:Nch,
        evals = squeeze(evalues(pp,:,cc));
        evecs = squeeze(evectors(pp,:,:,cc));
        
        % MUSIC algorithm:
        % 1. estimation of correlation matrix R
        % 2. eigendecomposition into Q*L*Q
        % 3. partitioning of Q into [Qm|Qn] where Qn matrix columns
        %    have the eigenvectors of the noise subspace i.e. Qn
        %    columns correspond to the lowest eigenvalues of Q,
        %    while the M interferers are in Qm
        % 4. plot of power P(phi) over 0..180 degrees (1D array)
        %    P(phi)=1/(herm(s(phi)) * Qn*herm(Qn) *s(phi))
        % 5. locate the M maxima in the P(phi) plot

        % Partitioning step, prune all strong signals from matrix
        rfi_thresh = abs(mean(evals)) + 3*mean(std(evals));
        Mrfi = 0; Qn = evecs;
        for rii=1:Nant,
            [v,vi] = max(abs(evals));
            if (v > rfi_thresh),
                Mrfi = Mrfi + 1;
                Qn(:,vi) = []; 
                evals(vi) = [];
                fprintf(1, 'Ch#%d RFI#%d eig[%d]=%f > thr=%f : removed\n', cc, Mrfi, vi, v, rfi_thresh);
            end
        end
        
        if (Mrfi<1),
            fprintf(1, 'No RFI detected in Ch#%d\n', cc);
            %continue;
        end
        
        % Set of angles (phi,theta) over which to compute power
        %   http://www.radionet-eu.org/fp7wiki/lib/exe/fetch.php?media=na:engineering:ew:bakker_ew2_final.pdf
        %   Beamformed beam pattern in Fig4 was done by steering vector
        %   that was {-1.5:0.15:+1.5} X {-1.5:0.15:+1.5} off-zenith.
        angle_range_deg = (-15:0.15:15);
        Nangles = size(angle_range_deg, 2);
        powers = zeros(Nangles, Nangles);

        % Steer into each (phi,theta) direction and get the power
        for ii=1:Nangles,
            for jj=1:Nangles,
                theta = angle_range_deg(ii)*(pi/180);
                phi = angle_range_deg(jj)*(pi/180);
                As = subspcrfi_A(channelHz(cc), elemXYZ, phi, theta);
                As = transpose(As);
                pwr = conj(transpose(As)) * Qn*conj(transpose(Qn)) * As;
                powers(ii,jj) = 1/pwr;
            end
        end
        
        % Attempt to find DOAs (TODO: gaussian mixture model?)
        searchpowers = powers;
        for dd=1:Mrfi,
            [vvec,i_phi]=min(searchpowers,[],1);
            [vv,i_theta]=min(vvec,[],2);
            i_phi = i_phi(i_theta);
            doa_phi = angle_range_deg(i_phi);
            doa_theta = angle_range_deg(i_theta);
            fprintf(1, 'DOA = (%f,%f) degrees\n', doa_phi, doa_theta);
        end
        
        % Make surface plot of powers in different phasing directions
        % Note: for some reason Matlab surf() is limited to <=24x24
        % so instead we can use Images
        if 1,
           figure(gcf); clf;
           surf(angle_range_deg,angle_range_deg,real(powers));
           view([90 90]), axis tight;
           %N=32;test=(1:N)'*(1:N); surf(test);
        else
           plotdata = real(powers);
           plotdata = plotdata - min(min(plotdata));
           plotdata = 255 * (plotdata ./ max(max(plotdata)));
           imshow(plotdata), colormap(jet);
        end
        title(['Channel ' int2str(cc) ' power vs steering angle']);
        xlabel('Angle off zenith [phi]'), ylabel('Angle off zenith [theta]');
        [empty]=input('Hit Enter to continue.');
                
    end % for(Nch)
  end % for(Nsets)
