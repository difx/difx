% subspcrfi(ch)
% Analysis of channel ch.
%
%function A=subspcrfi(ch)

  % channel selection (some plots use single-channel)
  % Apertif ch#35 = rfi, ch#17 = likely free of rfi
  %ch=35; 
  ch=17;
  
  clight = 299792458;
  
  %% APERTIF data
  fn='VirgoRFI.mat';
  ofn='OffVirgoRFI.mat';
  fprintf(1,'ON  source data from: %s\n', fn);
  fprintf(1,'OFF source data from: %s\n', ofn);
  src=load(fn);
  osrc=load(ofn);
  assert(max(abs(src.frequency-osrc.frequency))==0);
  ignore_mask = [60 61 62 63 64]; % only array elements 1 to 60 are actually connected! (maybe 60 even not..)
  
  fnr=1;
  
  %% APERTIF information
  % http://arxiv.org/ftp/arxiv/papers/0912/0912.0093.pdf
  % http://www.radionet-eu.org/fp7wiki/lib/exe/fetch.php?media=na:engineering:ew:bakker_ew2_final.pdf
  % Prototype 1
  %   DIGESTIF has 8x7x2 elements, installed in one 25m dish, Tsys=70K
  %   Individual elements have 3deg x 3deg antenna response
  %   Beamformed beam pattern in Fig4 was done by steering vector -1.5:0.15:+1.5 x -1.5:0.15:+1.5 off-zenith.
  %   Digestif total size 80x80cm, element separation 10cm = lamba/2@1.5GHz
  % Prototype 2
  %   144 elements, total size 97x97cm, element separation 11cm
  
  %% Combine ON and OFF-source data sets
  Ion = 1; Ioff = 2;
  alldata(Ion,:,:,:)=src.ACM(:,:,:);
  alldata(Ioff,:,:,:)=osrc.ACMoff(:,:,:);
  frequencies=src.frequency;
  freq=src.frequency(ch);

  Delems = 10e-2; % array element separation in meters
  Nch = size(src.ACM,3);
  Nant = size(src.ACM,1);
  Nsets = size(alldata, 1);
  Nrfi = 4; % max. number of interferers to cancel
  Mint = 12*Nant; % number of time-integrated covariances for 1 covariance (APERTIF)

  %% Data Summary
  fprintf(1,'Data size: %d x %d, %d channels\n', size(src.ACM,1), size(src.ACM,2), size(src.ACM,3));
  tstr=sprintf('Channel #%d (%f Hz) ', ch, freq);
  
  %% Process: compute array element layout (for a square array)
  [elem_positions]=subspcrfi_elemXYZ(Nant, Delems);
  if 0,
      figure(fnr), clf; fnr=fnr+1;
      scatter(elem_positions(1,:),elem_positions(2,:)), 
      title('Array element locations');
  end

  if 0,
      figure(fnr), clf; fnr=fnr+1;
      subspcrfi_plotArrayResponse(clight/frequencies(1), elem_positions, ones(Nant,1));
  end
  
  %% Overwrite data with model (note that "source" must be weaker than "RFI")
  signaldata = zeros(Nsets, Nch, Nant);
  if 0,
      if 0,
          off_src_list = [10 25 1; 40 17 2.7; 33 23 1.5; 7 41 1.3]; % {azimuth,el_off_zenith,simga^2}
          on_off_tilt    = [-1.5 2.0];
          on_src_list = [0 0 1e-4; off_src_list];
          ac_noise_pwr = mean(on_src_list(:,3))/(Nant^3);
          xc_noise_pwr = 1e-5*ac_noise_pwr;
      else
          on_src_list = [45 -90 1e-3; 100 40 1; 15 -17 1];
          off_src_list = on_src_list;
          ac_noise_pwr = 0;
          xc_noise_pwr = 0;
      end
      for cc=1:Nch,
        lambda = 299792458 / frequencies(cc);
        [r,x] = subspcrfi_modelgen(lambda, elem_positions, on_src_list, ac_noise_pwr, xc_noise_pwr);
        alldata(1,:,:,cc) = r;
        signaldata(1,cc,:) = x;
        [r,x] = subspcrfi_modelgen(lambda, elem_positions, off_src_list, ac_noise_pwr, xc_noise_pwr);
        alldata(2,:,:,cc) = r;
        signaldata(2,cc,:) = x;
      end

      Nrfi = size(off_src_list,1) - 1;
      
      tch = squeeze(alldata(1,:,:,1));
      [vv,ee]=eig(tch);
      Nmdl=subspcrfi_MDLrank(diag(ee),1);

      fprintf(1, 'Using model-generated covariance data with %d interferers (MDL says %d).\n', Nrfi, Nmdl);
      figure(fnr), clf; fnr=fnr+1;
      surf(log(abs(tch))), view([90 90]), title('Model data ON-src channel 1 : log(abs(Rxx))'),
      %surf(angle(tch)), view([90 90]), title('Model data ON-src channel 1 : angle(Rxx)'),
      axis tight, xlabel('Ant#'), ylabel('Ant#');
  end

  %% Process: compute SNR at zenith steering
  if 0,
      dsteer = ones(1, Nant);
      [ChSNR,Dmax]=subspcrfi_SNR(alldata); %, dsteer);
      figure(fnr), clf; fnr=fnr+1;
      subplot(2,1,1), plot(abs(ChSNR)), xlabel('Channel'), ylabel('SNR'), 
        title('Channel SNR''s with zenith steering and maximum SNR weights'), axis tight;
      subplot(2,1,2), plot(Dmax), xlabel('Channel'), ylabel('max(abs(R_s))'),
        title('Largest absolute (Ron-Roff) difference'), axis tight;
      % for the VirgoA data set, SNR is extremely! low
      % SNR<<1.0 for zenith, SNR max 2.5 for conjugate field match
  end

  %% Plot On/Off source raw data and eigenvalues
  if 0,
      figure(fnr), clf; fnr=fnr+1;
      subplot(2,2,1), surf(abs(squeeze(alldata(1,:,:,ch)))), view([90 90]), title([tstr 'ON-source']);
      subplot(2,2,2), surf(abs(squeeze(alldata(2,:,:,ch)))), view([90 90]), title([tstr 'OFF-source']);
      [ev,ed]=eig(squeeze(alldata(1,:,:,ch)));
      subplot(2,2,3), surf(abs(ed)), view([90 90]), title([tstr 'ON eig vals']);
      [ev,ed]=eig(squeeze(alldata(2,:,:,ch)));
      subplot(2,2,4), surf(abs(ed)), view([90 90]), title([tstr 'OFF eig vals']);
  end

  %% Plot every channel's phase/mag around center elements (mid-8x8)
  if 0,
      for pp=1:Nsets,
          figure(fnr), clf;
          Nantplt = 8;
          Iants = (Nant/2 - Nantplt/2):(Nant/2 + Nantplt/2);
          for i1=1:Nantplt,
              for i2=1:Nantplt,
                  dd = squeeze(alldata(1,Iants(i1),Iants(i2),:));
                  pltidx = (i1-1)*Nantplt + (i2-1) + 1;
                  subplot(Nantplt,Nantplt, pltidx),
                  %plot(angle(dd)*180/pi);
                  plot(abs(dd));
                  set(gca,'TickDir','in','FontSize',6);
                  if (i1==i2),
                    if (pp==1), title('On-Source'); else title('Off-source'); end
                  end
              end
          end
          fnr=fnr+1;
      end
  end

  %% Process: extract internal noise of elements
  % Done by subtracting cross-corrs from the auto-corrs
  % Potentially also doable with on-source R versus off-source R comparison
  [noises]=subspcrfi_getNoises(alldata); 

  %% Process: compute eigenvalues and eigenvectors if input data
  [rfi_evalues,rfi_eterm1,rfi_evecsfull]=subspcrfi_getEV(alldata);

  %% Process: look for reference antennas (if not known)
  if 0
      % manually: use channels known to have RFI in the data set
      for cc=30:35, 
          ch_evals = abs(squeeze(rfi_evalues(1,:,cc)));
          [m_v,m_i] = max(ch_evals);
          % ev row has weights of antenna contributions to the RFI power
          ch_vec = abs(squeeze(rfi_evecsfull(1,:,m_i,cc)));
          likely = ch_vec./std(ch_vec);
          lmask = (likely>12); % or some other threshold
          Nrefants = sum(lmask);
          %
          % APERTIF data set
          % Doesn't seem to contain any reference antennas,
          % nor any single antenna that would predominantly
          % contribute power to the RFI interferer power sum
      end
  end
  
  %% Process: nulling of RFI using eigenvalue decomposition
  if 1,
      fprintf(1, '%s\n', ['Trying to null ' int2str(Nrfi) ' RFI sources in channels where threshold exceeded.']);
      [nulldata]=subspcrfi_nulling(alldata, rfi_evalues, rfi_evecsfull, Nrfi, Mint, ignore_mask);
  else
      fprintf(1, 'No nulling, output data is copy of input data\n');
      nulldata=alldata;
  end

  %% Process: compute eigenvalues and eigenvectors if "nulled" data
  [null_evalues,null_eterm1,null_evecsfull]=subspcrfi_getEV(nulldata);

  %% Plot 'Ndom' most dominant eigenvalues, for each freq channel
  Ndom = Nant;   %Ndom = floor(size(src.ACM,1)/3)+1;
  if 1,
      figure(fnr), clf;  fnr=fnr+1;
      [evspecAx]=subspcrfi_plotEVspec(rfi_evalues, Ndom, 'Before nulling: ');
      figure(fnr), clf;  fnr=fnr+1;
      subspcrfi_plotEVspec(null_evalues, Ndom, 'After nulling: ', evspecAx);
  end
  
  %% Plot eigenvalue phase spectrum for 1st eigenvalue (highest energy)
  % Use the data where RFI has not been removed yet.
  if 0,
      for pp=1:Nsets,
          figure(fnr), clf;
          Nasqr=ceil(sqrt(Nant));
          for ant=1:Nant,
            % plot: square array of sub-plots 
            %       plot #i is antenna #i
            %       each plot shows 1xNch-point angle(eigvec(1))
            %   for each freq {
            %     EVec = eigs(covardata(freq) 64x64) = 64x64
            %     dominant Evec = 1x64
            %                   = [ g_ant1 g_ant2 g_ant3 .. g_antN ]
            %     pEVec(f) = phase(dominant Evec) = 1 x 64
            %   }
            %   pEVec = Nfreq x Nant

            figure(fnr), subplot(Nasqr,Nasqr, ant), hold on;
            chphases = angle(squeeze(rfi_eterm1(pp,ant,:)));
            plot(chphases,'x');
            %plot(unwrap(chphases),'x'); % unrwap hides some info though..
            axis([0, Nant, -4, 4]);
          end
          shg;
          if (pp==1), tstr='On-Source'; else tstr='Off-source'; end
          mtit([tstr ' : antenna (1-' int2str(Nant) ') weights in 1st EigVec vs frequency channel']);
          fnr=fnr+1;
      end
  end
      
  %% Beamforming
  % Can use the rfi-nulled R as reference, or the original R
  % MVDR beamformer 
  %   w[t+1] = inv(Rxx)*a(phi) / (hermitian(a(phi)) * inv(Rxx) * a(phi))
  %   where a(phi) is the beam steering towards source
  %   a(phi) = vector[a(phi,i)] = [exp(-j*2pi*f * (b_i*s_vec) /c)]
  %      s_vec = vector to sky source, b_i = position of element
  % Power-maximizing conventional beamformer
  %    w[t+1] = a / sqrt(hermitian(a)*a)
  % LMS http://etd.lib.fsu.edu/theses_1/available/etd-04092004-143712/unrestricted/Ch_6lms.pdf
  %   w[t+1] = w[t] + mu * x[t]*conj(error) ; error = ref-actual = (rfi-nulled Rxx) - (original Rxx)
  
  %% QR Beamforming
  % http://ce.et.tudelft.nl/publicationfiles/1775_999_Paper_SPB_Behar.pdf
  % Rxx = Q*R from QR decomposition
  % solve z1=inv(herm(R))*a, then solve z2=inv(R)*z1, finally w_opt_mvdr=z2/(herm(a)/z2)
  
  %% DOA estimation
  % MUSIC beamformer, good sidelobe suppression, smaller beamwidth, complexity O(ant^3)
  %   discard first N_rfi columns of eigenvector matrix to get noise matrix Un
  %   Power(s) = hermitian(a)*a / (hermitian(A)*Un*hermitian(Un)*a)
  %   w[t+1] = ...?
  if 0,
      [DOAs]=subspcrfi_doa_MUSIC(rfi_evalues, rfi_evecsfull, frequencies, elem_positions);
  end
  
  %% Plot image of the source, assuming it is visible
  if 1,
      figure(fnr), clf; fnr=fnr+1;
      tmpax=caxis;
      for pp=1:Nsets,
          [uvd] = subspcrfi_RtoUV( squeeze(alldata(pp,:,:,ch)), frequencies(ch), elem_positions );
          img = abs(fft2(uvd));
          subplot(1,2,pp), surf(img), view([90 90]), axis equal, axis tight;
          if pp==1, 
              title(['Original data : Image ON-source']);
              tmpax=caxis;
          else
              title(['Original data : Image OFF-source']); 
              %caxis(tmpax);
          end
      end
      figure(fnr), clf; fnr=fnr+1;
      for pp=1:Nsets,
          [uvd] = subspcrfi_RtoUV( squeeze(nulldata(pp,:,:,ch)), frequencies(ch), elem_positions );
          img = abs(fft2(uvd));
          subplot(1,2,pp), surf(img), view([90 90]), axis equal, axis tight,
          %caxis(tmpax);
          if pp==1, 
              title(['Nulled data : Image ON-source']);
          else
              title(['Nulled data : Image OFF-source']); 
          end
      end
  end

