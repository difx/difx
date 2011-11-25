%
% Use subtraction method to correct auto-correlations (self power
% spectra) by using information from two reference antennas.
%
% The input covariance matrix must contain data from the array
% and from two reference antennas. The references should not see the 
% sky sources at any significant power level but instead should see
% the RFI at high interferer to noise ratio INR.
%
% Inputs:
%   Rxx = Nsets x Nant x Nant x Nchannels = covariance data
%   refant_idcs = 2 x 1 = indices of the reference antennas within 1:Nant
%
% Output:
%   Nsets x Nant x Nant x Nchannels = covariance cleaned with Nrfi=1,Nref=2
%   Nsets x Nant x Nant x Nchannels = covariance cleaned with generic method
%
function [cleandata,cleandata2]=subspcrfi_subtraction(Rxx, refant_idcs, cleanmodel)
  Nsets = size(Rxx, 1);
  Nant = size(Rxx, 2);
  Nch = size(Rxx, 4);
  
  % Default: output is copy of input
  cleandata = Rxx;
  cleandata2 = Rxx;
  
  % Antenna list without reference antennas
  ant_idcs = (1:Nant);
  ant_idcs(refant_idcs) = [];
  
  % Remaining antenna numbers
  Nref = max(size(refant_idcs));
  Nsky = Nant - Nref;

  if (Nref ~= 2),
      fprintf(1, 'Special case Algorithm requires two reference antennas!\n');
  end
  
  for pp=1:Nsets,
      for cc=1:Nch,
          chRxx = squeeze(Rxx(pp,:,:,cc));
           
          % Sanity check for RFI presence
          astro_ac_mean = mean(diag(chRxx(ant_idcs,ant_idcs)));
          ref_ac_mean = mean(diag(chRxx(refant_idcs,refant_idcs)));
          if (ref_ac_mean < 10*astro_ac_mean),
              fprintf(1, 'Reference AC average is below +10dB of array AC average, no RFI.\n');
              continue;
          end

          %% Briggs, Kesteven special case method
          clean_kesteven = chRxx(ant_idcs,ant_idcs);
          if (Nref == 2),
              Cn1 = chRxx(refant_idcs(1),:);
              Cn2 = chRxx(refant_idcs(2),:);
              C12 = chRxx(refant_idcs(1),refant_idcs(2));

              % [A] Direct correction
              % This fails if noise>>RFI in the C<1,2>=C<ref1,ref2>
              %autogains = (Cn1.*conj(Cn2)) ./ C12;

              % [B] Expanded correction
              % 1) Plug in an estimate of correlated noise in C<ref1,ref2>
              powsqr = 1e-8;% 1e-8*mean(diag(chRxx(ant_idcs,ant_idcs)));

              % 2) Compute gains
              crossgains = (conj(transpose(Cn1)) * Cn2) .* C12 / (powsqr + C12*conj(C12));
              
              % 3) Check goodness of autocorrs: should be "practically" real-valued
              autophases = angle(diag(crossgains))*(180/pi); % angle() does not do unwrap()
              autophases_err = abs(autophases)>1e-5;
              if (sum(autophases_err)>0),
                  fprintf(1, 'Warning: bad RFI estimates in autocorrs: phase()>1e-5 deg for %d elements!\n', ...
                    sum(autophases_err));
              end

              % 4) Check amplitude closure
              % should see 1 == C<src1,ref1>*C<src2,ref2>/(C<src2,ref1>*C<src1,ref2>)
              ant_idcs1 = ant_idcs(1:(end-1));
              ant_idcs2 = ant_idcs(2:end);
              closure = chRxx(ant_idcs1,refant_idcs(1)) .* chRxx(ant_idcs2,refant_idcs(2)) ...
                      ./ (chRxx(ant_idcs2,refant_idcs(1)) .* chRxx(ant_idcs1,refant_idcs(2)));
              closure_err = abs(abs(closure)-1.0) > 1e-5;
              if (sum(closure_err)>0),
                  fprintf(1, 'Warning: bad RFI estimates in autocorrs: |closure-1.0|>1e-5 for %d elements!\n', ...
                    sum(closure_err));
              end

              % 4) Subtract from original data
              clean = chRxx - crossgains;
              clean_kesteven = clean(ant_idcs, ant_idcs);
              
              % Copy new data and remove reference antenna correlations
              % note: the abs(diff(cleaned-nonRfi)) is slightly less "off" with conj.transp.
              cleandata(pp,:,:,cc) = clean; 
          end

          %% Briggs, Kesteven special case method -- attempt at extension
          if (Nref == 3) && (0),
              syms gi1 gi2 gj1 gj2 gk1 gk2 gl1 gl2 I1 I2 Cik
              Rij=sym('gi1*conj(gj1)*I1*I1 + gi2*conj(gj2)*I2*I2 + gi1*conj(gj2)*I1*I2 + gj2*conj(gi1)*I1*I2');
              Rij2=subs(Rij,'I1*I2','(Cik-gi1*conj(gk1)*I1*I1-gi2*conj(gk2)*I2*I2)/(gi1*conj(gk2)+gk1*conj(gi2))');
              I1s=solve(Rij2, 'I1');
              I2s=solve(Rij2, 'I2');

              Cn1 = chRxx(refant_idcs(1),:);
              Cn2 = chRxx(refant_idcs(2),:);
              Cn3 = chRxx(refant_idcs(3),:);
              C12 = chRxx(refant_idcs(1),refant_idcs(2));
              C13 = chRxx(refant_idcs(1),refant_idcs(3));
              C23 = chRxx(refant_idcs(2),refant_idcs(3));

              % [B] Expanded correction
              % 1) Plug in an estimate of correlated noise in C<ref1,ref2>
              powsqr = 1e-8;% 1e-8*mean(diag(chRxx(ant_idcs,ant_idcs)));

              % 2) Compute gains
              crossgains1 = (conj(transpose(Cn1)) * Cn2) .* C12 / (powsqr + C12*conj(C12));
              crossgains2 = (conj(transpose(Cn2)) * Cn3) .* C23 / (powsqr + C23*conj(C23));
              crossgains3 = (conj(transpose(Cn1)) * Cn3) .* C13 / (powsqr + C13*conj(C13));
              crossgains4 = C12 * C13 * C23 / (powsqr + C23*conj(C23));
              crossgainsE = (conj(transpose(Cn1)) * Cn2 * conj(transpose(Cn2)) * Cn3) .* C12 .* C23 ...
                  / (powsqr + C12*conj(C12)*C23*conj(C23));
              
              % 4) Subtract from original data
              cleanE = chRxx - (crossgains1 + crossgains2 + crossgains3 + crossgains4) ./ 4;
          end

          %% General method
          % van der Veen, Boonstra, "Spatial filtering of RF interference in Radio
          % Astronomy using a reference antenna", 2004
          R00=chRxx(ant_idcs,ant_idcs);
          R11=chRxx(refant_idcs,refant_idcs);
          R01=chRxx(ant_idcs,refant_idcs);
          R10=chRxx(refant_idcs,ant_idcs);
          genclean = R00 - (R01*inv(R11))*R10;   % with inverse, not pseudo-inverse
          %genclean = R00 - (R01*pinv(R11))*R10; % with pseudo-inverse

          %% General method II
          % Since R11 contaminated by high noise, van der Veen "inv(R11)" diagonal has 1/(RFI^2 + N^2)
          % thus increasing N^2 (lowering the INR ratio) decreases the accuracy of the correction
          % Instead we estimate R11 from the R01,R10,R00 covariances to discard the N^2 term.
          cR11 = R10*pinv(R00)*R01;
          genclean2 = R00 - (R01*pinv(cR11))*R10; 
                  
          % A faster and more accurate by expanding genclean2(cR11) into full
          % form and reducing pseudoinverse:
          genclean3_ref = R00 - R01*pinv(R01)*R00*pinv(R10)*R10;
          R01w = R01*pinv(R01);
          genclean3 = R00 - R01w*R00*(R01w');

          %% Choose final output (Generic I, Generic II or test bench output)
          % note: the abs(diff(cleaned-nonRfi)) is slightly less "off" with conj.transp.
          cleandata2(pp,:,:,cc) = zeros(Nant,Nant);
          %cleandata2(pp,ant_idcs,ant_idcs,cc) = genclean';
          cleandata2(pp,ant_idcs,ant_idcs,cc) = genclean3;
          
          if (nargin==3) && (pp==1) && (cc==1),
              error_input_clean = abs(chRxx(ant_idcs,ant_idcs) - cleanmodel);
              error_br_clean = abs(clean_kesteven - cleanmodel);
              error_genI_clean = abs(genclean - cleanmodel);
              error_genII_clean = abs(genclean2 - cleanmodel);
              error_genIII_clean = abs(genclean3 - cleanmodel);
              mx_input = max(max(error_input_clean));
              mx_br = max(max(error_br_clean));
              mx_genI = max(max(error_genI_clean));
              mx_genII = max(max(error_genII_clean));
              mx_genIII = max(max(error_genIII_clean));
              [mx_br_i, mx_br_j] = max2idx(error_br_clean);
              [mx_genI_i, mx_genI_j] = max2idx(error_genI_clean);
              [mx_genII_i, mx_genII_j] = max2idx(error_genII_clean);
              [mx_genIII_i, mx_genIII_j] = max2idx(error_genIII_clean);
              fprintf(1, 'Max(Abs(delta)) deviations compared to clean model:\n');
              fprintf(1, '  Non-clean  : %e\n', mx_input);
              fprintf(1, '  Briggs     : %e @ (%d,%d)\n', mx_br, mx_br_i, mx_br_j);
              fprintf(1, '  Generic I  : %e @ (%d,%d)\n', mx_genI, mx_genI_i, mx_genI_j);
              fprintf(1, '  Generic II : %e @ (%d,%d)\n', mx_genII, mx_genII_i, mx_genII_j);
              fprintf(1, '  Generic III: %e @ (%d,%d)\n', mx_genIII, mx_genIII_i, mx_genIII_j);
          end
          
      end
  end

  % Alternative method:
  % van der Veen, Boonstra, "Spatial filtering of RF interference in Radio
  % Astronomy using a reference antenna", 2004
  % R00=chRxx(antIdx,antIdx);
  % R11=chRxx(refIdx,refIdx);
  % R01=chRxx(antIdx,refIdx);
  % R10=chRxx(refIdx,antIdx);
  %  clean = R00 - (R01*(inv(R11)))*R10;
  %    or with pseudo-inverse
  %  [u,s,v]=svd(R11);
  %  s_inv = transpose(diag(1./diag(s)));
  %  R11inv = v*s_inv*(u');
  %  clean = R00 - (R01*R11inv)*R10;
  %  however testing this with 1xRFI the resulting diag(clean) has high error
  
