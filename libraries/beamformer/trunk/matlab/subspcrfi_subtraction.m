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
%   Nsets x Nant x Nant x Nchannels = cleaned covariance data
%
function [cleandata,cleandata2]=subspcrfi_subtraction(Rxx, refant_idcs)
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
      fprintf(1, 'Algorithm requires two reference antennas!\n');
      return
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
          
          % Copy new data and remove reference antenna correlations
          % note: the abs(diff(cleaned-nonRfi)) is slightly less "off" with conj.transp.
          cleandata(pp,:,:,cc) = clean; 
          
          
          %% General method

          R00=chRxx(ant_idcs,ant_idcs);
          R11=chRxx(refant_idcs,refant_idcs);
          R01=chRxx(ant_idcs,refant_idcs);
          R10=chRxx(refant_idcs,ant_idcs);

          [u,s,v]=svd(R11);
          s_inv = transpose(diag(1./diag(s)));
          R11inv = v*s_inv*(u'); % pseudo-inverse
          
          genclean = R00 - (R01*inv(R11))*R10; % with inverse
          % genclean = R00 - (R01*R11inv)*R10; % with pseudo-inverse

          % note: the abs(diff(cleaned-nonRfi)) is slightly less "off" with conj.transp.
          cleandata2(pp,:,:,cc) = zeros(Nant,Nant);
          cleandata2(pp,ant_idcs,ant_idcs,cc) = genclean';
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
  