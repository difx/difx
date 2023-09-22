% Array calibration
% Takes difference between R_on and R_off matrices and
% computes maximum SNR weights into a steering direction.
%
% Input: alldata = 2 x Nant x Nant x Nch
%        dsteer = optional 1 x Nant steering vector
%
% Output: ChSNR = 1 x Nch     SNR values
%         MaxDiff = 1 x Nch   max(abs(R_on-R_off))
%         Wopt = Nch x Nant   max. SNR weight sets for every freq channel
%
% If dsteer is not specified, conjugate field matching is used
% instead and steering is done into dominant eigenvector direction,
% be careful in presence of RFI...!
%
function [ChSNR,MaxDiff,Wopt]=subspcrfi_SNR(alldata, dsteer)
  Nant = size(alldata,2);
  Nch = size(alldata,4);
  Ion=1; Ioff=2;
  
  ChSNR = zeros(Nch,1);
  MaxDiff = zeros(Nch,1); 
  Wopt = zeros(Nch,Nant);
  for cc=1:Nch,
      Ron = squeeze(alldata(Ion,:,:,cc));
      Roff = squeeze(alldata(Ioff,:,:,cc));
      Rs = Ron - Roff;

      % conjugate field matching weight, dominant eigenvector
      if nargin==1,
        [ev,ed] = eig(Rs);
        w_cfm = ev(:,1);
      else
        w_cfm = dsteer';
      end
      
      % maximum SNR weight from conjugate field match
      w_opt = inv(Roff) * w_cfm;
      
      SNR = conj(transpose(w_opt)) * Rs * w_opt;
      SNR = SNR / (conj(transpose(w_opt)) * Roff * w_opt);
      ChSNR(cc) = SNR;
      MaxDiff(cc) = max(max(abs(Rs)));
      Wopt(cc,:) = w_opt;
  end
  