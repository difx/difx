% Take full cross-correlation matrix, extract auto-correlations AC(i)=C(i,i)
% and substract the coupled components from AC(i) to get own noise
% N(i) = AC(i) - 0.5*sum[ (C(i,j)+C(j,i)) ; for all j!=i]
%
% Input:  alldata = 2 x Nant x Nant x Nch
%         gains = 1 x Nant = antenna gains if known
% Output: noises = 2 x Nant x Nch
%
% Based on assumptions:
% 1) array is pointed to blank sky
% 2) all channels see only noise
% 3) array element autocorrelation AC(i) is equal to own noise variance
%    plus noise coupled from other elements, while own noise dominates
% 4) own noise part of AC(i) can be determined by subtracting the
%    coupled noises
% http://www.sis-statistica.it/files/pdf/atti/RSMi0602p437-440.pdf
%  
function [noises,prenoises]=subspcrfi_getNoises(alldata, gains)
  Nsets = size(alldata, 1);
  Nant = size(alldata, 2);
  Nch = size(alldata, 4);

  noises = zeros(Nsets, Nant, Nch);
  prenoises = zeros(Nsets, Nant, Nch);
  if (nargin==2),
      gains = diag(gains);
  else
      gains = eye(Nant);
  end

  for pp=1:Nsets,
      for cc=1:Nch,
          xc = squeeze(alldata(pp,:,:,cc));
          noauto = xc - diag(diag(xc));
          submat = 0.5 * (noauto*gains + transpose(noauto*gains)) * ones(Nant,1);
          prenoises(pp,:,cc) = diag(xc);
          noises(pp,:,cc) = diag(xc) - submat;
      end
  end
  