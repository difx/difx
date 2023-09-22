% Returns eigenvalues and dominant eigenvector
% for all frequency channels and every antenna
%
% Input: alldata = 2 x Nant x Nant x Nch
function [evals,eterm1,evecsfull]=subspcrfi_getEV(alldata)
  Nsets = size(alldata, 1);
  Nant = size(alldata, 2);
  Nch = size(alldata, 4);
  
  evals = zeros(Nsets, Nant, Nch);
  eterm1 = zeros(Nsets, Nant, Nch);
  evecsfull = zeros(Nsets, Nant, Nant, Nch);
  for pp=1:Nsets,
      for ich=1:Nch,
          
        % one channel, but all antennas
        Rxx_in = squeeze(alldata(pp,:,:,ich));
        
        % diagonal matrix based on eigenvalue decomposition
        [evec,ediag]=eig(Rxx_in);
        evalues=diag(ediag);
        evals(pp,:,ich)=evalues(1:Nant);
        domevec=evec(:,1)'; %domevec=evec(1,:);
        eterm1(pp,:,ich)=squeeze(domevec);
        evecsfull(pp,:,:,ich)=evec;

        % diagonal matrix based on SVD decomposition
        % [uu,ss,vv]=svd(Rxx_in);
        % note: max(abs(uu-conj(transpose(vv)))) is close to 0 for hermitian
        % dataout=uu*ss*vv';
        
        % eigenvalue spread figure Lev
        Lev = max(abs(evalues(1:Nant))) / min(abs(evalues(1:Nant)));
        
        % upper bound for LMS beamformer update algorithm parameter
        % http://etd.lib.fsu.edu/theses_1/available/etd-04092004-143712/unrestricted/Ch_6lms.pdf
        mu_max = 1/(3*trace(Rxx_in));
      end
  end
  