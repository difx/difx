% Generates data from a model with specified array element
% positions and radio source positions.
%
% Signals are in two classes, "source" and "RFI".
% Array elements are in two classes as well, "astro" and "reference".
%
% The "RFI" signal is seen by both "astro" and "reference" elements but most
% strongly by the "reference" elements.
% The "astro" signal is seen only by the "astro" elements.
% 
% lambda = 1x1 = wavelength in meters
% elempos = 3 x Nant = x,y,z coordinates of array elements in meters
% src_dirpwr = Nsrc x 3 = phi, theta, power of each source
% rfi_dirpwr = Nsrc x 3 = phi, theta, power of each RFI
% refant_indices = Nrefants x 1 = reference antenna indices taken from (1:Nant)
% refant_gain = 1x1 gain of reference versus astro elements (often >1e3)
% ac_noise_pwr = 1x1 = sigma^2 noise power of every array element
% xc_noise_pwr = 1x1 = optional, power of correlated noise accross all elems
%
% Angles: theta is plane tilt from array zenith, phi is azimuth.
%         Values are in degrees.
%
% Function returns Rxx covariance matrix and element signal vector x.
%
% Example
% Ne=16;[eps]=subspcrfi_elemXYZ(Ne, 10e-2);
% Rxx=subspcrfi_modelgen2(0.2202, eps, [0 0 1e-5], [10 25 1; 40 25 1], [1 2], 1e3, 1e-7, 1e-8);
% tRxx=zeros(1,Ne,Ne,2); tRxx(1,:,:,1)=Rxx; tRxx(1,:,:,2)=Rxx; %make 2ch data
% cRxx=subspcrfi_subtraction(tRxx, [1 2]);
% surf(abs(Rxx));
% [vv,ee]=eig(Rxx); Nsrc=subspcrfi_MDLrank(diag(ee),1)

function [Rxx,x]=subspcrfi_modelgen2(lambda, elempos, src_dirpwr, rfi_dirpwr, refant_indices, refant_gain, ac_noise_pwr, xc_noise_pwr)

  % Derived data
  Nant = size(elempos,2);
  Nsrc = size(src_dirpwr,1);
  Nrfi = size(rfi_dirpwr,1);
  srcant_indices = 1:Nant;
  srcant_indices(refant_indices) = [];
  
  % Convert input args
  src_dirpwr(:,1:2) = deg2rad(src_dirpwr(:,1:2));
  rfi_dirpwr(:,1:2) = deg2rad(rfi_dirpwr(:,1:2));
  src_dirpwr(:,3) = sqrt(abs(src_dirpwr(:,3)));
  rfi_dirpwr(:,3) = sqrt(abs(rfi_dirpwr(:,3)));
  
  % Arrival phase delays of each plane wave signal (steering vectors)
  vk_src = zeros(Nsrc, Nant);
  vk_rfi = zeros(Nrfi, Nant);
  for kk=1:Nsrc,
      phi = src_dirpwr(kk,1); theta = src_dirpwr(kk,2);
      vk_src(kk,:) = subspcrfi_steer(phi, theta, lambda, elempos);
  end
  for kk=1:Nrfi,
      phi = rfi_dirpwr(kk,1); theta = rfi_dirpwr(kk,2);
      vk_rfi(kk,:) = subspcrfi_steer(phi, theta, lambda, elempos);
  end
  
  % Signals before beamforming (no weighting and summation applied)
  sig_at_elems = zeros(1, Nant);
  sig_tmp = zeros(1, Nant);
  Rxx = zeros(Nant, Nant);
  
  % Covariance of RFI signals
  % for reference antennas, we add E<g*I,g*I> into the 2x2 submatrix
  % for others, we add E<S+I,S+I>=E<S,S>+E<I,I> into the (N-2)x(N-2) submatrix
  for kk=1:Nrfi,
      % weak RFI signal seen by array
      sig_astrorfi = rfi_dirpwr(kk,3) * vk_rfi(kk,:);
      % seen at high gain by reference antennas
      sig_ref = refant_gain * sig_astrorfi;
      % assemble into single vector [ gI, gI, I, I, I, ..., I ]
      sig_tmp(refant_indices) = sig_ref(refant_indices);
      sig_tmp(srcant_indices) = sig_astrorfi(srcant_indices);
      sig_at_elems = sig_at_elems + sig_tmp;
      % Add to Rxx: assume signals uncorrelated, E<X,X*> = E<x0,x0*> + E<x1,x1*> + ...
      % This adds E<gI,gI> of Reference and E<I,I> of array
      Rxx = Rxx + conj(transpose(sig_tmp))*sig_tmp;
  end

  % Covariance of non-RFI source signals
  for kk=1:Nsrc,
      % weak source seen by array
      sig_astro = src_dirpwr(kk,3) * vk_src(kk,:);
      % much weaker or no source seen by reference
      sig_ref = zeros(size(sig_astro)); % or: sig_ref = (1/refant_gain)*sig_astro;
      % assemble into single vector [ 0, 0, S, S, S, ..., S ]
      sig_tmp(srcant_indices) = sig_astro(srcant_indices);
      sig_tmp(refant_indices) = sig_ref(refant_indices);
      sig_at_elems = sig_at_elems + sig_tmp;
      % Add to Rxx: assume signals uncorrelated, E<X,X*> = E<x0,x0*> + E<x1,x1*> + ...
      % This adds E<S,S> of array
      Rxx = Rxx + conj(transpose(sig_tmp))*sig_tmp;
  end
  
  % Add element-internal noise power to autocorrelations
  if (ac_noise_pwr>0),
    Rxx = Rxx + eye(Nant,Nant)*ac_noise_pwr;
    sig_at_elems = sig_at_elems + ac_noise_pwr*rand(size(sig_at_elems));
  end
  
  % Add overall noise using Hermitian matrix
  if (xc_noise_pwr>0),
      Nxx = xc_noise_pwr*(rand(Nant,Nant) + i*rand(Nant,Nant));
      Nxx = Nxx*conj(transpose(Nxx));
      Rxx = Rxx + Nxx;
  end
  
  x = sig_at_elems;
  