% Generates data from a model with specified array element
% positions and radio source positions.
% 
% lambda = 1x1 = wavelength in meters
% elempos = 3 x Nant = x,y,z coordinates of array elements in meters
% src_dirpwr = Nsrc x 3 = phi,theta,power of each source, angles in degrees
% ac_noise_pwr = 1x1 = sigma^2 noise power of every array element
% xc_noise_pwr = 1x1 = optional, power of correlated noise accross all elems
%
% Angles: theta is plane tilt from array zenith, phi is azimuth
%
% Returns Rxx covariance matrix and element signal vector x.
%
% http://www.antenna-theory.com/arrays/weights/mmse4.php
%
% Example
% Ne=16;[eps]=subspcrfi_elemXYZ(Ne, 10e-2);
% Rxx=subspcrfi_modelgen(0.2202, eps, [0 0 1e-5; 10 25 1; 40 25 1], 1e-6, 0);
% surf(abs(Rxx));
% [vv,ee]=eig(Rxx); Nsrc=subspcrfi_MDLrank(diag(ee),1)

function [Rxx,x]=subspcrfi_modelgen(lambda, elempos, src_dirpwr, ac_noise_pwr, xc_noise_pwr)
  Nant = size(elempos,2);
  Nsrc = max(size(src_dirpwr,1));
  
  % Convert input args
  src_dirpwr(:,1:2) = deg2rad(src_dirpwr(:,1:2));
  src_dirpwr(:,3) = sqrt(abs(src_dirpwr(:,3)));

  % Arrival phase delays of each plane wave signal (=steering vectors)
  vk_src = zeros(Nsrc, Nant);
  for kk=1:Nsrc,
      phi = src_dirpwr(kk,1); theta = src_dirpwr(kk,2);
      vk_src(kk,:) = subspcrfi_steer(phi, theta, lambda, elempos);
  end
  
  % Weights w counteract the vk_src steering, ideal weights are conj(vk_src)
  %  ArrayFactor = weights' * vk_src = scalar
  %  Y_response = ArrayFactor * radpattern(phi,theta) ; phi,theta=arrival 
  %                                       angle of wave relative to array

  % Signals before beamforming (no weighting and summation applied)
  sig_at_elems = zeros(1, Nant);
  Rxx = zeros(Nant, Nant);
  for kk=1:Nsrc,
      phi = src_dirpwr(kk,1);
      theta = src_dirpwr(kk,2);
      
      gain = 1; %cos(theta)^2;
      sig = gain*src_dirpwr(kk,3) * vk_src(kk,:);
      
      % Method 1: assume signals uncorrelated, E<X,X*> = E<x0,x0*> + E<x1,x1*> + ...
      Rxx = Rxx + conj(transpose(sig))*sig;
      
      % Method 2: assume signals correlated, E<X,X*> = E<x0+x1+..,(x0+x1+..)*>
      % after loop: Rxx = conj(transpose(sig_at_elems))*sig_at_elems;
      
      % Accumulate signals
      sig_at_elems = sig_at_elems + sig;
  end
  
  % Add element-internal noise power to autocorrelations
  if (ac_noise_pwr>0),
      Rxx = Rxx + eye(Nant,Nant)*ac_noise_pwr;
  end
  
  % Also add overall noise using Hermitian matrix
  if (xc_noise_pwr>0),
      Nxx = xc_noise_pwr*(rand(Nant,Nant) + i*rand(Nant,Nant));
      Nxx = Nxx*conj(transpose(Nxx));
      Rxx = Rxx + Nxx;
  end
  
  x = sig_at_elems + ac_noise_pwr;
  