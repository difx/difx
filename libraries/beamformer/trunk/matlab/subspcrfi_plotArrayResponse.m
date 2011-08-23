%
% lambda = 1x1 = wavelength in meters
% elempos = 3 x Nant = x,y,z coordinates of array elements in meters
% weights = Nant x 1 = complex antenna weights for beamforming
%
% Example: copy code from end of func...

function [img,angles]=subspcrfi_plotArrayResponse(lambda, elempos, weights)
  Nant = size(elempos, 2);

  % Loop through all angles
  angles_deg = -90:1:90;
  angles = deg2rad(angles_deg);
  Nang = max(size(angles));
  AFlist = zeros(Nang,Nang);
  Ylist = zeros(Nang,Nang);

  for ii=1:Nang,
      phi = angles(ii);
      for jj=1:Nang,
        theta = angles(jj);
        a = subspcrfi_steer(phi, theta, lambda, elempos); % = phase delays at each element

        AF = transpose(a) * weights; % weights = conj(a) for optimal
        gain = (cos(theta)*cos(phi))^2; % elementPattern(theta,phi)
        Y = gain * AF;
    
        AFlist(ii,jj) = AF;
        Ylist(ii,jj) = Y;
     end
  end
  img = Ylist;
  angles = angles_deg;
  
  surf(angles,angles, abs(img));
  % surf(elempos(1,:),elempos(2,:), AFlist);
return

% Testing
lambda=10e-2; Ne=16;
[eps]=subspcrfi_elemXYZ(Ne, lambda);
pt_phi=90;pt_theta=0;
pt_k=(2*pi/lambda)*[sin(pt_theta)*cos(pt_phi) sin(pt_theta)*sin(pt_phi) cos(pt_theta)];
pt_weights=exp(+j*pt_k*eps);
pt_weights=transpose(pt_weights);
[xx,aa]=subspcrfi_plotArrayResponse(lambda, eps, pt_weights);
%plot(aa,xx(90,:))

