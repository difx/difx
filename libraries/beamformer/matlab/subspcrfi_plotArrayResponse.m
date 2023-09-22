% Given an electrical beam weighting and array configuration, calculates
% response of the array in all (phi,theta) directions.
%
% Antenna element response is hard-coded, please edit the source code.
% Default response is F(phi,theta)==1
%
% Input:
%   lambda = 1x1 = wavelength in meters
%   elempos = 3 x Nant = x,y,z coordinates of array elements in meters
%   weights = Nant x 1 = complex antenna weights for beamforming
% Output:
%   img = Nang x Nang = gains into (phi,theta) directions
%   angles = 1 x Nang = corresponding angles ((phi,theta) in "angles X angles" product)
%
% Example: 
%   lambda=10e-2; Ne=16;
%   [eps]=subspcrfi_elemXYZ(Ne, lambda);
%   pt_weights=conj(subspcrfi_steer(45, 30, lambda, eps));
%   figure(1),[xx,aa]=subspcrfi_plotArrayResponse(lambda, eps, pt_weights);
%   figure(2),plot(aa,abs(xx(:,90))');
%   figure(3),polar(deg2rad(aa),abs(xx(:,90))');
%
function [img,angles]=subspcrfi_plotArrayResponse(lambda, elempos, weights)

  % Loop through all angles
  angles_deg = -90:1:90;
  angles = deg2rad(angles_deg);
  Nang = max(size(angles));
  AFlist = zeros(Nang,Nang);
  Ylist = zeros(Nang,Nang);
  angles_grid_theta = zeros(Nang,Nang);
  angles_grid_phi = zeros(Nang,Nang);

  for ii=1:Nang,
      phi = angles(ii);
      for jj=1:Nang,
        theta = angles(jj);
        angles_grid_theta(ii,jj) = theta;
        angles_grid_phi(ii,jj) = phi;
        
        a = subspcrfi_steer(phi, theta, lambda, elempos); % = phase delays at each element
        AF = transpose(a) * weights;
        
        % Choose array element response
        gain = 1; 
        % gain = (cos(theta)*cos(phi))^2; 
        % gain = (15/2)*(sin(phi)^2*cos(phi)^2); % 2-lobe, gains at +45deg and (180-45)deg
        % gain = (cos(pi*0.5*cos(theta))-cos(pi*0.5)) / sin(theta); % half-wave dipole
        % gain = elementPattern(theta,phi)
        
        Y = gain * AF;    
        AFlist(ii,jj) = AF;
        Ylist(ii,jj) = Y;
     end
  end

  % Data into cartesian coordinates
  [sphX,sphY,sphZ] = sph2cart(angles_grid_theta, angles_grid_phi, abs(Ylist));

  % Prepare output
  img = Ylist;
  angles = angles_deg;
  
  % Plotting
  clf;
  mid_ang = find(angles==0);
  subplot(2,2,1), surf(angles,angles, abs(img)), 
     axis tight, title('Directed array response (phi,theta) vs gain');
  subplot(2,2,2), mesh(sphX,sphY,sphZ), 
     axis tight, title('Directed array response plotted in cartesian coordinates');
  subplot(2,2,3), plot(angles,abs(img(:,mid_ang))'), 
     title(['Response over phi at fixed theta=' num2str(angles(mid_ang)) 'deg']);
  subplot(2,2,4), polar(deg2rad(angles),abs(img(:,mid_ang))') , 
     title(['Response over phi as polar plot with fixed theta=' num2str(angles(mid_ang)) 'deg']); 

