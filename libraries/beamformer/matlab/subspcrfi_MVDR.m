% Compute MVDR weights for a single beam and channel, optionally performing 
% Cox Projection RB-MVDR (w = w_classic + b * (w - w||s))
%
% Input: 
%   v_steer = Nant x 1  = steering vector i.e. plane-wave phases at
%                         antennas for the desired signal direction
%   Rxx = Nant x Nant   = covariance matrix (original or nulled)
%   b = scalar          = 0 classic BF, 1 for MVDR BF, >1 weight for Cox 
%
% Returns: w = Nant x 1 = complex weight vector
%
function [w]=subspcrfi_MVDR(v_steer, Rxx, b)

if (size(v_steer,1) < size(v_steer,2))
    v_steer = transpose(v_steer);
end

%% Classical non-adaptive beamformer
if (b<=0),
    % complex unit vector into steering direction gives the beamformer weights
    e_steer = v_steer / sqrt((v_steer')*v_steer);
    w = e_steer;
    return;
end

%% MVDR beamformer with inverse (or pseudo-inv) of covariance
if (b<=1),
    RxxI = inv(Rxx);
    inv_power = (v_steer') * RxxI * v_steer;
    w = (RxxI * v_steer) ./ inv_power;
    return;
end
  
%% RB-MVDR beamformer
if (b>1),
    
    % start with MVDR
    RxxI = inv(Rxx);
    inv_power = (v_steer') * RxxI * v_steer;
    w_mvdr = (RxxI * v_steer) ./ inv_power;
  
    % complex unit vector into steering direction
    e_steer = v_steer / sqrt((v_steer')*v_steer);
    
    % decompose w_mvdr into w1 vector projection, w2 vector rejection
    w1 = ((w_mvdr') * e_steer) * e_steer;
    w2 = w_mvdr - w1;
  
    % compute weights
    w = w1 + b*w2;
    return;
end    

%% Default
w = ones(size(v_steer));
