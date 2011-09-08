%
% Testing for the RFI Templating and subtraction method
%

do_subplot=0; % 1 to combine certain plots, 0 to use separate figures for each

%% Model input

lambda=0.2021;     % wavelength in meters

Nant=64+2;         % total number of antennas
refIdx=[1 2];      % indices of the reference antennas

% Model input data: sky source
srcDirPwr = [45 -90 1e-2];

% Model input data: RFI, un-comment the suitable one
% rfiDirPwr = [30 -15 0]; % no interferer (zero power), toxicity test
% rfiDirPwr = [30 -15 2]; % single interferer in one frequency channel
% rfiDirPwr = [10 -70 2]; % single interferer in one frequency channel, direction overlaps with source dir
rfiDirPwr = [30 -15 2; 40 -45 3]; % two interferers in one freq channel (Kesteven subtraction will fail)

% Model input data: gain of RFI reference antenna and count of expected RFIs
ref_ant_INR = 1e1;
Nrfi = size(rfiDirPwr,1);

% Model input data: antenna element noise and correlated noise
ac_pwr = 1e-7;
xc_pwr = 0; %1e-8;

% Automatically derived values
clight=299792458;
fHz=clight/lambda;
Nref=max(size(refIdx));
antIdx=1:Nant;
antIdx(refIdx)=[];
refIdxStr=int2str(refIdx);
refIdxStr=regexprep(refIdxStr, ' *', ','); % for figure titles
ignoreMask=[];
Mint=Nant+1;

%% Generate data

% Array element positions (for simplicity, assume reference antennas are inside array)
[eps_array]=subspcrfi_elemXYZ(Nant-Nref, 10e-2);
if (Nref==1),
    eps_ref=eps_array(:,ceil((Nant-Nref)/2));
elseif (Nref==2),
    eps_ref=[eps_array(:,1) , eps_array(:,end)];
else
    eps_ref=eps_array(:,1:Nref);
end        
eps = [eps_ref , eps_array];

% Model 1 (no reference antennas) just for comparison
nonref_eps = eps(:,antIdx); % discard positions of reference antennas
RxxV1=subspcrfi_modelgen(lambda, nonref_eps, [srcDirPwr; rfiDirPwr], ac_pwr, xc_pwr);
tRxxV1=zeros(1,Nant-Nref,Nant-Nref,2); 
tRxxV1(1,:,:,1)=RxxV1;
tRxxV1(1,:,:,2)=RxxV1;

% Model 2 (reference antennas) data used for subtraction method
RxxV2=subspcrfi_modelgen2(lambda, eps, srcDirPwr, rfiDirPwr, refIdx, ref_ant_INR, ac_pwr, xc_pwr);
tRxxV2=zeros(1,Nant,Nant,2); 
tRxxV2(1,:,:,1)=RxxV2;
tRxxV2(1,:,:,2)=RxxV2;

% for models to match, should see:
% max(max(abs(RxxV1 - RxxV2(antIdx,antIdx)))) < 1e-10 or <xc_pwr
% surf(real(RxxV1 - RxxV2(antIdx,antIdx)));

% Model 1 without RFI (and no reference antennas)
nonref_eps = eps(:,antIdx); % discard positions of reference antennas
RxxV3=subspcrfi_modelgen(lambda, nonref_eps, srcDirPwr, ac_pwr, xc_pwr);
tRxxV3=zeros(1,Nant-Nref,Nant-Nref,2); 
tRxxV3(1,:,:,1)=RxxV3;
tRxxV3(1,:,:,2)=RxxV3;


%% Apply the subtraction method to Model2 data

[briggsResult,genericResult]=subspcrfi_subtraction(tRxxV2, refIdx);
if 1, cRxxV2=genericResult; else cRxxV2=briggsResult; end

sub_briggs = squeeze(briggsResult(1,:,:,1));
sub_generic = squeeze(genericResult(1,:,:,1));
error_br_vs_noRfi = abs(sub_briggs(antIdx,antIdx)) - abs(RxxV3);
error_gen_vs_noRfi = abs(sub_generic(antIdx,antIdx)) - abs(RxxV3);

%% Apply nulling to Model1 data, and Model2 excluding reference antennas

[rfi_evalues,rfi_eterm1,rfi_evecsfull]=subspcrfi_getEV(tRxxV1);
[RxxV1nulled]=subspcrfi_nulling(tRxxV1, rfi_evalues, rfi_evecsfull, Nrfi, Mint, ignoreMask);

tRxxV2n=zeros(1,Nant-Nref,Nant-Nref,2);
tRxxV2n(1,:,:,1)=RxxV2(antIdx,antIdx);
tRxxV2n(1,:,:,2)=RxxV2(antIdx,antIdx);
[rfi_evalues,rfi_eterm1,rfi_evecsfull]=subspcrfi_getEV(tRxxV2n);
[RxxV2nulled]=subspcrfi_nulling(tRxxV2n, rfi_evalues, rfi_evecsfull, Nrfi, Mint, ignoreMask);

%% Imaging
if 1,
    % Compute dirty image of Model1
    [uvd] = subspcrfi_RtoUV(RxxV1, fHz, eps);
    img_M1_dirty = abs(fft2(uvd));

    % Compute clean/nulled image of Model1
    [uvd] = subspcrfi_RtoUV(squeeze(RxxV1nulled(1,:,:,1)), fHz, eps);
    img_M1_nulled = abs(fft2(uvd));

    % Compute dirty image of Model2, exclude reference antenna data
    [uvd] = subspcrfi_RtoUV( squeeze(tRxxV2(1,antIdx,antIdx,1)), fHz, eps(:,antIdx));
    img_M2_dirty = abs(fft2(uvd));

    % Compute clean image of subtracted Model2, exclude reference antenna data
    [uvd] = subspcrfi_RtoUV( squeeze(cRxxV2(1,antIdx,antIdx,1)), fHz, eps(:,antIdx));
    img_M2_subtr = abs(fft2(uvd));

    % Compute clean/nulled image of Model2
    [uvd] = subspcrfi_RtoUV( squeeze(RxxV2nulled(1,:,:,1)), fHz, eps);
    img_M2_nulled = abs(fft2(uvd));

    % Compute original clean image of Model1 Without RFI
    [uvd] = subspcrfi_RtoUV(RxxV3, fHz, eps);
    img_M1_norfi = abs(fft2(uvd));

%% Plotting

    fnr=1;
    if 1,
        figure(fnr);fnr=fnr+1;clf;
        if (do_subplot), subplot(2,1,1); end
        surf(abs(squeeze(tRxxV2(1,:,:,1)))), 
        view([90 90]), title(['Contaminated abs(Rxx) from M2 Nrfi=' int2str(Nrfi) ' incl. ref ant ' refIdxStr]), cscale=caxis();

        if (do_subplot), subplot(2,1,2); else figure(fnr);fnr=fnr+1;clf; end
        surf(abs(squeeze(cRxxV2(1,:,:,1)))),
        view([90 90]), title(['Subtraction-cleaned abs(Rxx) from M2 Nrfi=' int2str(Nrfi) ' incl. ref ant ' refIdxStr]); % caxis(cscale);
    end

    if 1,

        figure(fnr);fnr=fnr+1;clf;
        if (do_subplot), subplot(2,2,1); end
        surf(img_M2_dirty), view([90 90]), axis equal, axis tight,
        title(['Contaminated UV from M2 Nrfi=' int2str(Nrfi) ' w/o ref ant ' refIdxStr]);
        cscale=caxis();

        if 1,
            % img_delta = img_M2_subtr ./ (max(max(img_M2_subtr))) - img_M1_norfi ./ (max(max(img_M1_norfi)));
            img_delta = img_M2_subtr - img_M1_norfi;
            if (do_subplot), subplot(2,2,2); else figure(fnr);fnr=fnr+1;clf; end
            surf(abs(img_delta)), view([45 45]), axis tight,
            title(['Delta of abs(UV Subtracted - UV Without Added RFI) w/o ref ant ' refIdxStr]);
        else
            % img_delta = img_M2_subtr ./ (max(max(img_M2_subtr))) - img_M2_nulled ./ (max(max(img_M2_nulled)));
            img_delta = img_M2_subtr-img_M2_nulled;
            if (do_subplot), subplot(2,2,2); else figure(fnr);fnr=fnr+1;clf; end
            surf(abs(img_delta)), view([45 45]), axis tight,
            title(['Delta of abs(UV Subtracted - UV Nulled) w/o ref ant ' refIdxStr]);
        end

        if (do_subplot), subplot(2,2,3); else figure(fnr);fnr=fnr+1;clf; end
        surf(img_M2_subtr), view([90 90]), axis equal, axis tight,
        title(['Subtraction-cleaned UV from M2 Nrfi=' int2str(Nrfi) '  w/o ref ant ' refIdxStr]);
        %axis(cscale);

        if (do_subplot), subplot(2,2,4); else figure(fnr);fnr=fnr+1;clf; end
        surf(img_M2_nulled), view([90 90]), axis equal, axis tight,
        title(['Nulled UV from M2 Nrfi=' int2str(Nrfi) ' w/o ref ant ' refIdxStr]);
        %axis(cscale);
    end

    if 1,
        figure(fnr);fnr=fnr+1;clf;
        subplot(1,2,1),
            surf(error_gen_vs_noRfi);
            title('Covariance matrix error for Generic (abs(C_{cleaned})-abs(C_{norfi}))');
            view([45 15]), axis tight, cax=caxis();
        subplot(1,2,2),
            surf(error_br_vs_noRfi);
            title('Covariance matrix error for Kesteven-Briggs (abs(C_{cleaned})-abs(C_{norfi}))');
            view([45 15]), axis tight, caxis(cax);
    end
    
    if 0,
        figure(fnr);fnr=fnr+1;clf;
        if (do_subplot), subplot(2,1,1); end
        surf(img_M1_dirty), view([90 90]), axis equal, axis tight,
        title(['Contaminated UV from M1 Nrfi=' int2str(Nrfi)]);

        if (do_subplot), subplot(2,1,1); else figure(fnr);fnr=fnr+1;clf; end
        surf(img_M1_nulled), view([90 90]), axis equal, axis tight,
        title(['Nulled (Nrfi=' int2str(Nrfi) ') UV from M1']);
    end
    
end % if(imaging)
