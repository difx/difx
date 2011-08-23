
% Apertif ch#35 = rfi, ch#17 = likely free of rfi
ch=31;

orig_spec=zeros(Nch,Nant);
null_spec=zeros(Nch,Nant);
for cc=1:Nch,
    
    Ron=squeeze(alldata(1,:,:,cc));
    Roff=squeeze(alldata(2,:,:,cc));
    RonN=squeeze(nulldata(1,:,:,cc));
    RoffN=squeeze(nulldata(2,:,:,cc));

    deltaAC=abs(diag(Ron))-abs(diag(Roff));
    %deltaACN=abs(diag(RonN)-diag(RoffN));
    deltaACN=abs(diag(RonN))-abs(diag(RoffN)); %TODO, proper method?
    
    if 0,
        % keep baseline, just show On-source
        orig_spec(cc,:)=abs(diag(Ron));
        null_spec(cc,:)=abs(diag(Roff));
    else
        % remove baseline
        orig_spec(cc,:)=deltaAC;
        null_spec(cc,:)=deltaACN;
    end
end

figure(1),subplot(1,2,1),surf(orig_spec),view([90 90]),title('Orignal data Ron-Roff'),
subplot(1,2,2),surf(null_spec),view([90 90]),title('Nulled Ron - nulled Roff');

% note: max power in (Ron-Roff) is in element 31
pel=31;figure(2),subplot(1,2,1),plot(orig_spec(:,pel)),
title(['Element ' int2str(pel) ' original']), axx=axis();
subplot(1,2,2),plot(null_spec(:,pel)), axis(axx),
title(['Element ' int2str(pel) ' nulled']);

figure(3),
subplot(2,1,1),plot(mean(orig_spec,2)),title('Mean over elements, original'),
subplot(2,1,2),plot(mean(null_spec,2)),title('Mean over elements, nulled data');

%% try one additional thing, instead of null(Ron)-Null(Roff) plot null(Ron-Roff):
if 0,
    Rdiff = alldata(1,:,:,:) - alldata(2,:,:,:);
    [diff_evalues,diff_eterm1,diff_evecsfull]=subspcrfi_getEV(Rdiff);
    [diffnulldata]=subspcrfi_nulling(Rdiff, diff_evalues, diff_evecsfull, 4); % Nrfi=4

    diffnull_spec=zeros(Nch,Nant);
    for cc=1:Nch,
       diffnull_spec(cc,:)=abs(diag(squeeze(diffnulldata(1,:,:,cc))));
    end
    figure(4),subplot(1,2,1),surf(orig_spec),view([90 90]),title('Orignal data Ron-Roff'),
    subplot(1,2,2),surf(diffnull_spec),view([90 90]),title('Nulled (Ron-Roff)');
    
    % => result: this fails, perhaps due to non-Hermitian matrices
end

%% plot raw autocorrelations
if 0,
    figure(1)
    subplot(2,2,1),plot(abs(diag(Ron))),title('Ron AC orig');
    subplot(2,2,2),plot(abs(diag(Roff))),title('Roff AC orig');
    subplot(2,2,3),plot(abs(diag(RonN))),title('Ron AC nulled');
    subplot(2,2,4),plot(abs(diag(RoffN))),title('Roff AC nulled');

    figure(2),
    subplot(2,1,1),plot(deltaAC),
        title(['Original AC data abs(Ron-Roff) on Ch' int2str(ch)]);
    subplot(2,1,2),plot(abs(deltaACN)),
        title(['Nulled AC data abs(Ron-Roff) on Ch' int2str(ch)]);
end

