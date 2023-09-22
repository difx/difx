
Argb=imread('LOFAR_sun_rfi.png','png');
Aimg=rgb2gray(Argb);
Aimg=im2double(Aimg);
Argb=im2double(Argb);

Nfreq=size(Aimg,1);
Ntime=size(Aimg,2);

[uu,ss,vv]=svd(Aimg);
for L_null=1:(rank(ss)-5),

    % show original image
    figure(gcf);
    imshow(Argb, 'InitialMag', 'fit');
    hold on;

    % raw nulling of dominant singular values
    % (could replace with mean of remaining values instead of with 0)
    if 1,
        ssn=zeros(size(ss));
        for ii=1:L_null, ssn(ii,ii)=ss(ii,ii); end
    else
        ssn=ss;
        for ii=1:L_null, ssn(ii,ii)=0; end        
    end
    
    % generate a flag mask
    Anew=uu*ssn*vv';
    sigm=(Anew > 1.3*mean(mean(Anew)));
    Fpct=100*sum(sum(sigm))/(size(sigm,1)*size(sigm,2));
    rgb=grey2rgb(sigm, 128,128,0);
    
    % overlay the flag mask
    h=imshow(rgb);
    set(h, 'AlphaData', sigm);
    
    hdrStr = ['L_{null}=' int2str(L_null) ' F=' num2str(Fpct, '%.2f') '%'];
    text(10,10,hdrStr,'BackgroundColor',[1 1 1]);
    title(hdrStr);
    hold off;
    
    Mmovie(L_null)=getframe();    
    % [tmp]=input('next');
end
