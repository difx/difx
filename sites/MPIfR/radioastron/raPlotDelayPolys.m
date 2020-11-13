% function [xx,yy] = raPlotDelayPolys(coeffile)
%
% Plots the delay polynomial data found in the specified
% delay coefficients file. Input: coeffile file name and path.
%
% Each line of the coefficients file represents one polynomial segment.
% The format of each line is:
%    <mjd start> <mjd end> <coeff of t^0> <coeff of t^1> <coeff of t^2> ...
%
% Time t is in [seconds]. Each polynomial is relative to 0 sec.
% The delay poly is expected to evaluate into [microseconds].
%
% Polynomial files can be produced using DiFX raClosedLoop_convert.py.
% It converts DiFX/CALC9 .im files or ASC poly files into the above format.
%
function [xx,yy] = raPlotDelayPolys(coeffile)

    figure();

    dly_scale = 1e-6; % poly produces [usec]

    escaped_filename = strrep(coeffile, "\", "\\");
    escaped_filename = strrep(escaped_filename, "_", "\_");
    colorcodes = ['r','g','b','c','m'];
    cm = colormap('jet');
    tstep = 1;

    polys = dlmread(coeffile);
    M = size(polys,1);
    %M = 8;
    tstart = min(polys(:,1));
    tstop = max(polys(:,2));
    dly_plot_ref = 0;  % or:  polys(1,3)*dly_scale;
    if tstop < tstart
        fprintf(1, 'Error: coeff file format seems strange, stop time lies before start time\n')
        return
    end
    
    N = abs(tstop - tstart)/tstep + 1;
    cmstep = ceil(size(cm,1)/M);
    
    last_dly = 0;
    gaps = [];
    xx = [];
    yy = [];

    subplot(3,1,1);
    for n=1:M
        
        % time coverage of segment
        tsegmentbegin = polys(n,1);
        tsegmentend = polys(n,2);
        tinterval = floor(tsegmentend - tsegmentbegin);
        t = tsegmentbegin:tstep:tsegmentend;

        % polynomial coeffs of segment
        coeffs = fliplr(polys(n,3:end));
        %coeffs(end-1)=0; % for testing how >2nd order looks like
        
        % undefined?: reference time of polynomial segment
        %   tref = tsegmentbegin : appears to be clearly correct for CALC9,
        %                          the resulting polys have zero gap
        %   tref = tsegmentbegin : ASC max gap is ~10 nanosec
        %   tref = mean(begin,end) : large gaps
        tref = tsegmentbegin;
        dly = polyval(coeffs, (tref - tsegmentbegin) + 0:tstep:tinterval);
        dly = dly .* dly_scale;
        
        % Concatenate in time
        xx = [xx, t(1:(end-1))];
        yy = [yy, dly(1:(end-1))];
        
        % Plot segments in different colors
        cidx = mod(1+cmstep*(n-1), size(cm,1)) + 1;
        plot(t-tstart,dly-dly_plot_ref,'-.','LineWidth',1,'color',cm(cidx,:));
        hold on;
        
        % Keep track of discontinuities between poly segments
        if n>1
            gaps(end+1) = dly(1) - last_dly;  % usec
            last_dly = dly(end);
        else
            last_dly = dly(end);
        end
    end
    title(['Polynomial segments in ' escaped_filename]);
    xlabel('Time since start of first polynomial');
    ylabel('Delay since scan start (s)');
    set(gca,'XMinorTick','on','YMinorTick','on');
    axis tight;
    
    subplot(3,1,2);
    yylin = polyval(polyfit(xx,yy,1), xx);
    yyquadr = polyval(polyfit(xx,yy,2), xx);
    yyaxis left, plot(xx, yy - yylin, 'b-.'), hold on; ylabel('Delay (s)');
    yyaxis right, plot(xx, yy - yyquadr, 'r-.'); ylabel('Delay (s)');
    title('Deviation from pure linear and pure quadratic');
    legend('Linear dev.','Quadratic dev.');
    xlabel('Time (MJD)');
    axis tight;
    ylim(max(abs(ylim)).*[-1 1]);

    subplot(3,1,3);
    plot(gaps*1e12, 'ro-.');
    set(gca,'XMinorTick','on','YMinorTick','on');
    title('Gap ups and gap downs between segments');
    ylabel('Delay poly gap (ns)');
    xlabel('Polynomial pair n,n+1');
    ylim(max(abs(ylim)).*[-1 1]);
    ylim(10.*[-1 1]);
end

% Comparing two .im files:
% function raPlotDelayPolysCMP()
%    [x1,y1]=raPlotDelayPolys('C:\Users\jwagner\Dropbox\Apps\ext-sync\gs042_1001.im.closedloop.matlab');
%    [x2,y2]=raPlotDelayPolys('C:\Users\jwagner\Dropbox\Apps\ext-sync\gs042_1001.im.closedloop_v102.matlab');
%    [x3,y3]=raPlotDelayPolys('C:\Users\jwagner\Dropbox\Apps\ext-sync\gs042_1001.im.closedloop_v103.matlab');
%    figure();
%    subplot(2,1,1);
%    plot(x1,y1,'ro-'), hold on;
%    plot(x2,y2,'gx-'), hold on;
%    plot(x3,y3,'b+-'), hold on;
%    title('Delay polynomials');
%    ylabel('Delay (seconds)');
%    legend('Poly 1','Poly 2','Poly 3');
%    subplot(2,1,2), clf;
%    plot(x1,(y2-y1)*1e12,'rx-'); hold on;
%    plot(x1,(y3-y1)*1e12,'go-.');
%    plot(x1,(y3-y2)*1e12,'b+-.');
%    title('Difference between the two polynomials');
%    ylabel('Difference (nanoseconds)');
%    legend('Poly 2 - poly 1', 'Poly 3 - poly 1', 'Poly 3 - poly 2');
%    ylim(100*[-1,1]);
% end
