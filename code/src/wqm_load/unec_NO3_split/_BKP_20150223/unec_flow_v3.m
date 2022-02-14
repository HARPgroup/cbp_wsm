% Unit Nutrient Export Curve [UNEC - unique]
% Developed by Gopal Bhatt (Penn State) & Gary Shenk (CBPO- EPA)

ParamLeadEdge  = 0*12.0+0;
ParamPeakConc  = 0.0;
ParamTrailEdge = 1200.0; % months
ParamExponent1 = 0.1;
ParamExponent2 = 0.05;
ParamNormalize = 1;

nYears  = 15;
nMonths = nYears*12;
xLabel  = 'Years';
xTickInt = 12;
xTick = 12/xTickInt;

PAUSE1 = 3;

Conservative     = 1;

OnePulseInput   = 0;
TwoPulseInput   = 0;
ContinuousInput = 1;
SinusoidalInput = 0;
InterAnnualInputVariability = 1;

ConstantFlow = 0;
VariableFlow = 1;
InterAnnualFlowVariability  = 1;

ResponseCurve = zeros(ParamTrailEdge,1);
ResponseCurveTotal = 0;

for i = 1:ParamTrailEdge
    if ( i < ParamLeadEdge )
        ResponseCurve(i,1) = 0;
    elseif ( i < ParamPeakConc )
        %ResponseCurve(i,1) = exp(  ParamExponent1 * ( i - ParamPeakConc ) );
        ResponseCurve(i,1) = sqrt (  i - ParamLeadEdge ) / (sqrt (ParamPeakConc - ParamLeadEdge ));
    else
        ResponseCurve(i,1) = exp( -ParamExponent2 * ( i - ParamPeakConc ) );
    end
    
    %if ( ParamNormalize == 1 )
        ResponseCurveTotal = ResponseCurveTotal + ResponseCurve(i,1);
    %else
    %    ResponseCurveTotal = 1;
    %end
end

MeanResidenceTime = 0;
for i = 1:ParamTrailEdge
    if (MeanResidenceTime < 0.5 * ResponseCurveTotal && MeanResidenceTime + ResponseCurve(i,1) >= 0.5 * ResponseCurveTotal)
        MeanResidenceTime = i + 1
    else
        MeanResidenceTime = MeanResidenceTime + ResponseCurve(i,1);
    end
    
    if ( ParamNormalize == 1 )
        ResponseCurve(i,1) = ResponseCurve(i,1) / ResponseCurveTotal;
    end
end


%ResponseCurve(1:24)
figure(1); clf;
pause(PAUSE1);
subplot(2,3,1)
set(gcf,'color','w');

hold on;
for i=1:1.5*nMonths
    if ( ResponseCurve(i) > 0.0000001 )
plot(i,ResponseCurve(i),'o-');
end
end

set(gca,'Ylim',[0 1.1*(max(ResponseCurve(1:nMonths)))]);
set(gca,'FontSize',12);
set(gcf,'DefaultTextFontSize',20);
set(gcf,'DefaultTextFontName','Courier New');

title(['(1) Unit Nutrient Export Curve'],'FontWeight','bold');
ylabel(['Probability Density Function']);
xlabel([xLabel]);
%set(gca,'xtick',[])
%set(gca,'xticklabel',[])
set(gca,'Xlim',[1 1.5*nMonths]);
set(gca,'xtick',[1:2*xTickInt:1.5*(nMonths-xTickInt)]);
set(gca,'xticklabel',1:2*xTickInt/xTickInt:1.5*(nMonths-xTickInt)/xTickInt);
set(gca,'box','on');
text(nMonths/2,max(ResponseCurve(1:nMonths))*.95,0,['Lead Edge  = ' int2str(ParamLeadEdge) ' months']);
text(nMonths/2,max(ResponseCurve(1:nMonths))*.88,0,['Peak Conc  = ' int2str(ParamPeakConc) ' months']);
text(nMonths/2,max(ResponseCurve(1:nMonths))*.81,0,['Trail Edge = ' int2str(ParamTrailEdge) ' months']);
%text(nMonths/2,max(ResponseCurve(1:nMonths))*.74,0,['Exp. Rise  = ' num2str(ParamExponent1,3) ' ']);
text(nMonths/2,max(ResponseCurve(1:nMonths))*.74,0,['Exp. Ress  = ' num2str(ParamExponent2,3) ' ']);

Inputs = zeros(nMonths,1);

cResponseCurve = zeros(ParamTrailEdge,1);

TransTimeDist = zeros(nMonths,nMonths);

Flow = zeros(nMonths,1);

Load = zeros(nMonths,1);

Qo = 100;

%TARGET = 1.5 * (nMonths/12); %1.5 lb/ac/yr
%TARGET = 1.0;

InterAnnual = 1;
%Flow = 1 + Qo.*rand(nMonths,1);
for i = 1:nMonths
    if (mod(i-1,12) == 0 && InterAnnualFlowVariability == 1)
        InterAnnual = rand(1);
    end
    
    if (ConstantFlow == 1) Flow(i) = Qo; end
    if (VariableFlow == 1) Flow(i) = InterAnnual * (Qo) * 0.5 * ( 1 + sin(2*pi*mod(i,12)/12) ); end;

end
%return



%
if (OnePulseInput == 1 || TwoPulseInput == 1) Inputs(5,1)  = 1; end
if (TwoPulseInput == 1) Inputs(17,1) = 1; end
%

%
InterAnnual = 1;
for i = 1:nMonths
    if (mod(i-1,12) == 0 && InterAnnualInputVariability == 1)
        InterAnnual = rand(1);
    end
    if (ContinuousInput == 1) Inputs(i,1) = 1; end
    if (SinusoidalInput == 1) Inputs(i,1) = InterAnnual * 0.5 * ( 1 + sin(2*pi*mod(i,12)/12) ); end
    %Inputs(i,1) = 0.5 * ( 1 + sin(2*pi*mod(i,12)/12) ) / (1*(12+i)/12);
end
%

TARGET = sum(Inputs);
if ( Conservative == 0 )
    TARGET = 0.10 * TARGET;
end

%Inputs(1:nMonths)
%figure(2); plot(Inputs(1:nMonths));
%figure(1);
subplot(2,3,4)
set(gcf,'color','w');
bar(Inputs(1:nMonths),'b');
set(gca,'FontSize',12)
set(gca,'Ylim',[0 1.1*max(max(Inputs))]);
set(gca,'Xlim',[0 nMonths]);
%set(gca,'ylable','load kg/ac');
title(['(2) Nutrient Application'],'FontWeight','bold');
ylabel(['Input Load (lb/ac)']);
xlabel([xLabel]);
%set(gca,'xtick',[])
%set(gca,'xticklabel',[])
set(gca,'xtick',[1:12:nMonths]);
set(gca, 'xticklabel',1:12/xTickInt:nMonths/xTickInt);


subplot(2,3,5)
set(gcf,'color','w');
plot(Flow(1:nMonths),'b');
set(gca,'FontSize',12)
set(gca,'Ylim',[0 1.1*max(Flow)]);
set(gca,'Xlim',[0 nMonths]);
%set(gca,'ylable','load kg/ac');
title(['(4) Flow'],'FontWeight','bold');
ylabel(['Flow (ac-ft/s)']);
xlabel([xLabel]);
%set(gca,'xtick',[])
%set(gca,'xticklabel',[])
set(gca,'xtick',[1:12:nMonths]);
set(gca, 'xticklabel',1:12/xTickInt:nMonths/xTickInt);


for i = 1:nMonths
    for j = i:nMonths
        cResponseCurve(j,1) = cResponseCurve(j,1) + ...
        ResponseCurve(j-i+1,1) * ...
        Inputs(i,1);
    
        %if ( Inputs(i,1) > 0 )
            %TransTimeDist(j-i+1,j) = ResponseCurve(j-i+1) * Inputs(i,1);
        %else
        %    TransTimeDist(j,i) = 0.0;
        %end
    end
end

subplot(2,3,2); hold on;
set(gcf,'color','w');
set(gca,'box','on');
plot(cResponseCurve(1:nMonths),'k');
set(gca,'FontSize',12)
set(gca,'Ylim',[0 1.0*max(cResponseCurve(1:nMonths))]);
set(gca,'Xlim',[0 nMonths]);
title(['(3) Aggregate Nutrient Export'],'FontWeight','bold');
ylabel(['Concentration']);
xlabel([xLabel]);
set(gca,'xtick',[1:12:nMonths]);
set(gca, 'xticklabel',1:12/xTickInt:nMonths/xTickInt);

figure(2);
[ax,p1,p2] = plotyy([1:1:nMonths],Flow,[1:1:nMonths],cResponseCurve(1:nMonths));
set(gcf,'color','w');
xlabel([xLabel]);
ylabel(ax(1),'Flow');
ylabel(ax(2),'Concentration');
set(gca,'FontSize',12)
set(ax,'xtick',[])
set(ax,'xticklabel',[])
set(gca,'xtick',[1:12:nMonths]);
set(gca, 'xticklabel',1:12/xTickInt:nMonths/xTickInt);

%return;

figure(1);


subplot(2,3,6)

set(gcf,'color','w');
%subplot(1,1,1);
%title('Transit Time Distribution');
%set(gca,'YDir','reverse');
%set(gca,'XLim',[0,5]);
%y4 = axes
%set(y4,'Ydir','reverse')
    %max([TransTimeDist])
    
temp = 0;
for i = 1:nMonths
    Load(i) = Flow(i) * cResponseCurve(i);
    temp = temp + Load(i);
end
Load = Load * (TARGET/temp);

subplot(2,3,3)
set(gcf,'color','w');
plot(Load(1:nMonths),'m'); hold on;
set(gca,'FontSize',12)
set(gca,'Ylim',[0 1.1*max(Load)]);
set(gca,'Xlim',[0 nMonths]);
%set(gca,'ylable','load kg/ac');
title(['(5) Nutrient Export'],'FontWeight','bold');
ylabel(['Export Load (lb/ac)']);
xlabel([xLabel]);
set(gca,'xtick',[1:12:nMonths]);
set(gca, 'xticklabel',1:12/xTickInt:nMonths/xTickInt);


for i = 1:nMonths
    for j = i:nMonths
        TransTimeDist(j-i+1,j) = ResponseCurve(j-i+1) * Inputs(i,1) * Flow(j,1) * (TARGET/temp);
    end
end
    
for i = 1:nMonths
    subplot(2,3,2)
    plot(i,cResponseCurve(i),'or')
    
    subplot(2,3,3)
    %set(gca,'box','on');
    %plot(i,Flow(i)*cResponseCurve(i)*(TARGET/temp),'or');
    plot(i,Load(i),'*r');
    
    subplot(2,3,6)
    barh(TransTimeDist(:,i),'r');
    %set(gca,'ylable','Transit Time');
    
    %y4 = 
    %axis([0 0.2 0 nMonths])
    set(gca,'FontSize',12)
    set(gca,'Xlim',[0 1.1*max(max(TransTimeDist))]);
    set(gca,'Ylim',[0 nMonths]);
    set(gca,'ytick',[1:12:nMonths]);
    set(gca, 'yticklabel',1:12/xTickInt:nMonths/xTickInt);
    set(gca,'Ydir','reverse');
    title(['(6) Transit Time Distribution, t = ' int2str(i) ' months'],'FontWeight','bold');
    ylabel(['AGE (' xLabel ')']);
    xlabel(['Export (lb/ac)']);

    %y4 = axes;
    %set(y4,'Ydir','reverse');
    %set(gca,'xlim','[0 5]');
    pause(0.015);
end