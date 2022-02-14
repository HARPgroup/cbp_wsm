% Unit Nutrient Export Curve [UNEC - unique]

ParamLeadEdge  = 0*12.0+0;
ParamPeakConc  = 0.0;
ParamTrailEdge = 1200.0; % months
ParamExponent1 = 0.1;
ParamExponent2 = 0.05;
ParamNormalize = 1;

%nMonths = 25*12;
nYears  = 15;
nMonths = nYears*12;

PAUSE1 = 0;

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
subplot(2,2,1)
set(gcf,'color','w');

plot(ResponseCurve(1:1.5*nMonths),'o-');
set(gca,'Ylim',[0 1.1*(max(ResponseCurve(1:nMonths)))]);
set(gca,'FontSize',12);
set(gcf,'DefaultTextFontSize',20);
set(gcf,'DefaultTextFontName','Courier New');

title(['(1) Unit Nutrient Export Curve'],'FontWeight','bold');
ylabel(['Probability Density Function']);
xlabel(['Months']);
text(nMonths/2,max(ResponseCurve(1:nMonths))*.95,0,['Lead Edge  = ' int2str(ParamLeadEdge) ' months']);
text(nMonths/2,max(ResponseCurve(1:nMonths))*.88,0,['Peak Conc  = ' int2str(ParamPeakConc) ' months']);
text(nMonths/2,max(ResponseCurve(1:nMonths))*.81,0,['Trail Edge = ' int2str(ParamTrailEdge) ' months']);
%text(nMonths/2,max(ResponseCurve(1:nMonths))*.74,0,['Exp. Rise  = ' num2str(ParamExponent1,3) ' ']);
text(nMonths/2,max(ResponseCurve(1:nMonths))*.74,0,['Exp. Ress  = ' num2str(ParamExponent2,3) ' ']);

Inputs = zeros(nMonths,1);

cResponseCurve = zeros(ParamTrailEdge,1);

TransTimeDist = zeros(nMonths,nMonths);
Qo = 100;
TARGET = 1.5 * (nMonths/12);
%Flow = zeros(nMonths);
%Flow = ones(nMonths);
%Flow = 1 + Qo.*rand(nMonths,1);
for i = 1:nMonths
    Flow(i) = (Qo/2) * ( 1 + sin(2*pi*mod(i,12)/12) );
end
%return
%
Inputs(5,1)  = 1;
Inputs(17,1) = 1;
%

%
for i = 1:nMonths
    Inputs(i,1) = 1;
    %Inputs(i,1) = 0.5 * ( 1 + sin(2*pi*mod(i,12)/12) );
    %Inputs(i,1) = 0.5 * ( 1 + sin(2*pi*mod(i,12)/12) ) / (1*(12+i)/12);
end
%

%
%Inputs(17,1) = 0.5;
%

%Inputs(1:nMonths)
%figure(2); plot(Inputs(1:nMonths));
%figure(1);
subplot(2,2,2)

set(gcf,'color','w');
bar(Inputs(1:nMonths),'b');
set(gca,'FontSize',12)
set(gca,'Ylim',[0 1.1*max(max(Inputs))]);
set(gca,'Xlim',[0 nMonths]);
%set(gca,'ylable','load kg/ac');
title(['(2) Nutrient Input'],'FontWeight','bold');
ylabel(['LOAD (lb/ac)']);
xlabel(['Months']);


for i = 1:nMonths
    for j = i:nMonths
        cResponseCurve(j,1) = cResponseCurve(j,1) + ...
        ResponseCurve(j-i+1,1) * ...
        Inputs(i,1);
    
        %if ( Inputs(i,1) > 0 )
            TransTimeDist(j-i+1,j) = ResponseCurve(j-i+1) * Inputs(i,1);
        %else
        %    TransTimeDist(j,i) = 0.0;
        %end
    end
end
%figure(3);
subplot(2,2,3); hold on;
set(gcf,'color','w');
plot(cResponseCurve(1:nMonths),'k');
set(gca,'FontSize',12)
title(['(3) Aggregate Nutrient Export'],'FontWeight','bold');
ylabel(['Concentration']);
xlabel(['Months']);

%figure(4);
subplot(2,2,4)

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
    temp = temp + Flow(i) * cResponseCurve(i);
end
    
for i = 1:nMonths
    subplot(2,2,3)
    set(gca,'box','on');
    plot(i,Flow(i)*cResponseCurve(i)*(TARGET/temp),'or');
    
    subplot(2,2,4)
    barh(Flow(i)*TransTimeDist(:,i),'r');
    %set(gca,'ylable','Transit Time');
    
    %y4 = 
    %axis([0 0.2 0 nMonths])
    set(gca,'FontSize',12)
    set(gca,'Xlim',[0 1.1*Qo*max(max(TransTimeDist))]);
    set(gca,'Ylim',[0 nMonths]);
    set(gca,'Ydir','reverse');
    title(['(4) Transit Time Distribution, t = ' int2str(i) ' months'],'FontWeight','bold');
    ylabel(['AGE (months)']);

    %y4 = axes;
    %set(y4,'Ydir','reverse');
    %set(gca,'xlim','[0 5]');
    pause(0.15);
end