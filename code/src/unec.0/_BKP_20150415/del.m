
figure(3); clf;


set(gcf,'color','w');

hold on;
for i=1:1.5*nMonths
    if ( ResponseCurve(i) > 0.0000001 )
    plot(i,ResponseCurve(i),'o');
end
end
plot([1:1:i],ResponseCurve(1:i));

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
