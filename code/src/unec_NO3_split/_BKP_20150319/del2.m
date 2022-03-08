figure(3);clf;
set(gcf,'color','w');
yy = 13;
i = 12*(yy-1)+12;
%subplot(2,3,6)
barh(TransTimeDist(:,i),'r');

set(gca,'FontSize',12)
    set(gca,'Xlim',[0 1.1*max(max(TransTimeDist(:,12*(yy-1)+1:12*(yy-1)+12)))]);
    set(gca,'Ylim',[0 nMonths]);
    set(gca,'ytick',[1:12:nMonths]);
    set(gca, 'yticklabel',1:12/xTickInt:nMonths/xTickInt);
    set(gca,'Ydir','reverse');
    title(['(6) Transit Time Distribution, t = ' int2str(i) ' months'],'FontWeight','bold');
    ylabel(['AGE (' xLabel ')']);
    xlabel(['Export (lb/ac)']);