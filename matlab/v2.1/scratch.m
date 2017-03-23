% scratch work...
clc;
clear;

n_wy_peaks = 2;
wy_start = '10/01'; % start of wy.
wy_end = '09/30'; % end of wy.
all_obs_peaks = datenum(1999,09,30)*ones(1096,1)+[1:1096]';
all_obs_peaks = [all_obs_peaks,rand(1096,1)]

% trim beginning of data set to first complete wy...
for i = 1:length(all_obs_peaks),
    if datestr(all_obs_peaks(i,1),6) == wy_start,
        trim_index = i
        break;
    end
end
all_obs_peaks = all_obs_peaks(trim_index:end,:);


offset_peaks = [NaN,NaN];
max = -realmax;
% find max peaks (this loop will not include last wy if it is incomplete).
for i = 1:n_wy_peaks,
    for j = 1:length(all_obs_peaks),
        if all_obs_peaks(j,2) > max,
            date = all_obs_peaks(j,1);
            max = all_obs_peaks(j,2);
            max_index = j;
        end
        if datestr(all_obs_peaks(j,1),6) == wy_end, % reached end of water year.
            offset_peaks = [offset_peaks;[date,max]]; % store max within wy in output array.
            all_obs_peaks(max_index,:) = [date,-realmax]; % remove max found within wy in input array (for finding later maxes).
            max = -realmax; % reset max.
            disp(['found ',num2str(i),'th max peak for year ',datestr(date,10),'. j=',num2str(j)]) %?
        end
    end
end
offset_peaks = sortrows(offset_peaks(2:end,:),1);

output = [datestr(offset_peaks(:,1)),num2str(offset_peaks(:,2))]


% col = 1;
% row = 1;
% for i = 1:length(a),
%     b(row,col) = a(i,2); % data.
%     d(row,col) = a(i,1); % dates.
%     row = row + 1;
%     if a(i,1) == wy_end, % reached end of water year - start new column.
%         col = col + 1;
%         row = 1;
%     end
% end
% b
% d
% 
% if (d(1,1)~=wy_start) & (d(end,1)~=wy_end), b(:,1)=[]; d(:,1)=[]; end
% if (d(1,size(b,2))~=wy_start) & (d(end,size(b,2))~=wy_end), b(:,size(b,2))=[]; d(:,size(b,2))=[]; end
% b
% d

% 
% b = (a(:,1)==wy_end);
% b = [[1:length(b)]',b] % add index column.
% b(b(:,2)==0,:) = []; % get indexes of wy_end.
% b = b(:,1)
% if a(1,2) == wy,
%     a(1,:) = [];
% else
%     
% end
% if a(end,2) == wy,
%     n_wy = sum(b) - 1;
% else
%     n_wy = sum(b);
% end
% n_wy



% close;
% % set params - witdth of stats box and % buffer space around plots.
% st_w = 0.15;
% buff = 0.15;
% 
% % find axes widths w/o buffer.
% ts_w = 1-st_w;
% so_w = ts_w/2;
% cu_w = ts_w/2;
% 
% % adjust widths for buffer.
% ts_w = ts_w - 2*buff*ts_w;
% so_w = so_w - 2*buff*so_w;
% cu_w = cu_w - 2*buff*cu_w;
% 
% % find axes heights w/o buffer.
% ts_h = 0.5;
% so_h = 0.5;
% cu_h = 0.5;
% st_h = 1;
% 
% % adjust heights for buffer.
% ts_h = ts_h - 2*buff*ts_h;
% so_h = so_h - 2*buff*so_h;
% cu_h = cu_h - 2*buff*cu_h;
% 
% % find left position of axes.
% ts_l = buff*(1-st_w);
% so_l = buff*(1-st_w)/2;
% cu_l = (1-st_w)/2 + buff*(1-st_w)/2;
% st_l = 1-st_w;
% 
% % find bottom position of axes.
% ts_b = 0.5 + buff*0.5;
% so_b = buff*(1-st_w);
% cu_b = buff*(1-st_w);
% st_b = 0;
% 
% f = figure;
% %  [left, bottom, width, height] 
% a_ts = axes('Position',[ts_l,ts_b,ts_w,ts_h],'Box','on','XTick',[],'YTick',[]); % TS
% a_pp = axes('Position',[ts_l,ts_b,ts_w,ts_h],'Box','on','XTick',[],'YTick',[],'YAxisLocation','left'); % PP
% a_so = axes('Position',[so_l,so_b,so_w,so_h],'Box','on','XTick',[],'YTick',[]); % S VS O
% a_cu = axes('Position',[cu_l,cu_b,cu_w,cu_h],'Box','on','XTick',[],'YTick',[]); % CUM
% a_st = axes('Position',[st_l,st_b,st_w,st_h],'Box','on','XTick',[],'YTick',[]); % STATS

% close;
% st_w = 0.15; % param
% f = figure;
% %  [left, bottom, width, height] 
% a_ts = axes('Position',[0.055,0.619,0.685,0.312],'Box','on','XTick',[],'YTick',[]); % TS
% a_pp = axes('Position',[0.055,0.619,0.685,0.312],'Box','on','XTick',[],'YTick',[]); % PP
% set(a_pp,'YAxisLocation','right','Color','none','Layer','top');
% get(a_pp)
% a_so = axes('Position',[0.055,0.132,0.297,0.420],'Box','on','XTick',[],'YTick',[]); % S VS O
% a_cu = axes('Position',[0.443,0.132,0.297,0.420],'Box','on','XTick',[],'YTick',[]); % CUM
% a_st = axes('Position',[1-st_w,0.000,st_w,1.000],'Box','on','XTick',[],'YTick',[]); % STATS
% 
% x=[0:.1:10];
% 
% axes(a_ts);
% plot(x,sin(x));
% ylabel('TS-YLABEL');
% title('TS-TITLE');
% 
% axes(a_pp);
% plot(x,cos(x),'r');
% ylabel('PP-YLABEL');
% 
% axes(a_so);
% plot(x,exp(x));
% xlabel('SO-XLABEL');
% ylabel('SO-YLABEL');
% title('SO-TITLE');
% 
% axes(a_cu);
% plot(x,x.^2);
% xlabel('CU-XLABEL');
% ylabel('CU-YLABEL');
% title('CU-TITLE');

% axes(a_st)
% text('text 1');

%--------------------------------------------------------
% clc;
% close all;
% data = [datenum([1999 12 28]) 5 nan; ...
%         datenum([1999 12 29]) 3 6; ...
%         datenum([1999 12 30]) 7 nan; ...
%         datenum([1999 12 31]) 4 nan; ...
%         datenum([2000  1  1]) 2 4; ...
%         datenum([2000  1  2]) 6 3; ...
%         datenum([2000  1  3]) 3 nan; ...
%         datenum([2000  1  4]) 4 6; ...
%         datenum([2000  1  5]) 7 7; ...
%         datenum([2000  1  6]) 1 8; ...
%         datenum([2000  1  7]) 1 nan; ...
%         datenum([2000  1  8]) 2 nan];
% 
% % remove NaN's in observed data and nearest-neighbor interp for remaining values...
% temp = data;
% temp(:,2) = [];
% temp(any(isnan(temp)'),:) = [];
% % time-weight interp to fill replace NaN's...
% data(:,3) = interp1(temp(:,1),temp(:,2),data(:,1),'nearest','extrap')
% figure;
% hold on;
% plot(data(:,1),data(:,3),'b');
% datetick('x',28);
% hold off;
% 
% % all_dates = str2num([datestr(data(:,1),'mm'),datestr(data(:,1),'yyyy')]) % make list of all month-year pairs.
% % make list of all month-year pairs.
% all_dates = datenum(str2num(datestr(data(:,1),'yyyy')),str2num(datestr(data(:,1),'mm')),ones(size(data,1),1))
% unique_dates = unique(all_dates) % make list of unique month-year pairs.
% 
% % find mean of sim & observed data values that match each unique month-year pair...
% monthly_avg = [];
% for i = 1:size(unique_dates,1),
%     match = (all_dates == unique_dates(i))
%     match.*data(:,2)
%     month_avg(i,1:3) = [unique_dates(i), sum(match.*data(:,2))/sum(match), sum(match.*data(:,3))/sum(match)]
% end
% disp(month_avg);
% figure;
% bar(month_avg(:,1),month_avg(:,2:3));
% datetick('x',28);

%---------------------------------
% temp = csvread('..\data\observed\example.oprec');
% prec_data = [datenum(temp(:,1),temp(:,2),temp(:,3),temp(:,4),temp(:,5),zeros(length(temp(:,1)),1)), temp(:,6)];
% h = bar(prec_data(:,1),prec_data(:,2));
% % get(h)
% set(gca,'YDir','reverse');
% set(h,'FaceColor',[0 .4 0]);
% set(h,'EdgeColor',[0 .4 0]);
% datetick('x',1,'keepticks','keeplimits');

% function rounddate = round_day(yr,mo,dy,hr)
% % This function rounds a date to the nearest day.
% % IE: 1999/5/15 09:00 --> 1999/5/15 0:00
% %     1999/5/15 13:00 --> 1999/5/16 0:00
% if hr >= 12 % need to round-up day, incrementing month & year if necessary.
%     if (mo==4)|(mo==6)|(mo==9)|(mo==11) % These months have 30 days.
%         if dy == 30 % increment month and reset day.
%             mo = mo + 1;
%             dy = 1;
%         else % increment day.
%             dy = dy + 1;
%         end
%     elseif (mo==1)|(mo==3)|(mo==5)|(mo==7)|(mo==8)|(mo==10)|(mo==12) % These months have 31 days.
%         if dy == 31 % increment month and reset day.
%             mo = mo + 1;
%             dy = 1;
%         else % increment day.
%             dy = dy + 1;
%         end
%     elseif mo == 2 % Feb - special case.
%         if leap(yr) % year is leap year.
%             if dy == 29 % increment month and reset day.
%                 mo = mo + 1;
%                 dy = 1;
%             else % increment day.
%                 dy = dy + 1;
%             end
%         else % not leap year.
%             if dy == 28 % increment month and reset day.
%                 mo = mo + 1;
%                 dy = 1;
%             else % increment day.
%                 dy = dy + 1;
%             end
%         end
%     else % error: month not valid.
%     end
% end
% rounddate = datenum(yr,mo,dy,12,0,0); % fix hour at 12 (average value is at midday).

%-----------------------------------------------------
% clear;
% 
% sim = [1, 1.1; ...
%        2, 2.1; ...
%        3, 3.1; ...
%        4, 4.1; ...
%        5, 5.1; ...
%        6, 6.1; ...
%        7, 7.1; ...
%        8, 8.1; ...
%        9, 9.1; ...
%       10,10.1];
% 
% % obs = [0, 0.9; ...
% %        2, 2.1; ...
% %        4, 4.1; ...
% %        5, 5.1; ...
% %        7, 7.1; ...
% %        8, 8.1];
% obs = [1, nan; ...
%        2, nan; ...
%        3, nan; ...
%        4, nan; ...
%        5, nan; ...
%        6, nan; ...
%        7, nan; ...
%        8, nan; ...
%        9, nan; ...
%       10, nan];
% 
% data = union(sim(:,1),obs(:,1));
% data(:,2) = NaN*ones(size(data,1),1);
% data(:,3) = NaN*ones(size(data,1),1);
% 
% [junk,data_ind,sim_ind] = intersect(data(:,1),sim(:,1));
% data(data_ind,2) = sim(:,2);
% [junk,data_ind,obs_ind] = intersect(data(:,1),obs(:,1));
% data(data_ind,3) = obs(:,2);
% 
% sim
% obs
% data