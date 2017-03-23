function basingen(codeselect,verbose)
% function "basingen.m"
% USAGE: basingen('codeselect',verbose)
%
% DESCRIPTION: This pre-processor script takes a character sting input
% argument designating a terminating reach segment, minor basin, or major
% basin and determines the subsequently defined sub-basin (ie: all upstream
% reaches and their associated land segments). Formatted output files are
% generated for use by the "run" scripts to simulate the sub-basin.
%
% INPUT:
%   - 'codeselect': character sting, can be one of the following 4:
%     (1) terminating reach segment ID   (13-character catcode: 'PM7_4820_0001')
%     (2) terminating minor basin (1st 2-characters of catcode: 'PM')
%     (3) terminating major basin    (1st character of catcode: 'P')
%     (4) 'all' - see NOTES below.
%     These cases are distinguished by the length of the input string.
%     Letter input is case insensitive, but it must be enclosed in single
%     quotes (') to distinguish it as a string.
%     
%   - verbose (optional): Logical value (true/1 or false/0) selecting mode of
%     run-time screen output. If true, progress and results will be written to
%     screen upon execution; if false, there will be no screen output (except
%     for fatal error messages). If omitted, defaults to true.
%
% OUTPUT: 3 files used by the HSPF run scripts to simulate the sub-basin
% found. They are named with the catcode,minor basin code, or major basin
% code (depending on 'codeselect') and the following extensions:
%   - .riv file: river reaches found for sub-basin, ready to run.
%   - .calib file: river reaches found for sub-basin that are also
%     calibration sites, ready to run.
%   - .land file: associated land segments found for sub-basin, ready to run.
% These files are placed in the "work/p50x/run/seglists" directory.
%
% NOTES: using the string 'all' for the argument 'codeselect' will generate
% output .riv, .calib, and .land files for the entire watershed.
%
% written by: Joe Vrabel, USGS Aug. 2003

% NOTES ON CODE:
%   - text names of rivers and land segments are read and stored, and can
%     be used for future outputs, is desired. Currently, they are just
%     outputted to screen as a run-time report.

%------------------------------------------------------------------------------------
% PRELIMINARY TASKS

% set path of input files (calibsites.csv, land_water_connect.csv,
% landnames.csv, rivernames.csv) and set as global variables.
global INPUT_PATH OUTPUT_PATH
INPUT_PATH = '../../catalog/connect/';
OUTPUT_PATH = '../../../run/seglists/';

% default "verbose" if omitted...
if nargin < 1, % not enough input arguments.
    disp('ERROR: not enough input arguments.');
    disp('(type "help basingen" for more information)');
    return;
elseif nargin == 1, % "verbose" omitted.
    verbose = true; % default is true - output report to screen.
elseif nargin > 2, % too many input arguments.
    disp('ERROR: too many input arguments.');
    disp('(type "help basingen" for more information)');
    return;
end

% convert input string to upper-case, if lower-case used....
codeselect = upper(codeselect);

% check for special case 'all': process the entire watershed.
if (length(codeselect) == 3  & codeselect == 'ALL')
    proc_all(verbose) % separate function at bottom of this page.
    return;
end

%------------------------------------------------------------------------------------
% FIND RIVER REACH NETWORK FOR TERMINATING REACH / MINOR BASIN / MAJOR BASIN SELECTED
% load river reach file... 
try
    [rivercodes,rivernames] = textread([INPUT_PATH,'rivernames.csv'],'%s %q','delimiter',',');
catch
    disp(['ERROR: cannot open input file ',INPUT_PATH,'rivernames.csv'])
    disp('STOPPING.')
    return
end
rivercodes = rivercodes(2:size(rivercodes)-1); % trim header and last ("end") lines.
rivernames = rivernames(2:size(rivernames));   % trim header and last ("end") lines.

% check for input reach, minor/major basin input existence, and compile list of reaches to trace upstream...
switch length(codeselect)
    case 13 % single river reach selected.
        % see if selected river exists.
        flag = 0;
        for i = 1:size(rivercodes),
            if rivercodes{i} == codeselect
                riverselect(1,:) = codeselect;
                nameselect{1} = rivernames{i};
                flag = 1; % the river exists.
                break;
            end
        end
        if flag == 0 % didn't find river catcode.
            disp(['ERROR: Cannot find river: ',codeselect]);
            disp('STOPPING.')
            return;
        else
            if verbose
                disp('------------------------------------------------------------------')
                disp(['Terminal river found: ',riverselect(1,:),' -- ',nameselect{1}])
            end
        end        
    case 2 % minor basin selected.
        % see if selected minor basin exists, and make list of all reaches in it.
        flag = 0;
        for i = 1:size(rivercodes,1),
            if rivercodes{i}(1:2) == codeselect
                flag = flag + 1; % the minor basin exists.
                riverselect(flag,:) = rivercodes{i};
                nameselect{flag} = rivernames{i};
            end
        end
        if flag == 0 % didn't find river catcode.
            disp(['ERROR: Cannot find minor basin: ',codeselect]);
            disp('STOPPING.')
            return;
        else
            if verbose
                disp('----------------------------------------------')
                disp(['Terminal minor basin found: ',codeselect])
                disp( 'River reaches in selected minor basin:')
                for i = 1:size(riverselect,1),
                    disp(['   ',int2str(i),': ',riverselect(i,:),' -- ',nameselect{i}]);
                end
            end
        end  
    case 1 % major basin selected.
        % see if selected major basin exists, and make list of all reaches in it.
        flag = 0;
        for i = 1:size(rivercodes,1),
            if rivercodes{i}(1:1) == codeselect
                flag = flag + 1; % the minor basin exists.
                riverselect(flag,:) = rivercodes{i};
                nameselect{flag} = rivernames{i};
            end
        end
        if flag == 0 % didn't find river catcode.
            disp(['ERROR: Cannot find major basin: ',codeselect]);
            disp('STOPPING.')
            return;
        else
            if verbose
                disp('----------------------------------------------')
                disp(['Terminal major basin found: ',codeselect])
                disp( 'River reaches in selected major basin:')
                for i = 1:size(riverselect,1),
                    disp(['   ',int2str(i),': ',riverselect(i,:),' -- ',nameselect{i}]);
                end
            end
        end 
    otherwise % error.
        disp(['ERROR: input "',codeselect,'" inappropriate - must be one of following:']);
        disp( '  (1) 13-character alphanumeric river catcode');
        disp( '  (2) 2-letter minor basin code');
        disp( '  (3) 1-letter major basin code');
        disp( '  (4) the string ''all''');
        disp( 'Type "help basingen" for more information.');
        disp( 'STOPPING.');
        return;
end

% find all upstream reaches associated with selected river(s)...
foundrivercodes(1,:) = 'xxxxxxxxxxxxx'; % start list with dummy (so appending method below works).
for i = 1:size(riverselect,1),
    foundrivercodes = [foundrivercodes;riverselect(i,:)]; % append current river code to list.
    targetsegs = riverselect(i,5:8); % need to find all rivers with this as its down-river segment.
    while 1 == 1, % infinite loop will terminate when all upstream reaches found.
        for j = 1:size(rivercodes,1),
            if rivercodes{j}(10:13) == targetsegs(1,:) % found an upstream river.
                foundrivercodes = [foundrivercodes; rivercodes{j}];   % append code to "found" list.
                targetsegs = [targetsegs; rivercodes{j}(5:8)];  % add to list to find its upstream rivers.
            end
        end
        if size(targetsegs,1) == 1 % target list to find upstream rivers is exhausted - done.
            break;
        else % still have some targets to trace upriver.
            targetsegs = targetsegs(2:size(targetsegs),:); % remove the target that was just searched.
        end
    end
end
foundrivercodes = foundrivercodes(2:size(foundrivercodes,1),:); % get rid of dummy.

% remove any duplicate reaches from network found above...
foundrivercodes = sortrows(foundrivercodes); % put list into alphabetical order.
i = 1;
while i < size(foundrivercodes,1),
    if foundrivercodes(i,:) == foundrivercodes(i+1,:) % a duplicate pair.
        foundrivercodes = [foundrivercodes(1:i,:); foundrivercodes(i+2:size(foundrivercodes,1),:)]; % omit duplicate (at i+1).
        % don't increment counter to check current one with next one in list (to eliminate triplets, quadruplets, etc.).
    else % not a duplicate pair - increment counter to check next pair.
        i = i + 1;
    end
end

% sort the reaches found into correct order (ie: upstream to downstream)...
flag = 0;
while flag < size(foundrivercodes,1)*(size(foundrivercodes,1)-1)/2, % this is satisfied when everything in correct order.
    flag = 0; % reset counter before complete pass.
    for i = 1:size(foundrivercodes,1),
        for j = i+1:size(foundrivercodes,1),
            if foundrivercodes(i,5:8) == foundrivercodes(j,10:13) % then out of order: upsteam reach after downstream.
                % swap the two that are out of order.
                temp = foundrivercodes(i,:);
                foundrivercodes(i,:) = foundrivercodes(j,:);
                foundrivercodes(j,:) = temp;
                flag = 0; % reset counter (don't need to do this, but just in case).
            else % they are in order - increment counter.
                flag = flag + 1; % when flag reaches maximum allowed by while condition, all reaches are in order.
            end
        end
    end
end

% find names of river reaches found and ordered...
flag = 0;
for i = 1:size(foundrivercodes,1),
    for j = 1:size(rivercodes),
        if rivercodes{j} == foundrivercodes(i,:) % then found the reach.
            flag = flag + 1;
            foundrivernames{flag} = rivernames{j}; % append name to "found" list.
            break;
        end
    end
end

% write ordered reach network to output file and report to screen...
fid = fopen([OUTPUT_PATH,codeselect,'.riv'],'w');
if fid == -1 % cannot open output file.
    disp(['ERROR: cannot open output file ',OUTPUT_PATH,codeselect,'.riv'])
    disp('STOPPING.')
    return
end
fprintf(fid,'%s','set segments = (');
if verbose
    disp('----------------------------------------------');
    disp('Sub-basin reach network determined:');
end
for i = 1:size(foundrivercodes,1),
    fprintf(fid,'%s',[' ',foundrivercodes(i,:)]);
    if verbose
       disp(['   ',int2str(i),': ',foundrivercodes(i,:),' -- ',foundrivernames{i}]);
    end
end
fprintf(fid,'%s\n',' )');
fprintf(fid,'%s\n',' ');
fclose(fid);
if verbose
    disp(['Results written to ',OUTPUT_PATH,codeselect,'.riv']);
end

%------------------------------------------------------------------------------------
% FIND ALL CALIBRATION SITES ASSOCIATED WITH THE REACH NETWORK FOUND
% load calibration site file...
try
    [calibcodes,calibnames] = textread([INPUT_PATH,'calibsites.csv'],'%s %q','delimiter',',');
catch
    disp(['ERROR: cannot open input file ',INPUT_PATH,'calibsites.csv'])
    disp('STOPPING.')
    return;
end
calibcodes = calibcodes(2:size(calibcodes)-1); % trim header and last ("end") lines.
calibnames = calibnames(2:size(calibnames)-1); % trim header and last ("end") lines.

% find sub-basin reaches that are calibration sites...
flag = 0;
for i = 1:size(foundrivercodes),
    for j = 1:size(calibcodes),
        if calibcodes{j} == foundrivercodes(i,:) % then found a matching calibration site.
            flag = flag + 1;
            foundcalibcodes(flag,:) = calibcodes{j}; % append code to "found" list.
            foundcalibnames{flag}   = calibnames{j}; % append name to "found" list.
            break;
        end
    end
end

% write calibration sites found to output file and report to screen...
fid = fopen([OUTPUT_PATH,codeselect,'.calib'],'w');
if fid == -1 % cannot open output file.
    disp(['ERROR: cannot open output file ',OUTPUT_PATH,codeselect,'.calib'])
    disp('STOPPING.')
    return;
end
fprintf(fid,'%s','set segments = (');
if verbose
    disp('----------------------------------------------');
    disp('Corresponding calibration sites:');
end
if flag == 0 % no corresponding calibration sites.
    if verbose
        disp('   ( no calibration sites in sub-basin )');
    end        
else
    for i = 1:size(foundcalibcodes,1),
        fprintf(fid,'%s',[' ',foundcalibcodes(i,:)]);
        if verbose
            disp(['   ',int2str(i),': ',foundcalibcodes(i,:),' -- ',foundcalibnames{i}]);
        end        
    end
end
fprintf(fid,'%s\n',' )');
fprintf(fid,'%s\n',' ');
fclose(fid);
if verbose
    disp(['Results written to ',OUTPUT_PATH,codeselect,'.calib']);
end

%------------------------------------------------------------------------------------
% FIND ALL LAND SEGMENTS ASSOCIATED WITH THE REACH NETWORK FOUND
% load land and land-river connect file...

% NOTE: following line is old method - requires separate, specially formatted file, "land_water_connect.txt".
% [riv_con_codes,land_con_codes] = textread('../lib/connect/land_water_connect.txt','%s %s','delimiter',',');

% following block is new method - directly handles format of "land_water_connect.csv" file.
%---< begin new method >----
riv_con_codes{1} = 'xxxxxxxxxxxxx'; % start arrays with dummies.
land_con_codes{1} = 'xxxxxx';
fid = fopen([INPUT_PATH,'land_water_connect.csv'],'r');
if fid == -1 % cannot open input file.
    disp(['ERROR: cannot open input file ',INPUT_PATH,'land_water_connect.csv'])
    disp('STOPPING.')
    return
end
text_line = fgetl(fid); % skip header line.
while 1 == 1, % read land_connect info and assign to arrays until "end" reached.
    text_line = fgetl(fid);
    if upper(text_line(1:3)) == 'END'
        break; % done reading file.
    end
    %example line: BS3_8350_8330, 4,A51027,A51167,A51185,A54047
    commas = findstr(text_line,','); % find positions of commas in string.
    catcode = text_line(1:commas(1)-1); % read catcode.
    text_line = [',',text_line(commas(2)+1:size(text_line,2)),',']; % remove catcode and number from string.
    commas = findstr(text_line,','); % find positions of commas in new string.
    for i = 1:size(commas,2)-1
        riv_con_codes = [riv_con_codes,{catcode}];
        land_con_codes = [land_con_codes,{text_line(commas(i)+1:commas(i+1)-1)}];
    end
end
fclose(fid);
riv_con_codes = riv_con_codes(2:end)'; % remove dummies.
land_con_codes = land_con_codes(2:end)';
%---< end new method >------

% load landnames file...
try
    [landcodes,landnames] = textread([INPUT_PATH,'landnames.csv'],'%s %q','delimiter',',');
catch
    disp(['ERROR: cannot open input file ',INPUT_PATH,'landnames.csv'])
    disp('STOPPING.')
    return
end
landcodes = landcodes(2:size(landcodes)-1); % trim header and last ("end") lines.
landnames = landnames(2:size(landnames)-1); % trim header and last ("end") lines.

% find land segments associated with the sub-basin reaches...
flag = 0;
for i = 1:size(foundrivercodes),
    for j = 1:size(riv_con_codes),
        if riv_con_codes{j} == foundrivercodes(i,:) % then found the reach.
            flag = flag + 1;
            foundlandcodes(flag,:) = land_con_codes{j}; % append code to "found" list.
        end
    end
end

% take out any duplicates in list found...
foundlandcodes = sortrows(foundlandcodes); % put list into alphabetical order.
i = 1;
while i < size(foundlandcodes,1),
    if foundlandcodes(i,:) == foundlandcodes(i+1,:) % a duplicate pair.
        foundlandcodes = [foundlandcodes(1:i,:); foundlandcodes(i+2:size(foundlandcodes,1),:)]; % omit duplicate (at i+1).
        % don't increment counter to check current one with next one in
        % list (to eliminate triplets, quadruplets, etc.).
    else % not a duplicate pair - increment counter to check next pair.
        i = i + 1;
    end
end

% For debugging - output sizes of all data read in (pairs should have equal size)...
% disp(['size rivercodes:     ',num2str(size(rivercodes))])
% disp(['size rivernames:     ',num2str(size(rivernames))])
% disp(' ')
% disp(['size calibcodes:     ',num2str(size(calibcodes))])
% disp(['size calibnames:     ',num2str(size(calibnames))])
% disp(' ')
% disp(['size landcodes:      ',num2str(size(landcodes))])
% disp(['size landnames:      ',num2str(size(landnames))])
% disp(' ')
% disp(['size riv_con_codes:  ',num2str(size(riv_con_codes))])
% disp(['size land_con_codes: ',num2str(size(land_con_codes))])

% find names of land segments found...
flag = 0;
for i = 1:size(foundlandcodes,1),
    for j = 1:size(landcodes),
        if landcodes{j} == foundlandcodes(i,:) % then found the reach.
            flag = flag + 1;
            foundlandnames{flag} = landnames{j}; % append name to "found" list.
            break;
        end
    end
end

% write associated land segments found to output file and report to screen...
fid = fopen([OUTPUT_PATH,codeselect,'.land'],'w');
if fid == -1 % cannot open output file.
    disp(['ERROR: cannot open output file ',OUTPUT_PATH,codeselect,'.land'])
    disp('STOPPING.')
    return
end
fprintf(fid,'%s','set segments = (');
if verbose
    disp('----------------------------------------------');
    disp('Associated land segments:');
end
for i = 1:size(foundlandcodes,1),
    fprintf(fid,'%s',[' ',foundlandcodes(i,:)]);
    if verbose
        disp(['   ',int2str(i),': ',foundlandcodes(i,:),' -- ',foundlandnames{i}]);
    end        
end
fprintf(fid,'%s\n',' )');
fprintf(fid,'%s\n',' ');
fclose(fid);
if verbose
    disp(['Results written to ',OUTPUT_PATH,codeselect,'.land']);
end

%------------------------------------------------------------------------------------
% DONE - CLOSING TASKS
if verbose
    disp('----------------------------------------------')
    disp('Basingen sucessfully completed!')
end
return;
%----------------------<< END OF FUNCTION BASINGEN >>--------------------------------

%************************************************************************************
%************************************************************************************
function proc_all(verbose)
% this function performs the special task of creating the formated
% .riv, .calib, and .land files for the entire watershed.

%------------------------------------------------------------------------------------
% PRELIMINARY TASKS

% recognize path of input files (calibsites.csv, land_water_connect.csv,
% landnames.csv, rivernames.csv) as global variables.
global INPUT_PATH OUTPUT_PATH

codeselect = 'ALL';
%------------------------------------------------------------------------------------
% CREATE .RIV FILE...
if verbose,
    disp('----------------------------------------------')
    disp('Loading and ordering all river reaches.')
    disp('... NOTE: ordering routine will take a while...')
end

try
    [rivercodes,rivernames] = textread([INPUT_PATH,'rivernames.csv'],'%s %q','delimiter',',');
catch
    disp(['ERROR: cannot open input file ',INPUT_PATH,'rivernames.csv'])
    disp('STOPPING.')
    return;
end
% convert from cell to string, and trim header and last ("end") lines.
for i = 2:size(rivercodes)-1,
    foundrivercodes(i-1,:) = rivercodes{i};
end

% remove any duplicate reaches from network...
foundrivercodes = sortrows(foundrivercodes); % put list into alphabetical order.
i = 1;
while i < size(foundrivercodes,1),
    if foundrivercodes(i,:) == foundrivercodes(i+1,:) % a duplicate pair.
        foundrivercodes = [foundrivercodes(1:i,:); foundrivercodes(i+2:size(foundrivercodes,1),:)]; % omit duplicate (at i+1).
        % don't increment counter to check current one with next one in list (to eliminate triplets, quadruplets, etc.).
    else % not a duplicate pair - increment counter to check next pair.
        i = i + 1;
    end
end

%***************** for bug checking *********************
%********* shorten list to lessen run-time **************
% foundrivercodes = foundrivercodes(1:50,:);
%********************************************************

% sort the reaches found into correct order (ie: upstream to downstream)...
flag = 0;
while flag < size(foundrivercodes,1)*(size(foundrivercodes,1)-1)/2, % this is satisfied when everything in correct order.
    flag = 0; % reset counter before complete pass.
    for i = 1:size(foundrivercodes,1),
        for j = i+1:size(foundrivercodes,1),
            if foundrivercodes(i,5:8) == foundrivercodes(j,10:13) % then out of order: upsteam reach after downstream.
                % swap the two that are out of order.
                temp = foundrivercodes(i,:);
                foundrivercodes(i,:) = foundrivercodes(j,:);
                foundrivercodes(j,:) = temp;
                flag = 0; % reset counter (don't need to do this, but just in case).
            else % they are in order - increment counter.
                flag = flag + 1; % when flag reaches maximum allowed by while condition, all reaches are in order.
            end
        end
    end
end

% write ordered reach network to output file and report to screen...
fid = fopen([OUTPUT_PATH,codeselect,'.riv'],'w');
if fid == -1 % cannot open output file.
    disp(['ERROR: cannot open output file ',OUTPUT_PATH,codeselect,'.riv'])
    disp('STOPPING.')
    return;
end
fprintf(fid,'%s','set segments = (');
if verbose
    disp('Watershed rivers found and ordered.')
end
for i = 1:size(foundrivercodes,1),
    fprintf(fid,'%s',[' ',foundrivercodes(i,:)]);
end
fprintf(fid,'%s\n',' )');
fprintf(fid,'%s\n',' ');
fclose(fid);
if verbose
    disp(['Results written to ',OUTPUT_PATH,codeselect,'.riv']);
end

%------------------------------------------------------------------------------------
% CREATE .CALIB FILE...
% load calibration site file...
try
    [calibcodes,calibnames] = textread([INPUT_PATH,'calibsites.csv'],'%s %q','delimiter',',');
catch
    disp(['ERROR: cannot open input file ',INPUT_PATH,'calibsites.csv'])
    disp('STOPPING.')
    return
end
% convert from cell to string, and trim header and last ("end") lines.
for i = 2:size(calibcodes)-1,
    foundcalibcodes(i-1,:) = calibcodes{i};
end

% write calibration sites output file and report to screen...
fid = fopen([OUTPUT_PATH,codeselect,'.calib'],'w');
if fid == -1 % cannot open output file.
    disp(['ERROR: cannot open output file ',OUTPUT_PATH,codeselect,'.calib'])
    disp('STOPPING.')
    return
end
fprintf(fid,'%s','set segments = (');
for i = 1:size(foundcalibcodes,1),
    fprintf(fid,'%s',[' ',foundcalibcodes(i,:)]);
end
fprintf(fid,'%s\n',' )');
fprintf(fid,'%s\n',' ');
fclose(fid);
if verbose
    disp('----------------------------------------------')
    disp('Calibration sites in watershed found.')
    disp(['Results written to ',OUTPUT_PATH,codeselect,'.calib']);
end

%------------------------------------------------------------------------------------
% CREATE .LAND FILE...
% load calibration site file...
try
    [landcodes,landnames] = textread([INPUT_PATH,'landnames.csv'],'%s %q','delimiter',',');
catch
    disp(['ERROR: cannot open input file ',INPUT_PATH,'landnames.csv'])
    disp('STOPPING.')
    return
end
% convert from cell to string, and trim header and last ("end") lines.
for i = 2:size(landcodes)-1,
    foundlandcodes(i-1,:) = landcodes{i};
end

% write calibration sites to output file and report to screen...
fid = fopen([OUTPUT_PATH,codeselect,'.land'],'w');
if fid == -1 % cannot open output file.
    disp(['ERROR: cannot open output file ',OUTPUT_PATH,codeselect,'.land'])
    disp('STOPPING.')
    return
end
fprintf(fid,'%s','set segments = (');
for i = 1:size(foundlandcodes,1),
    fprintf(fid,'%s',[' ',foundlandcodes(i,:)]);
end
fprintf(fid,'%s\n',' )');
fprintf(fid,'%s\n',' ');
fclose(fid);
if verbose
    disp('----------------------------------------------')
    disp('Land segments in watershed found.')
    disp(['Results written to ',OUTPUT_PATH,codeselect,'.land']);
end

%------------------------------------------------------------------------------------
% DONE - CLOSING TASKS
if verbose
    disp('----------------------------------------------')
    disp('Basingen sucessfully completed!')
end
return;

%----------------------<< END OF FUNCTION PROC_ALL >>--------------------------------