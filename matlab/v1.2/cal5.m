function varargout = cal5(varargin)
% CAL5: A calibration tool developed for graphically and statistically
% assessing the performance of the Chesapeake Bay Program's Phase 5 HSPF
% model of the Potomac Watershed.
%
% USAGE: Type 'cal5' at the command line to launch.
%
% Written by: Joe Vrabel, U.S. Geological Survey, Sept. 2003

% CAL5 M-file for cal5.fig
%      CAL5, by itself, creates a new CAL5 or raises the existing
%      singleton*.
%
%      H = CAL5 returns the handle to a new CAL5 or the handle to
%      the existing singleton*.
%
%      CAL5('Property','Value',...) creates a new CAL5 using the
%      given property value pairs. Unrecognized properties are passed via
%      varargin to cal5_OpeningFcn.  This calling syntax produces a
%      warning when there is an existing singleton*.
%
%      CAL5('CALLBACK') and CAL5('CALLBACK',hObject,...) call the
%      local function named CALLBACK in CAL5.M with the given input
%      arguments.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES
% Edit the above text to modify the response to help cal5
% Last Modified by GUIDE v2.5 22-Oct-2003 15:32:03

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @cal5_OpeningFcn, ...
                   'gui_OutputFcn',  @cal5_OutputFcn, ...
                   'gui_LayoutFcn',  [], ...
                   'gui_Callback',   []);
if nargin & isstr(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before cal5 is made visible.
function cal5_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   unrecognized PropertyName/PropertyValue pairs from the
%            command line (see VARARGIN)
% Choose default command line output for cal5

handles.output = hObject;

% set defaults/parameters...
handles.river_sim_path = '..\..\..\output\river\daily\'; % search path for river simulated data.
handles.land_sim_path = '..\..\..\output\pltgen\land\'; % search path for land simulated data.
handles.obs_path = '..\..\data\observed\'; % search path for observed data.
handles.connect_path = '..\..\catalog\connect\'; % search path for connect files.
handles.default_mindate = '10/1/1982';
handles.default_maxdate = '9/30/2002';

% load user preferences from "cal5.prefs" file and set in GUI...
try
    load -mat cal5.prefs;
    handles.catcode = prefs.catcode;
    handles.scenario = prefs.scenario;
    handles.data_type = prefs.data_type;
    data_type_menu_select = prefs.data_type_menu_select;
    handles.mindate = prefs.mindate;
    handles.maxdate = prefs.maxdate;
    handles.hide_precip = prefs.hide_precip; % time-series plot parameters.
    handles.hide_obs_ts = prefs.hide_obs_ts;
    handles.logscale_ts = prefs.logscale_ts;
    handles.log_simobs = prefs.log_simobs; % sim vs. obs plot parameters.
    handles.log_cum = prefs.log_cum;  % cumulative frequency plot parameters.
    handles.hide_obs_cum = prefs.hide_obs_cum;
    handles.plot_data = prefs.plot_data; % serial_date, sim, log_sim, obs, log_obs, prec
    handles.have_sim = prefs.have_sim;
    handles.have_obs = prefs.have_obs;
    handles.have_prec = prefs.have_prec;
    handles.filter = prefs.filter;
    handles.river_land = prefs.river_land; % flag: 0 = river, 1 = land.
    clear prefs;
catch % if load fails, set defaults.
    handles.catcode = 'example';
    handles.scenario = 'expl';
    handles.data_type = 'flow';
    data_type_menu_select = 1;
    handles.mindate = handles.default_mindate;
    handles.maxdate = handles.default_maxdate;
    handles.hide_precip = 0; % time-series plot parameters.
    handles.hide_obs_ts = 0;
    handles.logscale_ts = 0;
    handles.log_simobs = 0; % sim vs. obs plot parameters.
    handles.log_cum = 0; % cumulative frequency plot parameters.
    handles.hide_obs_cum = 0;
    handles.plot_data = [NaN,NaN,NaN,NaN,NaN,NaN]; % serial_date, sim, log_sim, obs, log_obs, prec
    handles.have_sim = 0;
    handles.have_obs = 0;
    handles.have_prec = 0;
    handles.filter = 0;
    handles.river_land = 0; % flag: 0 = river, 1 = land.
end
set(findobj('Tag','catcode_edit'), 'String', handles.catcode);
set(findobj('Tag','scenario_edit'), 'String', handles.scenario);
set(findobj('Tag','data_type_popupmenu'), 'Value', data_type_menu_select);
set(findobj('Tag','mindate_edit'), 'String', handles.mindate);
set(findobj('Tag','maxdate_edit'), 'String', handles.maxdate);
set(findobj('Tag','timeseries_precip_checkbox'), 'Value', handles.hide_precip);
set(findobj('Tag','timeseries_obs_checkbox'), 'Value', handles.hide_obs_ts);
set(findobj('Tag','log_scale_checkbox'), 'Value', handles.logscale_ts);
set(findobj('Tag','log_simobs_checkbox'), 'Value', handles.log_simobs);
set(findobj('Tag','plot_log_data_checkbox'), 'Value', handles.log_cum);
set(findobj('Tag','cumprob_obs_checkbox'), 'Value', handles.hide_obs_cum);

% read calibration catcodes (for checking if entered catcode is a calibration site before getting data in get_data)...
try
    [handles.calibcodes,calibnames] = textread([handles.connect_path,'calibsites.csv'],'%s %q','delimiter',',');
catch
    handles.calibcodes = {'error'};
end

% read reach and land segment names...
try
    [handles.river_names(:,1),handles.river_names(:,2)] = textread([handles.connect_path,'rivernames.csv'],'%s %q','delimiter',',');
catch
    handles.river_names(:,1) = {'error'};
    handles.river_names(:,2) = {'error'};
end
handles.river_names = handles.river_names(2:end-1,:); % trim 1st and last lines
handles.river_names = [handles.river_names; {'example','example'}]; % add 'example'.
try
    [handles.land_names(:,1),handles.land_names(:,2)] = textread([handles.connect_path,'landnames.csv'],'%s %q','delimiter',',');
catch
    handles.land_names(:,1) = {'error'};
    handles.land_names(:,2) = {'error'};
end
handles.land_names = handles.land_names(2:end-1,:); % trim 1st and last lines
handles.land_names = [handles.land_names; {'example','example'}]; % add 'example'.

guidata(hObject, handles); % save/update handles structure.

% create plots and compute stats on start-up...
update_plots_pushbutton_Callback(findobj('Tag','update_plots_pushbutton'), [], handles);


% --- Outputs from this function are returned to the command line.
function varargout = cal5_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


%------------------------------------------------------------------------------------------------------
%================ CURRENT SCENARIO SPECIFICATION BOX ================
% --- Executes during object creation, after setting all properties.
function scenario_edit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to scenario_edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end
function scenario_edit_Callback(hObject, eventdata, handles)
% hObject    handle to scenario_edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hints: get(hObject,'String') returns contents of scenario_edit as text
%        str2double(get(hObject,'String')) returns contents of scenario_edit as a double
handles.scenario = get(hObject,'String'); % get and store scenario (directory name of scenario to process).
guidata(hObject,handles);


%================ CURRENT SEGEMENT SPECIFICATION BOX ===============
% --- Executes during object creation, after setting all properties.
function catcode_edit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to catcode_edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end
function catcode_edit_Callback(hObject, eventdata, handles)
% hObject    handle to catcode_edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hints: get(hObject,'String') returns contents of catcode_edit as text
%        str2double(get(hObject,'String')) returns contents of catcode_edit as a double
handles.catcode = get(hObject,'String'); % get and store catcode (root of filename to process).
guidata(hObject,handles);

%=================== DATA TYPE SPECIFICATION BOX ====================
% --- Executes during object creation, after setting all properties.
function data_type_popupmenu_CreateFcn(hObject, eventdata, handles)
% hObject    handle to data_type_popupmenu (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end
% --- Executes on selection change in data_type_popupmenu.
function data_type_popupmenu_Callback(hObject, eventdata, handles)
% hObject    handle to data_type_popupmenu (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hints: contents = get(hObject,'String') returns data_type_popupmenu contents as cell array
%        contents{get(hObject,'Value')} returns selected item from data_type_popupmenu
menu = get(hObject,'String');
data_type = menu{get(hObject,'Value')};
handles.data_type = data_type(1: findstr(data_type,' ')-1 ); % re-assign long data type name in menu to abbreviation.
if handles.data_type == 'SNOW' % land segment.
    handles.river_land = 1;
else
    handles.river_land = 0;
end
guidata(hObject,handles);


%========================= MIN DATE SPECIFICATION BOX =======================
% --- Executes during object creation, after setting all properties.
function mindate_edit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to mindate_edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end
function mindate_edit_Callback(hObject, eventdata, handles)
% hObject    handle to mindate_edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hints: get(hObject,'String') returns contents of mindate_edit as text
%        str2double(get(hObject,'String')) returns contents of mindate_edit as a double
temp = get(hObject,'String'); % get and store the minimum plot date (mm/dd/yyyy).
err = check_date(temp); % test for valid date.
if err ~= 0
    set(hObject, 'String', handles.mindate); % error - reset text window to previous value.
else
    handles.mindate = temp; % date OK - store.
end
guidata(hObject,handles);


%========================= MAX DATE SPECIFICATION BOX =======================
% --- Executes during object creation, after setting all properties.
function maxdate_edit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to maxdate_edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end
function maxdate_edit_Callback(hObject, eventdata, handles)
% hObject    handle to maxdate_edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hints: get(hObject,'String') returns contents of maxdate_edit as text
%        str2double(get(hObject,'String')) returns contents of maxdate_edit as a double
temp = get(hObject,'String'); % get and store the maximum plot date (mm/dd/yyyy).
err = check_date(temp); % test for valid date.
if err ~= 0
    set(hObject, 'String', handles.maxdate); % error - reset text window to previous value.
else
    handles.maxdate = temp; % date OK - store.
end
guidata(hObject,handles);


%============ CHECK INPUT DATES ===============
function err = check_date(date_str)
% This checks that a date is in "mm/dd/yyyy" format:
%  * must have 2 slashes, and mm, dd, yyyy must all be numbers.
%  * Day must be between 1 and 31, inclusive.
%  * Month must be between 1 and 12, inclusive.
%  * Year must be between 1950 and 2050, inclusive.
err = 0;
slashes = findstr(date_str,'/');
if  size(slashes,2) ~= 2 % doesn't have 2 "/"s.
    err = 1;
else % check numbers.
    mo = str2num(date_str(1:slashes(1)-1));
    dy = str2num(date_str(slashes(1)+1:slashes(2)-1));
    yr = str2num(date_str(slashes(2)+1:length(date_str)));    
    if sum(size(mo)) == 0 % "month" is not number.
        err = 1;
    else % "month" is number, check for validity.
        if (mo < 1) | (mo > 12) % invalid.
            err = 1;
        end
    end
    if sum(size(dy)) == 0 % "day" is not number.
        err = 1;
    else
        if (dy < 1) | (dy > 31) % invalid.
            err = 1;
        end
    end
    if sum(size(yr)) == 0 % "year" is not number.
        err = 1;
    else % "year" is number, check for validity.
        if ((yr < 1950) | (yr > 2050)) & (yr ~= 0) % invalid.
            err = 1;
        end
    end
end
if err ~= 0
    uiwait(errordlg(['Date must be in mm/dd/yyyy format, using slashes and valid interger values. ', ...
                     'Note: Set year to 0 to filter out all data not in specified month interval for all available data.'], ...
                     'Error: Invalid Date Selection','modal'));
end


%------------------------------------------------------------------------------------------------------
%===================== UPDATE PLOTS PUSHBUTTON ==========================
% ALL DATA ARE READ WITH THIS BUTTON PUSH.
% ALL PLOTS ARE GENERATED WITH THIS BUTTON PUSH.
% ALL STATS ARE COMPUTED WITH THIS BUTTON PUSH.
% --- Executes on button press in update_plots_pushbutton.
function update_plots_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to update_plots_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% clear plots and stats...
clear_plots(handles); % clear all plot windows.
clear_stats(handles); % clear all stat windows.

% check that mindate < maxdate and handle seasonal filter case...
min_slashes = findstr(handles.mindate,'/');
max_slashes = findstr(handles.maxdate,'/');
if (str2num(handles.mindate(min_slashes(end)+1:end)) == 0)&(str2num(handles.maxdate(max_slashes(end)+1:end)) == 0), % seasonal filter specified.
    handles.filter = 1; % set filter flag.
elseif (datenum(handles.mindate) >= datenum(handles.maxdate)) % no seasonal filter case.
    handles.filter = 0; % set filter flag.
    uiwait(errordlg(['"max date" must be greater than the "min date". ', ...
                     'Note: Set year to 0 to filter out all data not in specified month interval for all available data.'], ...
                     'Error: Invalid Date Selection','modal'));
    % reset date values to defaults and abort...
    set(handles.mindate_edit, 'String', handles.default_mindate);
    set(handles.maxdate_edit, 'String', handles.default_maxdate);
    handles.mindate = handles.default_mindate;
    handles.maxdate = handles.default_maxdate;
    guidata(hObject,handles);
    return;
end

guidata(hObject,handles);

% read data according to specifications...
[plot_data, have_sim, have_obs, have_prec, err] = get_data(handles);
if err ~= 0
    handles.plot_data = [NaN,NaN,NaN,NaN,NaN,NaN];
    guidata(hObject,handles);
    return;
end
% store and save data set in GUI data structure...
handles.plot_data = plot_data;
handles.have_sim  = have_sim;
handles.have_obs  = have_obs;
handles.have_prec = have_prec;
guidata(hObject,handles);

% create time-series plot...
plot_ts(handles,handles.timeseries_axes,handles.precip_axes);

% create sim vs. obs plot...
plot_sim_obs(handles,handles.simobs_axes,handles.log_simobs_axes);

% create cumulative dist. plot...
plot_cum(handles,handles.cumprob_axes);

% compute stats and update GUI stats text-boxes...
if have_sim & have_obs, % perform all stats on both data sets.
    calc_stats(handles);
elseif have_sim, % perform stats on sim only.
    calc_sim_stats(handles);
elseif have_obs, % perform stats on obs only.
    calc_obs_stats(handles);
end

% save user variables in "cal5.prefs" file for future cal5 session start-up...
% NOTE: prefs saved only if all of above executed successfully.
prefs.catcode = handles.catcode;
prefs.scenario = handles.scenario;
prefs.data_type = handles.data_type;
prefs.data_type_menu_select = get(findobj('Tag','data_type_popupmenu'),'Value');
prefs.mindate = handles.mindate;
prefs.maxdate = handles.maxdate;
prefs.hide_precip = handles.hide_precip;
prefs.hide_obs_ts = handles.hide_obs_ts;
prefs.logscale_ts = handles.logscale_ts;
prefs.log_simobs = handles.log_simobs;
prefs.log_cum = handles.log_cum;
prefs.hide_obs_cum = handles.hide_obs_cum;
prefs.plot_data = handles.plot_data;
prefs.have_sim = handles.have_sim;
prefs.have_obs = handles.have_obs;
prefs.have_prec = handles.have_prec;
prefs.filter = handles.filter;
prefs.river_land = handles.river_land; % flag: 0 = river, 1 = land.
save cal5.prefs prefs -mat;
clear prefs;


%------------------------------------------------------------------------------------------------------
%============= CLEAR PLOT WINDOWS ==================
function clear_plots(handles)
% this function clears all plot windows.
axes(handles.timeseries_axes); cla;
axes(handles.precip_axes);     cla;
axes(handles.simobs_axes);     cla;
axes(handles.log_simobs_axes); cla;
axes(handles.cumprob_axes);    cla;


%============ CLEAR STAT TEXT BOXES ================
function clear_stats(handles)
% this function clears the stats text windows of the GUI,
% and unchecks the JB test check-boxes.

% raw data stats...
set(handles.n_simobs_points_text, 'String', ' ');
set(handles.min_obs_text, 'String', ' ');
set(handles.min_sim_text, 'String', ' ');
set(handles.obs_mean_text, 'String', ' ');
set(handles.sim_mean_text, 'String', ' ');
set(handles.obs_median_text, 'String', ' ');
set(handles.sim_median_text, 'String', ' ');
set(handles.max_obs_text, 'String', ' ');
set(handles.max_sim_text, 'String', ' ');
set(handles.obs_variance_text, 'String', ' ');
set(handles.sim_variance_text, 'String', ' ');
set(handles.error_variance_value_text, 'String', ' ');
set(handles.relative_bias_value_text, 'String', ' ');
set(handles.rel_std_error_value_text, 'String', ' ');
set(handles.model_eff_value_text, 'String', ' ');
set(handles.sim_JB_pvalue_text, 'String', ' ');
set(handles.obs_JB_pvalue_text, 'String', ' ');

% log10 data stats...
set(handles.n_log_simobs_points_text, 'String', ' ');
set(handles.log_min_obs_text, 'String', ' ');
set(handles.log_min_sim_text, 'String', ' ');
set(handles.log_obs_mean_text, 'String', ' ');
set(handles.log_sim_mean_text, 'String', ' ');
set(handles.log_obs_median_text, 'String', ' ');
set(handles.log_sim_median_text, 'String', ' ');
set(handles.log_max_obs_text, 'String', ' ');
set(handles.log_max_sim_text, 'String', ' ');
set(handles.log_obs_variance_text, 'String', ' ');
set(handles.log_sim_variance_text, 'String', ' ');
set(handles.log_error_variance_value_text, 'String', ' ');
set(handles.log_relative_bias_value_text, 'String', ' ');
set(handles.log_rel_std_error_value_text, 'String', ' ');
set(handles.log_model_eff_value_text, 'String', ' ');
set(handles.log_sim_JB_pvalue_text, 'String', ' ');
set(handles.log_obs_JB_pvalue_text, 'String', ' ');

% clear JB-test checkboxes...
set(findobj('Tag',    'sim_JB_pass_checkbox'),'Value',0)
set(findobj('Tag','log_sim_JB_pass_checkbox'),'Value',0)
set(findobj('Tag',    'obs_JB_pass_checkbox'),'Value',0)
set(findobj('Tag','log_obs_JB_pass_checkbox'),'Value',0)


%------------------------------------------------------------------------------------------------------
%=========== GET SPECIFIED DATA ===================
function [plot_data, have_sim, have_obs, have_prec, err] = get_data(handles)
% This checks if the specified data files exist, and reads in the values.
% Observed, Simulated, and precip data are read. The data is then combined
% and trimmed to the time period specified.

% KNOWN BUG: IF CONTINUOUS SIM DATA IS MISSING AND ONLY POINT OBSERVED
% DATA LOADED, PRECIP DATA ON DATES OTHER THAN THE OBSERVED DATES
% WILL BE LOST AND NOT DISPLAYED.

% check if specified catcode is calibration site (for FLOW only)...
if upper(handles.data_type) == 'FLOW',
    if ( size(intersect( upper([handles.calibcodes;cellstr('example')]), upper(handles.catcode) ), 2 ) == 0 ) & ...
       ( not(strcmp(handles.calibcodes{1},'error')) ), % specified catcode not a calibration site.
        button_select = questdlg('Specified data file is not a flow calibration site.', ...
                            'Non-Calibration Site Selected', ...
                            'Continue','Cancel','Cancel');
        switch button_select,
            case 'Continue', 
                % do nothing.
            case 'Cancel',
                plot_data = [NaN,NaN,NaN,NaN,NaN,NaN];
                err = 1;
                return; % abort.
        end
    end
end

% read data...
err = 0;
have_sim  = 0;
have_obs  = 0;
have_prec = 0;
if handles.river_land == 1 % land segment.
    simfile  = [handles.land_sim_path,handles.scenario,'\grs_',handles.catcode,'.',handles.data_type]; % use grass (grs) land use as representative snow depth for entire segment.
else
    simfile  = [handles.river_sim_path,handles.scenario,'\',handles.catcode,'.', handles.data_type];
end
obsfile  = [handles.obs_path,handles.data_type,'\',handles.catcode,'.o',handles.data_type];
precfile = [handles.obs_path,'prec\',handles.catcode,'.oprec'];

h_wait = waitbar(0,'reading simulated data set...','Name','Loading Data'); % launch waitbar.
if exist(simfile) == 2 % sim data file exists.
    have_sim = 1;
    temp = csvread(simfile);
    % all data values are applied to mid-day of given date (noon).
    sim_data = [datenum(temp(:,1),temp(:,2),temp(:,3), ...
                12*ones(length(temp(:,1)),1),zeros(length(temp(:,1)),1),zeros(length(temp(:,1)),1)), ...
                temp(:,6)];
end

waitbar(0.3,h_wait,'reading observed data set...');
if exist(obsfile) == 2 % obs data file exists.
    have_obs = 1;
    temp = csvread(obsfile);
    % all data values are applied to mid-day of given date (noon).
    obs_data = [datenum(temp(:,1),temp(:,2),temp(:,3), ...
                12*ones(length(temp(:,1)),1),zeros(length(temp(:,1)),1),zeros(length(temp(:,1)),1)), ...
                temp(:,6)];
end

waitbar(0.6,h_wait,'reading precipitation data set...');
if exist(precfile) == 2 % precip data file exists.
    have_prec = 1;
    temp = csvread(precfile);
    % all data values are applied to mid-day of given date (noon).
    prec_data = [datenum(temp(:,1),temp(:,2),temp(:,3), ...
                12*ones(length(temp(:,1)),1),zeros(length(temp(:,1)),1),zeros(length(temp(:,1)),1)), ...
                temp(:,6)];
end

if (have_sim == 1) & (have_obs == 0) % don't have obs - dummy with NaN and continue.
    obs_data = [sim_data(:,1),NaN*ones(size(sim_data,1),1)];
    uiwait(errordlg(['Cannot open file: "',obsfile,'". Simulated data loaded only.' ],'File Error','modal'));
elseif (have_sim == 0) & (have_obs == 1) % don't have sim - dummy with NaN and continue.
    sim_data = [obs_data(:,1),NaN*ones(size(obs_data,1),1)];
    uiwait(errordlg(['Cannot open file: "',simfile,'". Observed data loaded only.' ],'File Error','modal'));
elseif (have_sim == 0) & (have_obs == 0) % have neither - abort.
    err = 1;
    plot_data = [NaN,NaN,NaN,NaN,NaN,NaN];
    uiwait(errordlg(['Cannot open files: "',obsfile,'", "',simfile,'". No plots or statistics generated.'],'File Error','modal'));
    close(h_wait); % close waitbar.
    return;
end
if have_prec == 0 % don't have precip data - dummy-up with NaN's.
    if size(obs_data,1) > size(sim_data,1)
        prec_data = [obs_data(:,1),NaN*ones(size(obs_data,1),1)];
    else
        prec_data = [sim_data(:,1),NaN*ones(size(sim_data,1),1)];
    end
    uiwait(errordlg(['Cannot open file: "',precfile,'". Precipitation data not available for viewing.' ],'File Error','modal'));
end

% merge data into single array, with columns: serial_time, sim_data, obs_data, precip_data.
% missing values are assigned NaN.
all_data = union(sim_data(:,1),obs_data(:,1)); % merge serial times.
all_data(:,2) = NaN*ones(size(all_data,1),1); % initialize sim
all_data(:,3) = NaN*ones(size(all_data,1),1); % initialize log_sim
all_data(:,4) = NaN*ones(size(all_data,1),1); % initialize obs
all_data(:,5) = NaN*ones(size(all_data,1),1); % initialize log_obs
all_data(:,6) = NaN*ones(size(all_data,1),1); % initialize precip

% find sim data that matches merged time set and put in array.
[junk,data_ind,sim_ind] = intersect(all_data(:,1),sim_data(:,1));
all_data(data_ind,2) = sim_data(sim_ind,2);
temp = all_data(:,2);
temp(temp==0) = NaN; % remove any 0's before logging.
all_data(:,3) = log10(temp);

% find obs data that matches merged time set and put in array.
[junk,data_ind,obs_ind] = intersect(all_data(:,1),obs_data(:,1));
all_data(data_ind,4) = obs_data(obs_ind,2);
temp = all_data(:,4);
temp(temp==0) = NaN; % remove any 0's before logging.
all_data(:,5) = log10(temp);

[junk,data_ind,prec_ind] = intersect(all_data(:,1),prec_data(:,1));
all_data(data_ind,6) = prec_data(prec_ind,2);

% trim data to specified time interval.
if handles.filter == 0, % no seasonal filter.
    all_data(all_data(:,1)<datenum(handles.mindate),:) = [];
    all_data(all_data(:,1)>datenum(handles.maxdate)+1,:) = []; % +1 required to get "missing day".
    plot_data = all_data;
else % seasonal filter specified - eliminate all data not in day-month window (from entire data set).
    min_slashes   = findstr(handles.mindate,'/');
    min_test_date = datenum([handles.mindate(1:min_slashes(2)),'2000']); % 2000 is arbitrary - year is irrelavent for checking seasonal window.

    max_slashes   = findstr(handles.maxdate,'/');
    max_test_date = datenum([handles.maxdate(1:max_slashes(2)),'2000']); % 2000 is arbitrary - year is irrelavent for checking seasonal window.

    plot_data = [];
    for i = 1:size(all_data,1),
        curr_date = datestr(all_data(i,1),23);
        slashes   = findstr(curr_date,'/');
        test_date = datenum([curr_date(1:slashes(2)),'2000']);
%         disp([num2str(i),': ',datestr(test_date,23)]); %?
        if (min_test_date < max_test_date) % doesn't span across year.
            if (test_date >= min_test_date)&(test_date <= max_test_date), % in specified window - add to final data set.
                plot_data = [plot_data; all_data(i,:)];
%                 disp([' IN: ',datestr(all_data(i,1),23)]); %?
%                 pause; %?
%             else
%                 disp(['OUT: ',datestr(all_data(i,1),23)]); %?
%                 pause; %?
            end
        elseif (min_test_date >= max_test_date) % spans across a year, like 12/1 -> 1/1.
            if (test_date < min_test_date)&(test_date > max_test_date) % not in specified window - do not add to final data set.
                % (do nothing)
%                 disp(['OUT: ',datestr(all_data(i,1),23)]); %?
%                 pause; %?
            else % is in specified window - add to final data set.
                plot_data = [plot_data; all_data(i,:)];
%                 disp([' IN: ',datestr(all_data(i,1),23)]); %?
%                 pause; %?
            end
        end
    end
end
% disp(datestr(plot_data(:,1),23)) %?

plot_data(isinf(plot_data)) = NaN; % remove any Inf's.
waitbar(1.0,h_wait,'generating plots and computing statistics...');
close(h_wait); % close waitbar.


%------------------------------------------------------------------------------------------------------
%============ PLOT TIME-SERIES ==================
function plot_ts(handles,axes1,axes2)
% This function plots the time-series data.
% All plots are first generated, then surpressed according to the chosen
% check-box options.

% plot obs/sim data sets...
axes(axes1); % specify axes time-series axes.
hold on;
if handles.have_sim
    h_sim = plot(handles.plot_data(:,1),handles.plot_data(:,2),'r'); % simulated.
end
if handles.have_obs
    switch upper(handles.data_type)
        case {'FLOW','BFLW','QFLW'} % use continuous line for observed flow.
            h_obs = plot(handles.plot_data(:,1),handles.plot_data(:,4),'Color',[0 0.5 0]); % observed.
        otherwise % use points for other discrete data.
            h_obs = plot(handles.plot_data(:,1),handles.plot_data(:,4), ...
                    'Marker','d', 'MarkerSize',5, 'MarkerEdgeColor',[0 0.5 0], 'MarkerFaceColor','none', ...
                    'LineStyle','none', 'Color',[0 0.5 0]); % observed.            
    end
end

% set various attributes on sim/obs plot...
if handles.river_land == 0 % river segment.
    [c ia ib] = intersect(upper(handles.catcode), upper(handles.river_names(:,1)));
    title_catcode = char(handles.river_names(ib,2));
    title_catcode(findstr(title_catcode,'_')) = '-';
    title(['\bf',upper(title_catcode),': ',upper(handles.data_type),' TIME-SERIES']);
else % land segment.
    [c ia ib] = intersect(upper(handles.catcode), upper(handles.land_names(:,1)));
    title_catcode = char(handles.land_names(ib,2));
    title(['\bf',upper(title_catcode),': ',upper(handles.data_type),' TIME-SERIES']);
end

ylabel(get_label(handles));
axis([min(handles.plot_data(:,1)), max(handles.plot_data(:,1)), -inf, inf]);
set(gca,'XTick',[min(handles.plot_data(:,1)): ...
                (max(handles.plot_data(:,1))-min(handles.plot_data(:,1)))/6: ...
                 max(handles.plot_data(:,1))]);
set(gca,'XTickLabel',datestr([min(handles.plot_data(:,1)): ...
                (max(handles.plot_data(:,1))-min(handles.plot_data(:,1)))/6: ...
                 max(handles.plot_data(:,1))],1));
axis manual;
hold off;

% over-lay precip inverted bar plot...
if handles.have_prec
    axes(axes2); % specify precip axes.
    h_bar = bar(handles.plot_data(:,1),handles.plot_data(:,6));
    set(axes2,'YAxisLocation','right', ...
          'Ydir','reverse', ...
          'Color','none', ...
          'XLim',get(axes1,'XLim'), ...
          'XTick',[], ...
          'XTickLabel',[]);
    set(h_bar,'FaceColor','b', 'EdgeColor','b');
    set(gca, 'TickLength',[0 0]); % this is only way I could figure out to get rid of precip ticks on left y-axis.
                                  % this also gets rid of the right precip axis, too, however.
    ylabel('DAILY PRECIPTATION, IN MM');
    % note: precip max y-axis is set to 3 times the maximum precip value so bars only extend 1/3 of the way down the plot.
    axis([min(handles.plot_data(:,1)), max(handles.plot_data(:,1)), min(handles.plot_data(:,6)), 3*max(handles.plot_data(:,6))]);
    axis manual;
else % clear precip tick marks.
    axes(axes2);
    axis off;
end

if handles.have_sim & handles.have_obs,
    legend(axes1,'Simulated','Observed',2);
elseif handles.have_sim,
    legend(axes1,'Simulated',2);
elseif handles.have_obs,
    legend(axes1,'Observed',2);
end

% adjust plots according to user preferences (checkboxes)...
% (NOTE: cannot just call checkbox subroutines for these because of conflict
% with printing and examine options.)
if handles.hide_precip == 0 % show inverted precip bars.
    set(findobj(axes2,'FaceColor','b'),'Visible','on');
else % hide precip bars.
    set(findobj(axes2, 'FaceColor','b'), 'Visible','off');
end
if handles.hide_obs_ts == 0 % show obs data line.
    set(findobj(axes1,'Type','line','Color',[0 0.5 0]), 'Visible','on');
else % hide obs data line.
    set(findobj(axes1,'Type','line','Color',[0 0.5 0]), 'Visible','off');
end
if handles.logscale_ts ~= 0 % use log Y-scale.
    set(axes1,'Yscale','log');
else % use linear scale.
    set(axes1,'Yscale','linear');
end


%======= HIDE PRECIP INVERTED BAR PLOT ON TIME-SERIES =================
% --- Executes on button press in timeseries_precip_checkbox.
function timeseries_precip_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to timeseries_precip_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of timeseries_precip_checkbox
hide_precip = get(hObject,'Value'); % get and store the show precip time-series plot option.
if hide_precip == 0 % show inverted precip bars.
    set(findobj(handles.precip_axes,'FaceColor','b'),'Visible','on');
else % hide precip bars.
    set(findobj(handles.precip_axes, 'FaceColor','b'), 'Visible','off');
end
handles.hide_precip = hide_precip;
guidata(hObject,handles);


%=================== HIDE OBSERVED TIME-SERIES ===================
% --- Executes on button press in timeseries_obs_checkbox.
function timeseries_obs_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to timeseries_obs_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of timeseries_obs_checkbox
hide_obs_ts = get(hObject,'Value'); % get and store the hide obs. time-series plot option.
if hide_obs_ts == 0 % show obs data line.
    set(findobj(handles.timeseries_axes,'Type','line','Color',[0 0.5 0]), 'Visible','on');
else % hide obs data line.
    set(findobj(handles.timeseries_axes,'Type','line','Color',[0 0.5 0]), 'Visible','off');
end
handles.hide_obs_ts = hide_obs_ts;
guidata(hObject,handles);


%========== USE LOG Y-SCALE FOR TIME-SERIES PLOT ==============
% --- Executes on button press in log_scale_checkbox.
function log_scale_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to log_scale_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of log_scale_checkbox
logscale_ts = get(hObject,'Value'); % get and store the log y-scale time-series plot option.
if logscale_ts ~= 0 % use log Y-scale.
    set(handles.timeseries_axes,'Yscale','log');
else % use linear scale.
    set(handles.timeseries_axes,'Yscale','linear');
end
handles.logscale_ts = logscale_ts;
guidata(hObject,handles);


%========= LAUNCH SEPARATE WINDOW FOR TIME-SERIES PLOT ===========
% --- Executes on button press in examine_timeseries_pushbutton.
function examine_timeseries_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to examine_timeseries_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
figure; % create separate figure window.
axes1 = axes;
axes2 = axes;
plot_ts(handles,axes1,axes2);
% zoom tool in separate figure only works on one axis at a time.
% need to hide precip and activate time-series axes.
set(findobj(axes2, 'FaceColor',[0 0.4 0]), 'Visible','off');
set(axes2,'YTick',[]);
ylabel(' ');
axes(axes1); % activate time-series axes.
h = uicontrol('Style', 'pushbutton', ...
              'String', 'rescale x-axis', ...
              'tooltipString','Click to refresh x-axis ticks', ...
              'Position', [10 10 100 30], ...
              'Callback', 'rescale');


%------------------------------------------------------------------------------------------------------
%============ PLOT SIMULATED VS. OBSERVED =================
function plot_sim_obs(handles,axes1,axes2)
% This function plots the simulated vs. observed.
% The y = x ideal line and least-squares fit lines
% are added, along with some stats.

% if don't have sim, obs, or both: do not do this (return).
if not(handles.have_sim & handles.have_obs)
%     uiwait(warndlg('Simulated vs. Observed plots will not be generated since both data sets do not exist.', ...
%                    'Message','modal'));
    axes(axes1); % give message in plot, set titles and axis lables for raw plot.
    axis([-1 1 -1 1])
    text(-0.3,0,'Plot Not Available');
    xlabel(['SIMULATED ',get_label(handles)]);
    ylabel(['OBSERVED ',get_label(handles)]);
    title_catcode = handles.catcode;
    title_catcode(findstr(title_catcode,'_'))='-';
    title(['\bf',upper(title_catcode),': SIMULATED VS. OBSERVED']);

    axes(axes2); % give message in plot, set titles and axis lables for log plot.
    axis([-1 1 -1 1])
    text(-0.3,0,'Plot Not Available');
    xlabel(['LOG10 OBSERVED ',upper(handles.data_type)]);
    ylabel(['LOG10 SIMULATED ',upper(handles.data_type)]);
    title_catcode = handles.catcode;
    title_catcode(findstr(title_catcode,'_'))='-';
    title(['\bf',upper(title_catcode),': SIMULATED VS. OBSERVED']);
    return;
end

% plot raw (un-logged) data...
xymin = min([handles.plot_data(:,2);handles.plot_data(:,4)]);
xymax = max([handles.plot_data(:,2);handles.plot_data(:,4)]);

axes(axes1); % specify axes to use.
hold on;
plot(handles.plot_data(:,4),handles.plot_data(:,2),'ko','MarkerSize',4); % obs vs. sim dots.

% add least-square fit line with stats...
[b, bint, r, rint, stats] = regress(handles.plot_data(:,2), [ones(size(handles.plot_data(:,4),1),1),handles.plot_data(:,4)]);
plot([xymin,xymax],[b(2)*xymin+b(1),b(2)*xymax+b(1)],'r');
% lsline; % this gives same line as above command - use to check above command.
text(xymin+0.1*(xymax-xymin), xymin+0.90*(xymax-xymin),['m = ',num2str(b(2))],'Color',[1,0,0]) % add m stats text
text(xymin+0.1*(xymax-xymin), xymin+0.85*(xymax-xymin),['b  = ',num2str(b(1))],'Color',[1,0,0]) % add b stats text
text(xymin+0.1*(xymax-xymin), xymin+0.80*(xymax-xymin),['r^2 = ',num2str(stats(1))],'Color',[1,0,0]) % add r2 stats text

% add ideal y = x line...
plot([xymin xymax], [xymin xymax], 'Color',[0 0.5 0], 'LineStyle',':'); % add ideal y=x line

% xlabel(['OBSERVED ',upper(handles.data_type)]);
% ylabel(['SIMULATED ',upper(handles.data_type)]);
xlabel(['SIMULATED ',get_label(handles)]);
ylabel(['OBSERVED ',get_label(handles)]);
title_catcode = handles.catcode;
title_catcode(findstr(title_catcode,'_'))='-';
title(['\bf',upper(title_catcode),': SIMULATED VS. OBSERVED']);
axis([xymin, xymax, xymin, xymax]);
set(gca,'XTick',get(gca,'YTick'));
set(gca,'XTickLabel',get(gca,'YTickLabel'));
axis equal;
box on;
hold off;

%----------- plot logged data ------------
xymin = min([handles.plot_data(:,3);handles.plot_data(:,5)]);
xymax = max([handles.plot_data(:,3);handles.plot_data(:,5)]);

axes(axes2); % specify axes to use.
hold on;
plot(handles.plot_data(:,5),handles.plot_data(:,3),'ko','MarkerSize',4); % obs vs. sim dots.

% add least-square fit line with stats...
[b, bint, r, rint, stats] = regress(handles.plot_data(:,3), [ones(size(handles.plot_data(:,5),1),1),handles.plot_data(:,5)]);
plot([xymin,xymax],[b(2)*xymin+b(1),b(2)*xymax+b(1)],'r');
% lsline; % this gives same line as above command - use to check above command.
text(xymin+0.1*(xymax-xymin), xymin+0.90*(xymax-xymin),['m = ',num2str(b(2))],'Color',[1,0,0]) % add m stats text
text(xymin+0.1*(xymax-xymin), xymin+0.85*(xymax-xymin),['b  = ',num2str(b(1))],'Color',[1,0,0]) % add b stats text
text(xymin+0.1*(xymax-xymin), xymin+0.80*(xymax-xymin),['r^2 = ',num2str(stats(1))],'Color',[1,0,0]) % add r2 stats text

% add ideal y = x line...
plot([xymin xymax], [xymin xymax], 'Color',[0 0.5 0], 'LineStyle',':'); % add ideal y=x line

xlabel(['LOG10 OBSERVED ',upper(handles.data_type)]);
ylabel(['LOG10 SIMULATED ',upper(handles.data_type)]);
title_catcode = handles.catcode;
title_catcode(findstr(title_catcode,'_'))='-';
title(['\bf',upper(title_catcode),': SIMULATED VS. OBSERVED']);
axis([xymin, xymax, xymin, xymax]);
set(gca,'XTick',get(gca,'YTick'));
set(gca,'XTickLabel',get(gca,'YTickLabel'));
axis equal;
box on;
hold off;

% show raw or log data depending on log checkbox state...
% (NOTE: cannot just call checkbox subroutine for this because of conflict
% with printing options.)
h_raw = get(axes1,'Children');
h_log = get(axes2,'Children');
if handles.log_simobs == 0 % show raw data plot.
    for i = 1:length(h_raw),
        set(h_raw(i),'Visible','on'); % show raw data, lines, and stats.
    end
    set(axes1,'Visible','on');  % show raw data axes labels, ticks, and title.
    for i = 1:length(h_log),
        set(h_log(i),'Visible','off'); % hide log data, lines, and stats.
    end
    set(axes2,'Visible','off');  % hide log data axes labels, ticks, and title.
else % show log10 data plot.
    for i = 1:length(h_raw),
        set(h_raw(i),'Visible','off'); % hide raw data, lines, and stats.
    end
    set(axes1,'Visible','off');  % hide raw data axes labels, ticks, and title.
    for i = 1:length(h_log),
        set(h_log(i),'Visible','on'); % show log data, lines, and stats.
    end
    set(axes2,'Visible','on');  % show log data axes labels, ticks, and title.
end


%========= TOGGLE BETWEEN RAW AND LOG SIM/OBS PLOTS ===============
% --- Executes on button press in log_simobs_checkbox.
function log_simobs_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to log_simobs_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of log_simobs_checkbox
log_simobs = get(hObject,'Value');
h_raw = get(handles.simobs_axes,'Children');
h_log = get(handles.log_simobs_axes,'Children');
if log_simobs == 0 % show raw data plot.
    for i = 1:length(h_raw),
        set(h_raw(i),'Visible','on'); % show raw data, lines, and stats.
    end
    set(handles.simobs_axes,'Visible','on');  % show raw data axes labels, ticks, and title.
    for i = 1:length(h_log),
        set(h_log(i),'Visible','off'); % hide log data, lines, and stats.
    end
    set(handles.log_simobs_axes,'Visible','off');  % hide log data axes labels, ticks, and title.
else % show log10 data plot.
    for i = 1:length(h_raw),
        set(h_raw(i),'Visible','off'); % hide raw data, lines, and stats.
    end
    set(handles.simobs_axes,'Visible','off');  % hide raw data axes labels, ticks, and title.
    for i = 1:length(h_log),
        set(h_log(i),'Visible','on'); % show log data, lines, and stats.
    end
    set(handles.log_simobs_axes,'Visible','on');  % show log data axes labels, ticks, and title.
end
handles.log_simobs = log_simobs;
guidata(hObject,handles);


%------------------------------------------------------------------------------------------------------
%============= PLOT CUMULATIVE DISTRIBUTION ===================
function plot_cum(handles,axes1)
% this function plots the cumulative frequency distribution.
% NOTE: no interpolation procedure is used for point-data; raw data used.

axes(axes1);
hold on;

if handles.have_sim
    sim_h = cdfplot(handles.plot_data(:,2));
    set(sim_h,'Color','r');
end
if handles.have_obs
    obs_h = cdfplot(handles.plot_data(:,4));
    set(obs_h,'Color',[0 0.5 0]);
end

title_catcode = handles.catcode;
title_catcode(findstr(title_catcode,'_'))='-';
title(['\bf',upper(title_catcode),': EMPIRICAL CUMULATIVE DISTRIBUTION']);
xlabel(get_label(handles));
ylabel('CUMULATIVE DISTRIBUTION');
hold off;
box on;
if handles.have_sim & handles.have_obs,
    legend('Simulated','Observed',4);
elseif handles.have_sim,
    legend('Simulated',4);
elseif handles.have_obs,
    legend('Observed',4);
end
% legend('Simulated','Observed',4);

% adjust plot for user preferences (check-boxes)...
% (NOTE: cannot just call checkbox subroutines for these because of conflict
% with printing options.)
if handles.log_cum ~= 0 % use log X-scale.
    set(axes1,'Xscale','log');
else % use linear scale.
    set(axes1,'Xscale','linear');
end
if handles.hide_obs_cum == 0 % show obs line.
    set(findobj(axes1,'Type','line','Color',[0 0.5 0]), 'Visible','on');
else % hide obs line.
    set(findobj(axes1,'Type','line','Color',[0 0.5 0]), 'Visible','off');
end


%========== USE LOG-SCALE FOR CUMULATIVE PLOT ===========================
% --- Executes on button press in plot_log_data_checkbox.
function plot_log_data_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to plot_log_data_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of plot_log_data_checkbox
log_cum = get(hObject,'Value'); % get and store the don't log data cumulative plot option.
if log_cum ~= 0 % use log X-scale.
    set(handles.cumprob_axes,'Xscale','log');
else % use linear scale.
    set(handles.cumprob_axes,'Xscale','linear');
end
handles.log_cum = log_cum;
guidata(hObject,handles);


%=============== HIDE OBSERVED CUMULATIVE PLOT =====================
% --- Executes on button press in cumprob_obs_checkbox.
function cumprob_obs_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to cumprob_obs_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of cumprob_obs_checkbox
hide_obs_cum = get(hObject,'Value'); % get and store the hide observed cumulative plot option.
if hide_obs_cum == 0 % show obs line.
    set(findobj(handles.cumprob_axes,'Type','line','Color',[0 0.5 0]), 'Visible','on');
else % hide obs line.
    set(findobj(handles.cumprob_axes,'Type','line','Color',[0 0.5 0]), 'Visible','off');
end
handles.hide_obs_cum = hide_obs_cum;
guidata(hObject,handles);


%------------------------------------------------------------------------------------------------------
%============ CALCULATE AND DISPLAY STATS ================
function calc_stats(handles)
% This function computes statistics on the current data set and
% displays them in the corresponding GUI text boxes.
% Stats are computed on points where there are both sim and obs data.

% create stats data set (ie: remove data where sim, obs, or both = NaN).
% note: raw and log data treated separately because 0's were removed from
% the data before logging.
stats_data = handles.plot_data(:,[1,2,4]);
stats_data(any(isnan(stats_data)'),:) = []; % remove any row with NaN.
log_stats_data = handles.plot_data(:,[1,3,5]);
log_stats_data(any(isnan(log_stats_data)'),:) = []; % remove any row with NaN.

% calc/display # of common data points...
n = size(stats_data,1);
set(handles.n_simobs_points_text, 'String', n);
log_n = size(log_stats_data,1);
set(handles.n_log_simobs_points_text, 'String', log_n);
 
% calc/display mins...
set(    handles.min_sim_text, 'String', min(stats_data(:,2)));
set(handles.log_min_sim_text, 'String', min(log_stats_data(:,2)));
set(    handles.min_obs_text, 'String', min(stats_data(:,3)));
set(handles.log_min_obs_text, 'String', min(log_stats_data(:,3)));

% calc/display means...
set(    handles.sim_mean_text, 'String', mean(stats_data(:,2)));
set(handles.log_sim_mean_text, 'String', mean(log_stats_data(:,2)));
set(    handles.obs_mean_text, 'String', mean(stats_data(:,3)));
set(handles.log_obs_mean_text, 'String', mean(log_stats_data(:,3)));

% calc/display medians...
set(    handles.sim_median_text, 'String', median(stats_data(:,2)));
set(handles.log_sim_median_text, 'String', median(log_stats_data(:,2)));
set(    handles.obs_median_text, 'String', median(stats_data(:,3)));
set(handles.log_obs_median_text, 'String', median(log_stats_data(:,3)));

% calc/display maxes...
set(    handles.max_sim_text, 'String', max(stats_data(:,2)));
set(handles.log_max_sim_text, 'String', max(log_stats_data(:,2)));
set(    handles.max_obs_text, 'String', max(stats_data(:,3)));
set(handles.log_max_obs_text, 'String', max(log_stats_data(:,3)));

% calc/display variance...
% (the following manual calculations produces same results as built in VAR function.)
%     sim_var = dot(stats_data(:,2) - sim_mean, stats_data(:,2) - sim_mean)/(n-1);
% log_sim_var = dot(log_stats_data(:,2) - log_sim_mean, log_stats_data(:,2) - log_sim_mean)/(log_n-1);
%     obs_var = dot(stats_data(:,3) - obs_mean, stats_data(:,3) - obs_mean)/(n-1);
% log_obs_var = dot(log_stats_data(:,3) - log_obs_mean, log_stats_data(:,3) - log_obs_mean)/(log_n-1);
set(    handles.sim_variance_text, 'String', var(stats_data(:,2)));
set(handles.log_sim_variance_text, 'String', var(log_stats_data(:,2)));
set(    handles.obs_variance_text, 'String', var(stats_data(:,3)));
set(handles.log_obs_variance_text, 'String', var(log_stats_data(:,3)));

% calc/display error variance...
    err_var = dot(stats_data(:,2)-stats_data(:,3),stats_data(:,2)-stats_data(:,3))/(n-1);
log_err_var = dot(log_stats_data(:,2)-log_stats_data(:,3),log_stats_data(:,2)-log_stats_data(:,3))/(log_n-1);
set(    handles.error_variance_value_text, 'String', err_var);
set(handles.log_error_variance_value_text, 'String', log_err_var);

% calc/display percent relative bias...
%     rel_bias = sum(stats_data(:,2)-stats_data(:,3))/n; % this is absolute bias.
% log_rel_bias = sum(log_stats_data(:,2)-log_stats_data(:,3))/log_n; % this is absolute bias.
    rel_bias = 100*sum(stats_data(:,2)-stats_data(:,3))/(n*mean(stats_data(:,3)));
log_rel_bias = 100*sum(log_stats_data(:,2)-log_stats_data(:,3))/(log_n*mean(log_stats_data(:,3)));
set(    handles.relative_bias_value_text, 'String', rel_bias);
set(handles.log_relative_bias_value_text, 'String', log_rel_bias);

% calc/display rel std err...
    rel_std_err = err_var/var(stats_data(:,3));
log_rel_std_err = log_err_var/var(log_stats_data(:,3));
set(    handles.rel_std_error_value_text, 'String', rel_std_err);
set(handles.log_rel_std_error_value_text, 'String', log_rel_std_err);

% calc/display model efficiency...
set(    handles.model_eff_value_text, 'String', 1-rel_std_err);
set(handles.log_model_eff_value_text, 'String', 1-log_rel_std_err);

% calc/display JB test for normality results, and check boxes...
[    h_sim,     p_sim] = jbtest(stats_data(:,2));
[h_log_sim, p_log_sim] = jbtest(log_stats_data(:,2));
[    h_obs,     p_obs] = jbtest(stats_data(:,3));
[h_log_obs, p_log_obs] = jbtest(log_stats_data(:,3));
set(    handles.sim_JB_pvalue_text, 'String', p_sim);
set(handles.log_sim_JB_pvalue_text, 'String', p_log_sim);
set(    handles.obs_JB_pvalue_text, 'String', p_obs);
set(handles.log_obs_JB_pvalue_text, 'String', p_log_obs);
if h_sim == 0, % passes normality test.
    set(findobj('Tag','sim_JB_pass_checkbox'),'Value',1)
else % fails normality test.
    set(findobj('Tag','sim_JB_pass_checkbox'),'Value',0)
end
if h_log_sim == 0, % passes normality test.
    set(findobj('Tag','log_sim_JB_pass_checkbox'),'Value',1)
else % fails normality test.
    set(findobj('Tag','log_sim_JB_pass_checkbox'),'Value',0)
end
if h_obs == 0, % passes normality test.
    set(findobj('Tag','obs_JB_pass_checkbox'),'Value',1)
else % fails normality test.
    set(findobj('Tag','obs_JB_pass_checkbox'),'Value',0)
end
if h_log_obs == 0, % passes normality test.
    set(findobj('Tag','log_obs_JB_pass_checkbox'),'Value',1)
else % fails normality test.
    set(findobj('Tag','log_obs_JB_pass_checkbox'),'Value',0)
end

%------------------------------------------------------------------------------------------------------
function calc_sim_stats(handles)
% This function computes statistics on the current simulated data set only and
% displays them in the corresponding GUI text boxes.
% Stats that involve both sim and observed are ignored.

% create sim stats data set.
% note: raw and log data treated separately because 0's were removed from
% the data before logging.
stats_data = handles.plot_data(:,[1,2]);
stats_data(any(isnan(stats_data)'),:) = []; % remove any row with NaN.
log_stats_data = handles.plot_data(:,[1,3]);
log_stats_data(any(isnan(log_stats_data)'),:) = []; % remove any row with NaN.

% calc/display # of common data points...
n = size(stats_data,1);
set(handles.n_simobs_points_text, 'String', n);
log_n = size(log_stats_data,1);
set(handles.n_log_simobs_points_text, 'String', log_n);
 
% calc/display mins...
set(    handles.min_sim_text, 'String', min(stats_data(:,2)));
set(handles.log_min_sim_text, 'String', min(log_stats_data(:,2)));
set(    handles.min_obs_text, 'String', 'N/A');
set(handles.log_min_obs_text, 'String', 'N/A');

% calc/display means...
set(    handles.sim_mean_text, 'String', mean(stats_data(:,2)));
set(handles.log_sim_mean_text, 'String', mean(log_stats_data(:,2)));
set(    handles.obs_mean_text, 'String', 'N/A');
set(handles.log_obs_mean_text, 'String', 'N/A');

% calc/display medians...
set(    handles.sim_median_text, 'String', median(stats_data(:,2)));
set(handles.log_sim_median_text, 'String', median(log_stats_data(:,2)));
set(    handles.obs_median_text, 'String', 'N/A');
set(handles.log_obs_median_text, 'String', 'N/A');

% calc/display maxes...
set(    handles.max_sim_text, 'String', max(stats_data(:,2)));
set(handles.log_max_sim_text, 'String', max(log_stats_data(:,2)));
set(    handles.max_obs_text, 'String', 'N/A');
set(handles.log_max_obs_text, 'String', 'N/A');

% calc/display variance...
% (the following manual calculations produces same results as built in VAR function.)
%     sim_var = dot(stats_data(:,2) - sim_mean, stats_data(:,2) - sim_mean)/(n-1);
% log_sim_var = dot(log_stats_data(:,2) - log_sim_mean, log_stats_data(:,2) - log_sim_mean)/(log_n-1);
set(    handles.sim_variance_text, 'String', var(stats_data(:,2)));
set(handles.log_sim_variance_text, 'String', var(log_stats_data(:,2)));
set(    handles.obs_variance_text, 'String', 'N/A');
set(handles.log_obs_variance_text, 'String', 'N/A');

% calc/display error variance...
set(    handles.error_variance_value_text, 'String', 'N/A');
set(handles.log_error_variance_value_text, 'String', 'N/A');

% calc/display percent relative bias...
set(    handles.relative_bias_value_text, 'String', 'N/A');
set(handles.log_relative_bias_value_text, 'String', 'N/A');

% calc/display rel std err...
set(    handles.rel_std_error_value_text, 'String', 'N/A');
set(handles.log_rel_std_error_value_text, 'String', 'N/A');

% calc/display model efficiency...
set(    handles.model_eff_value_text, 'String', 'N/A');
set(handles.log_model_eff_value_text, 'String', 'N/A');

% calc/display JB test for normality results, and check boxes...
[    h_sim,     p_sim] = jbtest(stats_data(:,2));
[h_log_sim, p_log_sim] = jbtest(log_stats_data(:,2));
set(    handles.sim_JB_pvalue_text, 'String', p_sim);
set(handles.log_sim_JB_pvalue_text, 'String', p_log_sim);
set(    handles.obs_JB_pvalue_text, 'String', 'N/A');
set(handles.log_obs_JB_pvalue_text, 'String', 'N/A');
if h_sim == 0, % passes normality test.
    set(findobj('Tag','sim_JB_pass_checkbox'),'Value',1)
else % fails normality test.
    set(findobj('Tag','sim_JB_pass_checkbox'),'Value',0)
end
if h_log_sim == 0, % passes normality test.
    set(findobj('Tag','log_sim_JB_pass_checkbox'),'Value',1)
else % fails normality test.
    set(findobj('Tag','log_sim_JB_pass_checkbox'),'Value',0)
end
set(findobj('Tag','obs_JB_pass_checkbox'),'Value',0)
set(findobj('Tag','log_obs_JB_pass_checkbox'),'Value',0)

%------------------------------------------------------------------------------------------------------
function calc_obs_stats(handles)
% This function computes statistics on the current observed data set only and
% displays them in the corresponding GUI text boxes.
% Stats that involve both sim and observed are ignored.

% create obs stats data set.
% note: raw and log data treated separately because 0's were removed from
% the data before logging.
stats_data = handles.plot_data(:,[1,4]);
stats_data(any(isnan(stats_data)'),:) = []; % remove any row with NaN.
log_stats_data = handles.plot_data(:,[1,5]);
log_stats_data(any(isnan(log_stats_data)'),:) = []; % remove any row with NaN.

% calc/display # of common data points...
n = size(stats_data,1);
set(handles.n_simobs_points_text, 'String', n);
log_n = size(log_stats_data,1);
set(handles.n_log_simobs_points_text, 'String', log_n);
 
% calc/display mins...
set(    handles.min_obs_text, 'String', min(stats_data(:,2)));
set(handles.log_min_obs_text, 'String', min(log_stats_data(:,2)));
set(    handles.min_sim_text, 'String', 'N/A');
set(handles.log_min_sim_text, 'String', 'N/A');

% calc/display means...
set(    handles.obs_mean_text, 'String', mean(stats_data(:,2)));
set(handles.log_obs_mean_text, 'String', mean(log_stats_data(:,2)));
set(    handles.sim_mean_text, 'String', 'N/A');
set(handles.log_sim_mean_text, 'String', 'N/A');

% calc/display medians...
set(    handles.obs_median_text, 'String', median(stats_data(:,2)));
set(handles.log_obs_median_text, 'String', median(log_stats_data(:,2)));
set(    handles.sim_median_text, 'String', 'N/A');
set(handles.log_sim_median_text, 'String', 'N/A');

% calc/display maxes...
set(    handles.max_obs_text, 'String', max(stats_data(:,2)));
set(handles.log_max_obs_text, 'String', max(log_stats_data(:,2)));
set(    handles.max_sim_text, 'String', 'N/A');
set(handles.log_max_sim_text, 'String', 'N/A');

% calc/display variance...
set(    handles.obs_variance_text, 'String', var(stats_data(:,2)));
set(handles.log_obs_variance_text, 'String', var(log_stats_data(:,2)));
set(    handles.sim_variance_text, 'String', 'N/A');
set(handles.log_sim_variance_text, 'String', 'N/A');

% calc/display error variance...
set(    handles.error_variance_value_text, 'String', 'N/A');
set(handles.log_error_variance_value_text, 'String', 'N/A');

% calc/display percent relative bias...
set(    handles.relative_bias_value_text, 'String', 'N/A');
set(handles.log_relative_bias_value_text, 'String', 'N/A');

% calc/display rel std err...
set(    handles.rel_std_error_value_text, 'String', 'N/A');
set(handles.log_rel_std_error_value_text, 'String', 'N/A');

% calc/display model efficiency...
set(    handles.model_eff_value_text, 'String', 'N/A');
set(handles.log_model_eff_value_text, 'String', 'N/A');

% calc/display JB test for normality results, and check boxes...
[    h_obs,     p_obs] = jbtest(stats_data(:,2));
[h_log_obs, p_log_obs] = jbtest(log_stats_data(:,2));
set(    handles.obs_JB_pvalue_text, 'String', p_obs);
set(handles.log_obs_JB_pvalue_text, 'String', p_log_obs);
set(    handles.sim_JB_pvalue_text, 'String', 'N/A');
set(handles.log_sim_JB_pvalue_text, 'String', 'N/A');
if h_obs == 0, % passes normality test.
    set(findobj('Tag','obs_JB_pass_checkbox'),'Value',1)
else % fails normality test.
    set(findobj('Tag','obs_JB_pass_checkbox'),'Value',0)
end
if h_log_obs == 0, % passes normality test.
    set(findobj('Tag','log_obs_JB_pass_checkbox'),'Value',1)
else % fails normality test.
    set(findobj('Tag','log_obs_JB_pass_checkbox'),'Value',0)
end
set(findobj('Tag','sim_JB_pass_checkbox'),'Value',0)
set(findobj('Tag','log_sim_JB_pass_checkbox'),'Value',0)


%=============== JB-TEST CHECK-BOXES ==============================
% The following check-boxes are checked automatically if the JB-test
% for normality is passed for the respective data set. If the user
% tries to manually check / uncheck them, they will switch back to their
% original state.
% --- Executes on button press in obs_JB_pass_checkbox.
function obs_JB_pass_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to obs_JB_pass_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of obs_JB_pass_checkbox
check_status = get(hObject,'Value');
if check_status == 1, % uncheck if user manually checks.
    set(findobj('Tag','obs_JB_pass_checkbox'),'Value',0);
else % check if user manually unchecks.
    set(findobj('Tag','obs_JB_pass_checkbox'),'Value',1);
end
% --- Executes on button press in log_obs_JB_pass_checkbox.
function log_obs_JB_pass_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to log_obs_JB_pass_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of log_obs_JB_pass_checkbox
check_status = get(hObject,'Value');
if check_status == 1, % uncheck if user manually checks.
    set(findobj('Tag','log_obs_JB_pass_checkbox'),'Value',0);
else % check if user manually unchecks.
    set(findobj('Tag','log_obs_JB_pass_checkbox'),'Value',1);
end
% --- Executes on button press in sim_JB_pass_checkbox.
function sim_JB_pass_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to sim_JB_pass_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of sim_JB_pass_checkbox
check_status = get(hObject,'Value');
if check_status == 1, % uncheck if user manually checks.
    set(findobj('Tag','sim_JB_pass_checkbox'),'Value',0);
else % check if user manually unchecks.
    set(findobj('Tag','sim_JB_pass_checkbox'),'Value',1);
end
% --- Executes on button press in log_sim_JB_pass_checkbox.
function log_sim_JB_pass_checkbox_Callback(hObject, eventdata, handles)
% hObject    handle to log_sim_JB_pass_checkbox (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of log_sim_JB_pass_checkbox
check_status = get(hObject,'Value');
if check_status == 1, % uncheck if user manually checks.
    set(findobj('Tag','log_sim_JB_pass_checkbox'),'Value',0);
else % check if user manually unchecks.
    set(findobj('Tag','log_sim_JB_pass_checkbox'),'Value',1);
end


%------------------------------------------------------------------------------------------------------
%=============== RESIDUAL PLOT ======================
% --- Executes on button press in residual_pushbutton.
function residual_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to residual_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if not(handles.have_sim & handles.have_obs) % don't have sim and obs, feature unavailable.
    uiwait(warndlg('This feature is unavailable since both simulated and observed data sets do not exist.', ...
                   'Message','modal'));
    return;
end

if isequalwithequalnans(handles.plot_data,[NaN,NaN,NaN,NaN,NaN,NaN]) == 1 % no data loaded.
    uiwait(errordlg('No data is currently loaded - no action taken.','Error: No Data Loaded','modal'));
    return;
end

h_fig = figure; % create separate figure window.
set(h_fig,'Name','Residual Plots');

% plot absolute residuals vs. time...
subplot(2,3,1);
hold on;
h = bar(handles.plot_data(:,1),(handles.plot_data(:,2)-handles.plot_data(:,4))); % (sim_i - obs_i).
set(h,'FaceColor',[1 0 0], 'EdgeColor',[1 0 0]);
title(['\bfAbsolute ',handles.data_type,' Residuals vs. Time']);
ylabel('SIM_i - OBS_i');
datetick('x',28);
plot([handles.plot_data(1,1),handles.plot_data(end,1)],[0,0], 'Color',[1 1 1]); % clear zero-line.
plot([handles.plot_data(1,1),handles.plot_data(end,1)],[0,0], 'Color',[0 0.5 0], 'LineStyle',':'); % add dashed ideal zero-line.
hold off;

% plot scaled residuals vs. time...
% find mean of data without NaN's...
obs_mean = handles.plot_data(:,4);
obs_mean(isnan(obs_mean)) = []; % remove NaN's.
obs_mean = mean(obs_mean);
subplot(2,3,2);
hold on;
h = bar(handles.plot_data(:,1),(handles.plot_data(:,2)-handles.plot_data(:,4))/obs_mean); % (sim_i - obs_i)/mean(obs).
set(h,'FaceColor',[1 0 0], 'EdgeColor',[1 0 0]);
title(['\bfScaled ',handles.data_type,' Residuals vs. Time']);
ylabel('(SIM_i - OBS)/mean(OBS)');
datetick('x',28);
plot([handles.plot_data(1,1),handles.plot_data(end,1)],[0,0], 'Color',[1 1 1]); % clear zero-line.
plot([handles.plot_data(1,1),handles.plot_data(end,1)],[0,0], 'Color',[0 0.5 0], 'LineStyle',':'); % add dashed ideal zero-line.
hold off;

% plot relative error vs. time...
subplot(2,3,3);
hold on;
h = bar(handles.plot_data(:,1),(handles.plot_data(:,2)-handles.plot_data(:,4))./handles.plot_data(:,4)); % (sim_i - obs_i)/obs_i.
set(h,'FaceColor',[1 0 0], 'EdgeColor',[1 0 0]);
title(['\bf',handles.data_type,' Relative Error vs. Time']);
ylabel('(SIM_i - OBS_i)/OBS_i');
datetick('x',28);
plot([handles.plot_data(1,1),handles.plot_data(end,1)],[0,0], 'Color',[1 1 1]); % clear zero-line.
plot([handles.plot_data(1,1),handles.plot_data(end,1)],[0,0], 'Color',[0 0.5 0], 'LineStyle',':'); % add dashed ideal zero-line.
hold off;

%------------------------------
% plot absolute residuals vs. observed...
subplot(2,3,4);
hold on;
h = bar(handles.plot_data(:,4),(handles.plot_data(:,2)-handles.plot_data(:,4))); % (sim_i - obs_i).
set(h,'FaceColor',[1 0 0], 'EdgeColor',[1 0 0]);
title(['\bfAbsolute ',handles.data_type,' Residuals vs. Observed']);
xlabel(['Observed ',handles.data_type]);
ylabel('SIM_i - OBS_i');
plot([min(handles.plot_data(:,4)),max(handles.plot_data(:,4))],[0,0], 'Color',[1 1 1]); % clear zero-line.
plot([min(handles.plot_data(:,4)),max(handles.plot_data(:,4))],[0,0], 'Color',[0 0.5 0], 'LineStyle',':'); % add dashed ideal zero-line.
hold off;

% plot scaled residuals vs. observed...
subplot(2,3,5);
hold on;
h = bar(handles.plot_data(:,4),(handles.plot_data(:,2)-handles.plot_data(:,4))/obs_mean); % (sim_i - obs_i)/mean(obs).
set(h,'FaceColor',[1 0 0], 'EdgeColor',[1 0 0]);
title(['\bfScaled ',handles.data_type,' Residuals vs. Observed']);
xlabel(['Observed ',handles.data_type]);
ylabel('(SIM_i - OBS)/mean(OBS)');
plot([min(handles.plot_data(:,4)),max(handles.plot_data(:,4))],[0,0], 'Color',[1 1 1]); % clear zero-line.
plot([min(handles.plot_data(:,4)),max(handles.plot_data(:,4))],[0,0], 'Color',[0 0.5 0], 'LineStyle',':'); % add dashed ideal zero-line.
hold off;

% plot relative error vs. observed...
subplot(2,3,6);
hold on;
h = bar(handles.plot_data(:,4),(handles.plot_data(:,2)-handles.plot_data(:,4))./handles.plot_data(:,4)); % (sim_i - obs_i)/obs_i.
set(h,'FaceColor',[1 0 0], 'EdgeColor',[1 0 0]);
title(['\bf',handles.data_type,' Relative Error vs. Observed']);
xlabel(['Observed ',handles.data_type]);
ylabel('(SIM_i - OBS_i)/OBS_i');
plot([min(handles.plot_data(:,4)),max(handles.plot_data(:,4))],[0,0], 'Color',[1 1 1]); % clear zero-line.
plot([min(handles.plot_data(:,4)),max(handles.plot_data(:,4))],[0,0], 'Color',[0 0.5 0], 'LineStyle',':'); % add dashed ideal zero-line.
hold off;


%------------------------------------------------------------------------------------------------------
%======================= CREATE QQ-PLOT ==========================
% --- Executes on button press in QQ_plot_pushbutton.
function QQ_plot_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to QQ_plot_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if isequalwithequalnans(handles.plot_data,[NaN,NaN,NaN,NaN,NaN,NaN]) == 1 % no data loaded.
    uiwait(errordlg('No data is currently loaded - no action taken.','Error: No Data Loaded','modal'));
    return;
end

h = figure; % create new figure window.
set(h,'Name','Percentile Plots')

% sim vs. normal...
subplot(2,3,1);
if handles.have_sim
    qqplot(handles.plot_data(:,2));
    title('Raw Simulated vs. Normal Distribution');
    xlabel('Standard Normal Percentiles');
    ylabel('Simulated Percentiles');
end

% obs vs. normal...
subplot(2,3,2);
if handles.have_obs
    qqplot(handles.plot_data(:,4));
    title('Raw Observed vs. Normal Distribution');
    xlabel('Standard Normal Percentiles');
    ylabel('Observed Percentiles');
end

% sim vs. obs...
subplot(2,3,3);
if handles.have_sim & handles.have_obs
    qqplot(handles.plot_data(:,2),handles.plot_data(:,4));
    title('Raw Simulated vs. Raw Observed');
    xlabel('Observed Percentiles');
    ylabel('Simulated Percentiles');
end

%----------------------
% log_sim vs. normal...
subplot(2,3,4);
if handles.have_sim
    qqplot(handles.plot_data(:,3));
    title('Log10 Simulated vs. Normal Distribution');
    xlabel('Standard Normal Percentiles');
    ylabel('Log10 Simulated Percentiles');
end

% log_obs vs. normal...
subplot(2,3,5);
if handles.have_obs
    qqplot(handles.plot_data(:,5));
    title('Log10 Observed vs. Normal Distribution');
    xlabel('Standard Normal Percentiles');
    ylabel('Log10 Observed Percentiles');
end

% log_sim vs. log_obs...
subplot(2,3,6);
if handles.have_obs & handles.have_sim
    qqplot(handles.plot_data(:,3),handles.plot_data(:,5));
    title('Log10 Simulated vs. Log10 Observed');
    xlabel('Log10 Observed Percentiles');
    ylabel('Log10 Simulated Percentiles');
end


%------------------------------------------------------------------------------------------------------
%=============== DAILY ACCUMULATION PLOT ================
% --- Executes on button press in accumulative_plot_pushbutton.
function accumulative_plot_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to accumulative_plot_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if isequalwithequalnans(handles.plot_data,[NaN,NaN,NaN,NaN,NaN,NaN]) == 1 % no data loaded.
    uiwait(errordlg('No data is currently loaded - no action taken.','Error: No Data Loaded','modal'));
    return;
end

% get sim data and trim to natural interval...
if handles.have_sim
    sim = handles.plot_data(:,1:2);
    i_start = 1;
    i_end = size(sim,1);
    for i = 1:size(sim,1),
        if isnan(sim(i,2)) == 0; % found 1st non-NaN value from beginning.
            i_start = i;
            break;
        end
    end
    for i = size(sim,1):-1:1,
        if isnan(sim(i,2)) == 0; % found 1st non-NaN value from end.
            i_end = i;
            break;
        end
    end
    sim = sim(i_start:i_end,:);
end

% get obs data and trim to natural interval...
if handles.have_obs
    obs = [handles.plot_data(:,1),handles.plot_data(:,4)];
    i_start = 1;
    i_end = size(obs,1);
    for i = 1:size(obs,1),
        if isnan(obs(i,2)) == 0; % found 1st non-NaN value from beginning.
            i_start = i;
            break;
        end
    end
    for i = size(obs,1):-1:1,
        if isnan(obs(i,2)) == 0; % found 1st non-NaN value from end.
            i_end = i;
            break;
        end
    end
    obs = obs(i_start:i_end,:);
end

% trim sim/obs to their common date/time interval...
if handles.have_sim & handles.have_obs
    [data,i_sim,i_obs] = intersect(sim(:,1),obs(:,1));
    data(:,2) = sim(i_sim,2);
    data(:,3) = obs(i_obs,2);
elseif handles.have_sim
    data = sim;
    data(:,3) = 0; % pad missing obs with 0's.
elseif handles.have_obs
    data = obs;
    data(:,3) = data(:,2);
    data(:,2) = 0; % pad missing sim with 0's.
end

% remove any NaN's in simulated data...
if handles.have_sim
    temp = data(:,[1 2]);
    temp(any(isnan(temp)'),:) = [];
    % replace simulated data with nearest-neighbor interpolation using remaining values...
    data(:,2) = interp1(temp(:,1),temp(:,2),data(:,1),'nearest','extrap');
end

% remove any NaN's in observed data...
if handles.have_obs
    temp = data(:,[1 3]);
    temp(any(isnan(temp)'),:) = [];
    % replace observed data with nearest-neighbor interpolation using remaining values...
    data(:,3) = interp1(temp(:,1),temp(:,2),data(:,1),'nearest','extrap');
end

% compute accumulative values for sim...
if handles.have_sim
    sim_accum(1,1) = data(1,2);
    for i = 2:size(data,1),
        sim_accum(i,1) = sum(data(1:i,2));
    end
end

% compute accumulative values for obs...
if handles.have_obs
    obs_accum(1,1) = data(1,3);
    for i = 2:size(data,1),
        obs_accum(i,1) = sum(data(1:i,3));
    end
end

% plot results in new window.
h = figure; % create separate figure window.
set(h,'Name','Daily Accumulations')
hold on;
if handles.have_sim
    plot(data(:,1),sim_accum(:,1),'r'); % sim.
end
if handles.have_obs
    obs_h = plot(data(:,1),obs_accum(:,1)); % obs.
    set(obs_h,'Color',[0 0.5 0]);
end
hold off;
if handles.have_sim & handles.have_obs
    title('\bfDaily Accumulations of Observed and Simulated Values');
elseif handles.have_sim
    title('\bfDaily Accumulations of Simulated Values');
elseif handles.have_obs
    title('\bfDaily Accumulations of Observed Values');
end
ylabel(get_label(handles));
datetick('x',28);
% axis([ month_avg(1,1)-datenum([0 0 14]), month_avg(end,1)+datenum([0 0 14]), -inf, inf ]);
switch upper(handles.data_type)
    case {'FLOW','BFLW','QFLW'}
        if handles.have_sim & handles.have_obs
            legend('Simulated','Observed',2);
        elseif handles.have_sim
            legend('Simulated',2);            
        elseif handles.have_obs
            legend('Observed',2);
        end
    otherwise
        if handles.have_sim & handles.have_obs
            legend('Simulated','Time-Weighted Observed',2);
        elseif handles.have_sim
            legend('Simulated',2);            
        elseif handles.have_obs
            legend('Time-Weighted Observed',2);
        end
end


%------------------------------------------------------------------------------------------------------
%=============== MONTHLY AVERAGE BAR-PLOT ==================
% --- Executes on button press in monthly_avg_pushbutton.
function monthly_avg_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to monthly_avg_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if isequalwithequalnans(handles.plot_data,[NaN,NaN,NaN,NaN,NaN,NaN]) == 1 % no data loaded.
    uiwait(errordlg('No data is currently loaded - no action taken.','Error: No Data Loaded','modal'));
    return;
end

% if don't have sim or obs, replace NaN's with 0's to create 0 height bars.
if handles.have_sim & handles.have_obs
    sim = handles.plot_data(:,1:2);
    obs = [handles.plot_data(:,1),handles.plot_data(:,4)];
elseif handles.have_sim
    sim = handles.plot_data(:,1:2);    
    obs = [sim(:,1)];
    obs(:,2) = 0;
elseif handles.have_obs
    obs = [handles.plot_data(:,1),handles.plot_data(:,4)];
    sim = [obs(:,1)];
    sim(:,2) = 0;
end

% get sim data and trim to natural interval...
i_start = 1;
i_end = size(sim,1);
for i = 1:size(sim,1),
    if isnan(sim(i,2)) == 0; % found 1st non-NaN value from beginning.
        i_start = i;
        break;
    end
end
for i = size(sim,1):-1:1,
    if isnan(sim(i,2)) == 0; % found 1st non-NaN value from end.
        i_end = i;
        break;
    end
end
sim = sim(i_start:i_end,:);

% get obs data and trim to natural interval...
i_start = 1;
i_end = size(obs,1);
for i = 1:size(obs,1),
    if isnan(obs(i,2)) == 0; % found 1st non-NaN value from beginning.
        i_start = i;
        break;
    end
end
for i = size(obs,1):-1:1,
    if isnan(obs(i,2)) == 0; % found 1st non-NaN value from end.
        i_end = i;
        break;
    end
end
obs = obs(i_start:i_end,:);

% trim sim/obs to their common date/time interval...
[data,i_sim,i_obs] = intersect(sim(:,1),obs(:,1));
data(:,2) = sim(i_sim,2);
data(:,3) = obs(i_obs,2);

% remove any NaN's in simulated data...
temp = data(:,[1 2]);
temp(any(isnan(temp)'),:) = [];
% replace simulated data with nearest-neighbor interpolation using remaining values...
data(:,2) = interp1(temp(:,1),temp(:,2),data(:,1),'nearest','extrap');

% remove any NaN's in observed data...
temp = data(:,[1 3]);
temp(any(isnan(temp)'),:) = [];
% replace observed data with nearest-neighbor interpolation using remaining values...
data(:,3) = interp1(temp(:,1),temp(:,2),data(:,1),'nearest','extrap');

% % TEST PLOT FOR VERIFYING MANIPULATED DATA...
% figure;
% hold on;
% plot(data(:,1),data(:,2),'r');
% plot(data(:,1),data(:,3),'g');
% hold off;
% datetick('x',28);

all_dates = datenum(str2num(datestr(data(:,1),'yyyy')), ...
                    str2num(datestr(data(:,1),'mm')),   ...
                    ones(size(data,1),1)); % make list of all month-year pairs.
unique_dates = unique(all_dates); % make list of unique month-year pairs.
x_label = datestr(unique_dates,28); % expl: Feb1999
x_label(:,2:5) = []; % expl: F99

% find mean of sim & observed data values that match each unique month-year pair...
monthly_avg = [NaN,NaN,NaN];
for i = 1:size(unique_dates,1),
    match = (all_dates == unique_dates(i));
    match.*data(:,2);
    month_avg(i,1:3) = [unique_dates(i), ...
                        sum(match.*data(:,2))/sum(match), ...
                        sum(match.*data(:,3))/sum(match)];
end

% find overall means for entire period...
month_avg = [month_avg; ...
            [unique_dates(end)+datenum(0,0,30), mean(data(:,2)), mean(data(:,3))]];
x_label = [x_label;'TOT'];

% plot results in new window.
h = figure; % create separate figure window.
set(h,'Name','Individual Monthly Averages')
bar(month_avg(:,1),month_avg(:,2:3),1);
h = get(gca,'Children');
set(h(1),'FaceColor',[0 0.5 0]);
set(h(2),'FaceColor',[0.8 0 0]);
title('\bfObserved and Simulated Individual Monthly Averages');
ylabel(get_label(handles));
set(gca, 'XTick',month_avg(:,1), 'XTickLabel',x_label);
axis([ month_avg(1,1)-datenum([0 0 14]), month_avg(end,1)+datenum([0 0 14]), -inf, inf ]);
if handles.have_sim & handles.have_obs
    switch upper(handles.data_type)
        case {'FLOW','BFLW','QFLW'}
            legend('Simulated','Observed',2);
        otherwise
            legend('Simulated','Time-Weighted Observed',2);
    end
elseif handles.have_sim
    legend('Simulated',' ',2);
elseif handles.have_obs
    switch upper(handles.data_type)
        case {'FLOW','BFLW','QFLW'}
            legend(' ','Observed',2);
        otherwise
            legend(' ','Time-Weighted Observed',2);
    end
end


%------------------------------------------------------------------------------------------------------
%=============== ACCUMULATED MONTHLY AVERAGE BAR-PLOT ==================
% --- Executes on button press in accum_monthly_avg_pushbutton.
% NOTE: This funtion is similar to the normal monthly averages pushbutton,
% but averages are taken across all years for each month. (ie: a 5-year
% simulation span will still only create 12 month bars.)
function accum_monthly_avg_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to accum_monthly_avg_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if isequalwithequalnans(handles.plot_data,[NaN,NaN,NaN,NaN,NaN,NaN]) == 1 % no data loaded.
    uiwait(errordlg('No data is currently loaded - no action taken.','Error: No Data Loaded','modal'));
    return;
end

% if don't have sim or obs, replace NaN's with 0's to create 0 height bars.
% if don't have sim or obs, replace NaN's with 0's to create 0 height bars.
if handles.have_sim & handles.have_obs
    sim = handles.plot_data(:,1:2);
    obs = [handles.plot_data(:,1),handles.plot_data(:,4)];
elseif handles.have_sim
    sim = handles.plot_data(:,1:2);    
    obs = [sim(:,1)];
    obs(:,2) = 0;
elseif handles.have_obs
    obs = [handles.plot_data(:,1),handles.plot_data(:,4)];
    sim = [obs(:,1)];
    sim(:,2) = 0;
end

% get sim data and trim to natural interval...
% sim = handles.plot_data(:,1:2);
i_start = 1;
i_end = size(sim,1);
for i = 1:size(sim,1),
    if isnan(sim(i,2)) == 0; % found 1st non-NaN value from beginning.
        i_start = i;
        break;
    end
end
for i = size(sim,1):-1:1,
    if isnan(sim(i,2)) == 0; % found 1st non-NaN value from end.
        i_end = i;
        break;
    end
end
sim = sim(i_start:i_end,:);

% get obs data and trim to natural interval...
% obs = [handles.plot_data(:,1),handles.plot_data(:,4)];
i_start = 1;
i_end = size(obs,1);
for i = 1:size(obs,1),
    if isnan(obs(i,2)) == 0; % found 1st non-NaN value from beginning.
        i_start = i;
        break;
    end
end
for i = size(obs,1):-1:1,
    if isnan(obs(i,2)) == 0; % found 1st non-NaN value from end.
        i_end = i;
        break;
    end
end
obs = obs(i_start:i_end,:);

% trim sim/obs to their common date/time interval...
[data,i_sim,i_obs] = intersect(sim(:,1),obs(:,1));
data(:,2) = sim(i_sim,2);
data(:,3) = obs(i_obs,2);

% remove any NaN's in simulated data...
temp = data(:,[1 2]);
temp(any(isnan(temp)'),:) = [];
% replace simulated data with nearest-neighbor interpolation using remaining values...
data(:,2) = interp1(temp(:,1),temp(:,2),data(:,1),'nearest','extrap');

% remove any NaN's in observed data...
temp = data(:,[1 3]);
temp(any(isnan(temp)'),:) = [];
% replace observed data with nearest-neighbor interpolation using remaining values...
data(:,3) = interp1(temp(:,1),temp(:,2),data(:,1),'nearest','extrap');

% replace full date with just month...
data(:,1) = str2num(datestr(data(:,1),5));

% find averages across months...
month_avg = zeros(13,3); % initialize averages array.
for i = 1:12, % sum up each month across all years.
    month_avg(i,1) = i;
    month_avg(i,2:3) = mean(data(find(data(:,1)==i),2:3));
end
% find overall averages...
month_avg(13,1) = 13;
month_avg(13,2:3) = mean(data(:,2:3));

% plot results in new window...
h = figure; % create separate figure window.
set(h,'Name','Accumulated Monthly Averages')
bar(month_avg(:,1),month_avg(:,2:3),1);
h = get(gca,'Children');
set(h(1),'FaceColor',[0 0.5 0]); % obs - green.
set(h(2),'FaceColor',[0.8 0 0]); % sim - red.
title('\bfObserved and Simulated Accumulated Monthly Averages');
ylabel(get_label(handles));
set(gca, 'XTick',month_avg(:,1), 'XTickLabel',['Jan';'Feb';'Mar';'Apr';'May';'Jun';'Jul';'Aug';'Sep';'Oct';'Nov';'Dec';'TOT']);
% switch upper(handles.data_type)
%     case {'FLOW','BFLW','QFLW'}
%         legend('Simulated','Observed',2);
%     otherwise
%         legend('Simulated','Time-Weighted Observed',2);
% end
if handles.have_sim & handles.have_obs
    switch upper(handles.data_type)
        case {'FLOW','BFLW','QFLW'}
            legend('Simulated','Observed',2);
        otherwise
            legend('Simulated','Time-Weighted Observed',2);
    end
elseif handles.have_sim
    legend('Simulated',' ',2);
elseif handles.have_obs
    switch upper(handles.data_type)
        case {'FLOW','BFLW','QFLW'}
            legend(' ','Observed',2);
        otherwise
            legend(' ','Time-Weighted Observed',2);
    end
end


%------------------------------------------------------------------------------------------------------
%========================= PRINT CONTROLS =======================
% --- Executes on button press in print_timeseries_pushbutton.
function print_timeseries_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to print_timeseries_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
h = gcf; % save cal5 figure handle.
figure; % create temporary, separate figure window for printing.
axes1 = axes; % create time-series axes on new figure.
axes2 = axes; % create precip axes on new figure.
plot_ts(handles,axes1,axes2);
set(findobj(axes1,'Type','line','Color','r'), 'LineStyle',':'); % set sim line to dotted for print.
% set(findobj(axes1,'Type','line','Color',[0 0.5 0]), 'LineWidth',2.5); % set obs line to heavier width for print.
set(gcf,'PaperOrientation','landscape', 'PaperPosition',[0.5,0.5,10,7.5]); % set up page format: landscape with 0.5inch margins.
print; % print figure.
close(gcf); % close temporary figure.
figure(h); % set cal5 as active figure.

% --- Executes on button press in print_simobs_pushbutton.
function print_simobs_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to print_simobs_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
h = gcf; % save cal5 figure handle.
figure; % create temporary, separate figure window for printing.
axes1 = axes; % create axes on new figure.
axes2 = axes;
plot_sim_obs(handles,axes1,axes2);
set(gcf,'PaperOrientation','portrait', 'PaperPosition',[0.5,1.75,7.5,7.5]); % set up page format: portrait with 7.5inch centered figure.
print; % print figure.
close(gcf); % close temporary figure.
figure(h); % set cal5 as active figure.

% --- Executes on button press in print_cumprob_pushbutton.
function print_cumprob_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to print_cumprob_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
h = gcf; % save cal5 figure handle.
figure; % create temporary, separate figure window for printing.
axes1 = axes; % create time-series axes on new figure.
plot_cum(handles,axes1);
% set(findobj(axes1,'Type','line','Color','r'), 'LineStyle',':'); % set sim line to dashed for print.
set(findobj(axes1,'Type','line','Color',[0 0.5 0]), 'LineWidth',2.5); % set obs line to heavier width for print.
set(gcf,'PaperOrientation','portrait', 'PaperPosition',[0.5,1.75,7.5,7.5]); % set up page format: portrait with 7.5inch centered figure.
print; % print figure.
close(gcf); % close temporary figure.
figure(h); % set cal5 as active figure.

% --- Executes on button press in print_all_pushbutton.
function print_all_pushbutton_Callback(hObject, eventdata, handles)
% hObject    handle to print_all_pushbutton (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% this executes each print push-button in succession...
print_timeseries_pushbutton_Callback(hObject, eventdata, handles);
print_simobs_pushbutton_Callback(hObject, eventdata, handles);
print_cumprob_pushbutton_Callback(hObject, eventdata, handles);


%===================== SAVE PDF FILE ===================
% --- Executes on button press in save_PDF.
function save_PDF_Callback(hObject, eventdata, handles)
% hObject    handle to save_PDF (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% print -dpsc -noui PS.ps;
% print -dpsc -noui -append PSC.ps;
% print -djpeg100 -noui JPEG.jpg
% print -depsc2 -noui -tiff EPSC2.eps
% print -dpdf -noui -r600 PDF.pdf
% print -dtiff -noui -r600 TIF

% X = getframe(gcf);
% if isempty(X.colormap)
%    imwrite(X.cdata,'graphic.bmp')
% else
%    imwrite(X.cdata, X.colormap, 'graphic.tif')
% end
% imwrite(X.cdata,'HDF.hdf', 'Compression','none', 'Quality',100);
% imwrite(X.cdata,'HDF.hdf', 'Compression','none', 'Quality',100, 'WriteMode','append');

% h = figure;
% X = getframe(gcf);
% image(X.cdata);
% set(gca,'XTick',[],'YTick',[]);
% print -dpsc PS.ps;
% close(h);
% h = figure;
% image(X.cdata);
% set(gca,'XTick',[],'YTick',[]);
% print -dpsc -append PS.ps;
% close(h);

uiwait(errordlg('This option is not yet available - no action taken.','Message: Unavailable Option','modal'));


%------------------------------------------------------------------------------------------------------
%============ MISC. UTILITY FUNCTIONS =========================
function label = get_label(handles)
% this function gets the string to use as the x or y axis label.
switch upper(handles.data_type)
    case 'FLOW'
        label = 'TOTAL DISHARGE, IN CFS';
    case 'BFLW'
        label = 'BASE FLOW, IN CFS';
    case 'QFLW'
        label = 'QUICK FLOW, IN CFS';
    case 'TOTN'
        label = 'TOTAL N, IN MG/L';
    case 'ORGN'
        label = 'ORGANIC N, IN MG/L';
    case 'NO3X'
        label = 'NITRATE, IN MG/L';
    case 'NH3X'
        label = 'AMMONIA, IN MG/L';
    case 'TOTP'
        label = 'TOTAL P, IN MG/L';
    case 'ORGP'
        label = 'ORGANIC P, IN MG/L';
    case 'PO4X'
        label = 'PHOSPHATE, IN MG/L';
    case 'TSSX'
        label = 'TOT.SUSP.SEDIMENT, IN MG/L';
    case 'TOCX'
        label = 'TOTAL ORGANIC C, IN MG/L';
    case 'DOXX'
        label = 'DISSOLVED O, IN MG/L';
    case 'CHLA'
        label = 'CHLOROPHYLL, IN UG/L';
    case 'WTMP'
        label = 'WATER TEMPERATURE, IN  ^{o}C';
    case 'SNOW'
        label = 'SNOW DEPTH, IN INCHES';
    otherwise
        label = 'UNKNOWN';
end


%------------------------------------------------------------------------------------------------------
%============ FIGURE CALLBACK FUNCTIONS =========================
% --- Executes during object creation, after setting all properties.
function figure1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
% set(gcbo,'Position',get(0,'ScreenSize')); % resize CAL5 to screen size.

% --- Executes during object deletion, before destroying properties.
function figure1_DeleteFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% --- Executes on mouse press over figure background.
function figure1_ButtonDownFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% --- Executes on mouse press over figure background, over a disabled or
% --- inactive control, or over an axes background.
function figure1_WindowButtonUpFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% --- Executes on mouse press over figure background, over a disabled or
% --- inactive control, or over an axes background.
function figure1_WindowButtonDownFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% --- Executes on mouse motion over figure - except title and menu.
function figure1_WindowButtonMotionFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% --- Executes on key press over figure1 with no controls selected.
function figure1_KeyPressFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% --- Executes when figure1 window is resized.
function figure1_ResizeFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: delete(hObject) closes the figure
delete(hObject);
