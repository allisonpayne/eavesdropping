% Edit these user settings:
csvFile = "/Users/allisonpayne/Local Documents/Github/Manuscripts/eavesdropping/data/SWCombBouts_Validated.csv"; % input CSV
tpwsDir = "/Users/allisonpayne/Local Documents/Github/Manuscripts/eavesdropping/data/matlab/data/tpws-sw";     % .mat directory
saveOut = true;
windowDays = 1;     % moving-median window in days
f_threshold = 10;   % if <1000 treated as kHz (i.e., 10 -> 10000 Hz)

%% Convert threshold to Hz (and keep kHz label)
if f_threshold < 1000
    f_threshold_Hz = double(f_threshold) * 1000;
else
    f_threshold_Hz = double(f_threshold);
end
f_threshold_kHz = f_threshold_Hz / 1000;

%% ---- 1. Read table and filter to species == "SW" ----
T = readtable(csvFile, 'PreserveVariableNames', true, 'TextType', 'string');

if any(strcmpi(T.Properties.VariableNames, 'species'))
    speciesVar = T.(T.Properties.VariableNames{strcmpi(T.Properties.VariableNames, 'species')});
    keepRows = strcmpi(strtrim(speciesVar), 'SW');
    nBefore = height(T);
    T = T(keepRows, :);
    fprintf('Filtered CSV: kept %d of %d rows where species == "SW".\n', height(T), nBefore);
else
    warning('No "species" column found in CSV. Using all rows.');
end

%% ---- 1b. Robust ISO-8601 timestamp parsing (handles fractional seconds + Z) ----
clean_iso = @(s) regexprep(string(s), '\.\d+Z$', 'Z');  % strip variable fractional secs
parse_iso_utc = @(s) datetime(s, 'InputFormat', "yyyy-MM-dd'T'HH:mm:ss'Z'", 'TimeZone', 'UTC');

vars = T.Properties.VariableNames;
hasStart = any(strcmpi(vars,'start'));
hasEnd   = any(strcmpi(vars,'end'));

if hasStart && hasEnd
    rawStart = T.(vars{find(strcmpi(vars,'start'),1)});
    rawEnd   = T.(vars{find(strcmpi(vars,'end'),1)});
    try
        startTimes = parse_iso_utc( clean_iso(rawStart) );
        endTimes   = parse_iso_utc( clean_iso(rawEnd) );
    catch
        startTimes = datetime(string(rawStart), 'TimeZone', 'UTC');
        endTimes   = datetime(string(rawEnd), 'TimeZone', 'UTC');
    end
else
    rawTime = T.(1); % fallback: first column
    try
        startTimes = parse_iso_utc( clean_iso(rawTime) );
    catch
        try
            startTimes = datetime(string(rawTime), 'TimeZone','UTC');
        catch
            error('Failed to parse timestamps in the first column. Inspect your CSV format.');
        end
    end
    endTimes = startTimes + minutes(5) - seconds(1);
end

% ensure vectors and sort (keep endTimes aligned)
startTimes = startTimes(:);
endTimes   = endTimes(:);
[ startTimes, sidx ] = sort(startTimes);
endTimes = endTimes(sidx);

%% ---- 2. timezone-safe initialization of boutStart / boutEnd ----
if exist('startTimes','var') && isdatetime(startTimes)
    boutStart = startTimes([]);  % 0x1 datetime with same timezone
    if exist('endTimes','var') && isdatetime(endTimes)
        boutEnd = endTimes([]);  % preserve timezone
    else
        boutEnd = startTimes([]);
    end
else
    boutStart = datetime.empty;
    boutEnd   = datetime.empty;
end

MTT_dt = datetime(MTT_num, 'ConvertFrom','datenum', 'TimeZone','UTC');


%% ---- 3. Merge consecutive 5-min bins into bouts ----
if isempty(startTimes)
    error('No start times found in CSV.');
end

currStart = startTimes(1);
currEnd   = endTimes(1);
for i = 2:numel(startTimes)
    if startTimes(i) - startTimes(i-1) <= minutes(5)
        currEnd = endTimes(i);
    else
        boutStart(end+1,1) = currStart; %#ok<SAGROW>
        boutEnd(end+1,1)   = currEnd;   %#ok<SAGROW>
        currStart = startTimes(i);
        currEnd   = endTimes(i);
    end
end
% append final bout
boutStart(end+1,1) = currStart;
boutEnd(end+1,1)   = currEnd;

nBouts = numel(boutStart);
fprintf('Created %d merged bouts from %d input rows.\n', nBouts, numel(startTimes));
boutStartNum = datenum(boutStart);
boutEndNum   = datenum(boutEnd);

%% ---- 4. Load TPWS .mat files (MTT, MPP, MSP, f) ----
fprintf('Loading TPWS data from %s ...\n', tpwsDir);
fList = dir(fullfile(tpwsDir, '*.mat'));

MTT_all = [];
MPP_all = [];
MSP_rows = {};   % each cell is a row vector for one detection
f_global = [];
f_inconsistent = false;

for fi = 1:numel(fList)
    fname = fullfile(fList(fi).folder, fList(fi).name);
    S = load(fname);

    if ~isfield(S,'MTT')
        warning('File %s missing MTT; skipping.', fList(fi).name);
        continue;
    end

    local_MTT = S.MTT(:);
    nLocal = numel(local_MTT);

    if isfield(S,'MPP')
        local_MPP = S.MPP(:);
        if numel(local_MPP) ~= nLocal
            warning('File %s: MTT and MPP lengths differ; padding/truncating.', fList(fi).name);
            local_MPP = local_MPP(1:min(end,nLocal));
            if numel(local_MPP) < nLocal, local_MPP(end+1:nLocal,1) = NaN; end
        end
    else
        local_MPP = NaN(nLocal,1);
    end

    % Normalize MSP rows to 1 x nCols per detection
    if isfield(S,'MSP')
        MSP_local = double(S.MSP);
        if size(MSP_local,1) == nLocal
            for r = 1:nLocal, MSP_rows{end+1,1} = MSP_local(r,:); end
        elseif size(MSP_local,2) == nLocal
            MSP_local = MSP_local';
            for r = 1:nLocal, MSP_rows{end+1,1} = MSP_local(r,:); end
        elseif numel(MSP_local) == nLocal * 141
            MSP_local = reshape(MSP_local, [nLocal, 141]);
            for r = 1:nLocal, MSP_rows{end+1,1} = MSP_local(r,:); end
        else
            warning('File %s: MSP has unexpected shape; inserting NaN rows for these detections.', fList(fi).name);
            for r = 1:nLocal, MSP_rows{end+1,1} = NaN(1,141); end
        end
    else
        for r = 1:nLocal, MSP_rows{end+1,1} = NaN(1,141); end
    end

    % f vector
    if isfield(S,'f')
        f_here = double(S.f(:))';
        if isempty(f_global)
            f_global = f_here;
        else
            if numel(f_here) ~= numel(f_global) || any(abs(f_here - f_global) > 1e-6)
                warning('f differs in file %s. Marking as inconsistent.', fList(fi).name);
                f_inconsistent = true;
            end
        end
    end

    % append MTT and MPP
    MTT_all = [MTT_all; local_MTT(:)];
    MPP_all = [MPP_all; local_MPP(:)];
end

% convert MSP_rows to matrix
if ~isempty(MSP_rows)
    MSP_all = cell2mat(MSP_rows); % N x nCols
else
    MSP_all = zeros(0,141);
end

tpwsData = [MTT_all(:), MPP_all(:)];
fprintf('Loaded %d TPWS detections from %d files.\n', size(tpwsData,1), numel(fList));
if f_inconsistent, warning('Frequency vector f inconsistent across files. Inspect f_global.'); end
if isempty(f_global), warning('No f vector found in .mat files; fallback mask will be columns 1:12.'); end

%% ---- 5. Prepare detection times numeric for matching ----
if ~isempty(tpwsData)
    if isdatetime(tpwsData(1,1))
        MTT_num = datenum(tpwsData(:,1));
    else
        MTT_num = double(tpwsData(:,1));
    end
else
    MTT_num = [];
end

%% ---- 6–9: ensure units, filter MSP, compute per-bout maxima, diagnostics ----
% Ensure MSP_all is N_detections x N_freqs
if isempty(MSP_all)
    error('MSP_all is empty. Check earlier loading steps.');
end

% If f_global is empty, try to load from files or error
if isempty(f_global)
    warning('No f vector found in .mat files; defaulting mask to 1:12 (assumes 12 cols under threshold).');
    f_global = (1:size(MSP_all,2))'; % pseudo vector so size checks work
    f_units_assumed = 'unknown';
else
    % convert to column vector
    f_global = double(f_global(:));
end

% Ensure MSP orientation: expect size(MSP_all) => [N_detections x N_freqs]
[nRows, nCols] = size(MSP_all);
if nCols == numel(f_global) && nRows ~= numel(f_global)
    % Good: MSP_all is already N_detections x N_freqs
elseif nRows == numel(f_global) && nCols ~= numel(f_global)
    % MSP_all is transposed (freqs x detections) -> transpose
    MSP_all = MSP_all';
    [nRows, nCols] = size(MSP_all);
    fprintf('Transposed MSP_all so shape is %d x %d (detections x freqs).\n', nRows, nCols);
else
    % ambiguous orientation: try to coerce so columns match f length
    if nCols == numel(f_global)
        % ok
    elseif nRows == numel(f_global)
        MSP_all = MSP_all';
        [nRows, nCols] = size(MSP_all);
        fprintf('Transposed MSP_all (ambiguous case). New shape: %d x %d.\n', nRows, nCols);
    else
        warning('MSP_all dimensions (%d x %d) do not match f length (%d). Proceeding but results may be wrong.', nRows, nCols, numel(f_global));
    end
end

% --- Detect units of f_global and convert to Hz if needed ---
% Heuristic: if max(f_global) < 1000 -> values are likely kHz; convert to Hz.
if max(f_global) < 1000
    fprintf('Detected f vector with max value < 1000 -> assuming units are kHz. Converting f to Hz.\n');
    f_global = f_global * 1000;  % now in Hz
    f_units_assumed = 'kHz->converted to Hz';
else
    f_units_assumed = 'Hz';
end

% Recompute nCols after possible conversion
nFreqs = numel(f_global);

% Make sure MSP_all has the same number of frequency columns as f_global
if size(MSP_all,2) ~= nFreqs
    % If shapes still mismatch, try to trim/pad MSP to match f_global length:
    if size(MSP_all,2) > nFreqs
        MSP_all = MSP_all(:, 1:nFreqs);
        fprintf('Trimmed MSP_all columns to match f length (%d cols).\n', nFreqs);
    elseif size(MSP_all,2) < nFreqs
        % pad with NaNs
        MSP_all(:,end+1:nFreqs) = NaN;
        fprintf('Padded MSP_all columns with NaN to match f length (%d cols).\n', nFreqs);
    end
end

% Convert threshold to Hz earlier; ensure it exists
if ~exist('f_threshold_Hz','var')
    if exist('f_threshold','var') && f_threshold < 1000
        f_threshold_Hz = double(f_threshold) * 1000;
    else
        f_threshold_Hz = double(f_threshold);
    end
end

% Build mask: keep frequencies strictly below threshold
maskCols = find(f_global < f_threshold_Hz);

% Fallback if mask is empty
if isempty(maskCols)
    warning('maskCols is empty for threshold %.2f Hz — using full freq range as fallback.', f_threshold_Hz);
    maskCols = 1:numel(f_global);
end

% Diagnostics about mask
fprintf('f units assumed: %s. f range: [%.1f, %.1f] (Hz). Keeping %d of %d columns (f < %.1f Hz).\n', ...
    f_units_assumed, min(f_global), max(f_global), numel(maskCols), numel(f_global), f_threshold_Hz);

% Subset MSP and f
MSP_sub = MSP_all(:, maskCols);         % N_detections x N_masked_freqs
f_sub   = f_global(maskCols);           % in Hz

% ---- compute per-bout maxima and freq ----
if isempty(MTT_num)
    error('No TPWS timestamps found; cannot compute per-bout maxima.');
end

boutMaxMSP = NaN(nBouts,1);
boutMaxFreq_Hz = NaN(nBouts,1);
boutMaxDetIdx = NaN(nBouts,1); % index of detection (row in MSP_sub)
boutMaxColIdx = NaN(nBouts,1); % column (index in maskCols)

for b = 1:nBouts
    inBout = MTT_num >= boutStartNum(b) & MTT_num <= boutEndNum(b);
    detIdx = find(inBout);
    if isempty(detIdx), continue; end

    MSP_this = MSP_sub(detIdx, :); % rows = detections in bout
    % find maximum across all rows/cols (omit NaN)
    [maxRowVals, colIdxs] = max(MSP_this, [], 2, 'omitnan'); % per-row maxima
    [maxVal, rowOfMax] = max(maxRowVals, [], 'omitnan');     % overall max and which row
    if isempty(maxVal) || isnan(maxVal)
        continue;
    end
    colOfMax = colIdxs(rowOfMax);         % column index within MSP_this (and MSP_sub)
    boutMaxMSP(b) = maxVal;
    boutMaxFreq_Hz(b) = f_sub(colOfMax);
    boutMaxDetIdx(b) = detIdx(rowOfMax); % global detection row index
    boutMaxColIdx(b) = maskCols(colOfMax); % original column in MSP_all / f_global
end

% ---- Early diagnostic: any bout with freq above threshold? ----
offending = find(boutMaxFreq_Hz > f_threshold_Hz);
if ~isempty(offending)
    fprintf('WARNING: %d bouts have max frequency above threshold (%.1f Hz). Listing examples:\n', numel(offending), f_threshold_Hz);
    nshow = min(10, numel(offending));
    for k = 1:nshow
        b = offending(k);
        fprintf('  Bout %d: start %s end %s -> maxFreq %.1f Hz (det idx %d, f col %d), maxMSP %.3f\n', ...
            b, string(boutStart(b)), string(boutEnd(b)), boutMaxFreq_Hz(b), boutMaxDetIdx(b), boutMaxColIdx(b), boutMaxMSP(b));
    end
    if numel(offending) > nshow, fprintf('  ...and %d more.\n', numel(offending)-nshow); end
else
    fprintf('All bout maxima are at or below threshold (%.1f Hz).\n', f_threshold_Hz);
end

% ---- Plot diagnostics (frequency in Hz now) ----
figure('Name','Bout MSP Diagnostics','Color','w');
subplot(2,1,1);
plot(boutStart, boutMaxMSP, 'o-');
ylabel('Max MSP value');
xlabel('Bout start time (UTC)');
title('Per-bout maximum MSP (below threshold)');

subplot(2,1,2);
plot(boutStart, boutMaxFreq_Hz, 'o-');
ylabel('Frequency (Hz)');
xlabel('Bout start time (UTC)');
title('Frequency of max MSP per bout');

% ---- Console diagnostics table (freq in Hz) ----
fprintf('\n=== Per-bout MSP Summary (freq in Hz; threshold = %.1f Hz) ===\n', f_threshold_Hz);
fprintf('%-4s %-25s %-25s %-12s %-12s\n', 'Idx', 'Bout Start (UTC)', 'Bout End (UTC)', 'Max MSP', 'Freq (Hz)');
fprintf('%s\n', repmat('-',1,100));
for b = 1:nBouts
    if isnan(boutMaxMSP(b)), continue; end
    fprintf('%-4d %-25s %-25s %-12.3f %-12.0f\n', ...
        b, string(boutStart(b)), string(boutEnd(b)), ...
        boutMaxMSP(b), boutMaxFreq_Hz(b));
end
fprintf('%s\n', repmat('-',1,100));
fprintf('Printed %d bouts with valid max MSP values.\n', sum(~isnan(boutMaxMSP)));


%% === 10. Plot MPP progression throughout each bout and save ===

outDir = "C:\Users\mg2377\Documents\tag_spermwhale_bout";
if ~exist(outDir, 'dir')
    mkdir(outDir);
end

fprintf('\nSaving per-bout MPP plots to:\n  %s\n', outDir);

for b = 1:nBouts

    % Find detections inside this bout
    inBout = MTT_num >= boutStartNum(b) & MTT_num <= boutEndNum(b);
    detIdx = find(inBout);
    if isempty(detIdx)
        continue; 
    end

    % Time since bout start (minutes)
    t_rel_min = minutes(MTT_dt(detIdx) - boutStart(b));

    % Extract MPP values for these detections
    MPP_this = MPP_all(detIdx);

    % Make figure
    fig = figure('Visible','off','Color','w');
    plot(t_rel_min, MPP_this, 'o-','LineWidth',1.3);
    xlabel('Minutes since bout start');
    ylabel('MPP (dB re 1 µPa?)');   % adjust label if needed
    title(sprintf('Bout %d: MPP progression\n%s → %s', ...
        b, string(boutStart(b)), string(boutEnd(b))));
    grid on;

    % Build filename
    fname = fullfile(outDir, sprintf('bout_%03d_MPP.png', b));

    % Save
    exportgraphics(fig, fname, 'Resolution', 300);

    close(fig);
end

fprintf('Finished generating and saving all MPP plots.\n');

