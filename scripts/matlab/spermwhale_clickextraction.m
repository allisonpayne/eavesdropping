%% process_SW_clicks.m
% =========================================================================
% Processes sperm whale click detections from TPWS (.mat) files.
%
% For each DB## folder:
%   - Reads all .tpws files (MATLAB .mat format)
%   - Extracts MTT (timestamps, MATLAB datenum), MPP, MSN, MSP, and f
%   - Filters detections to those within validated SW episode windows
%   - Computes peak frequency per click (argmax of MSP row -> f value)
%   - Outputs one CSV per DB scenario: timestamp, MPP, MSN, peak_freq_Hz, episode_id
%
% Also outputs: SWClicks_Summary_ClicksPerEpisode.csv
%   - Rows = episodes, columns = DB scenarios (click counts)
%
% INPUT FILES:
%   - SWCombBouts_Validated.csv  (columns: start, start_ltsa, end_ltsa, end, notes)
%   - DB##/ folders containing *.tpws files
%
% =========================================================================

clear; clc;

%% -------------------------------------------------------------------------
% USER CONFIGURATION
% -------------------------------------------------------------------------
baseDir      = 'C:\Users\mg2377\Documents\Allison_SW_ReceivedLevel';
validatedCSV = fullfile(baseDir, 'SWCombBouts_Validated.csv');
outputDir    = fullfile(baseDir, 'Output_ClickAnalysis');

if ~exist(outputDir, 'dir')
    mkdir(outputDir);
end

%% -------------------------------------------------------------------------
% LOAD VALIDATED EPISODE WINDOWS
% Use textscan to avoid conflict with MATLAB reserved keyword 'end'.
% CSV columns: start(1), start_ltsa(2), end_ltsa(3), end(4), notes(5)
% We only use columns 1 (start) and 4 (end).
% -------------------------------------------------------------------------
fprintf('Loading validated episodes from CSV...\n');

fid = fopen(validatedCSV, 'r');
raw = textscan(fid, '%s%s%s%s%[^\n]', 'Delimiter', ',', 'HeaderLines', 1);
fclose(fid);

startTimes = raw{1};  % 'start' column
endTimes   = raw{4};  % 'end' column

nEpisodes = numel(startTimes);
ep_starts = zeros(nEpisodes, 1);
ep_ends   = zeros(nEpisodes, 1);
ep_ids    = cell(nEpisodes, 1);

for i = 1:nEpisodes
    startStr     = strtrim(startTimes{i});
    endStr       = strtrim(endTimes{i});
    ep_starts(i) = datenum(startStr, 'yyyy-mm-dd HH:MM:SS');
    ep_ends(i)   = datenum(endStr,   'yyyy-mm-dd HH:MM:SS');
    ep_ids{i}    = sprintf('Episode_%02d', i);
end

fprintf('  Loaded %d validated episodes.\n\n', nEpisodes);

%% -------------------------------------------------------------------------
% FIND DB FOLDERS
% -------------------------------------------------------------------------
dbList = dir(fullfile(baseDir, 'DB*'));
dbList = dbList([dbList.isdir]);

if isempty(dbList)
    error('No DB## folders found in: %s', baseDir);
end

fprintf('Found %d DB folders:\n', numel(dbList));
for k = 1:numel(dbList)
    fprintf('  %s\n', dbList(k).name);
end
fprintf('\n');

%% -------------------------------------------------------------------------
% SUMMARY TABLE SETUP
% -------------------------------------------------------------------------
summaryClickCounts = zeros(nEpisodes, numel(dbList));
summaryLowMPP      = zeros(nEpisodes, numel(dbList));  % clicks with MPP < 30
dbNames            = {dbList.name};

%% -------------------------------------------------------------------------
% PROCESS EACH DB FOLDER
% -------------------------------------------------------------------------
for k = 1:numel(dbList)

    dbName   = dbList(k).name;
    dbFolder = fullfile(baseDir, dbName);

    fprintf('============================================================\n');
    fprintf('Processing: %s\n', dbName);
    fprintf('============================================================\n');

    % --- Find all .tpws files in this folder ---
    tpwsFiles = dir(fullfile(dbFolder, '*_TPWS*.mat'));

    if isempty(tpwsFiles)
        fprintf('  [WARN] No .tpws files found. Skipping.\n\n');
        continue;
    end

    fprintf('  Found %d .tpws file(s).\n', numel(tpwsFiles));

    % --- Load and concatenate all .tpws files ---
    MTT_all = [];
    MPP_all = [];
    MSN_all = [];
    MSP_all = [];
    f_ref   = [];

    for j = 1:numel(tpwsFiles)
        fpath = fullfile(dbFolder, tpwsFiles(j).name);
        fprintf('  Loading: %s\n', tpwsFiles(j).name);

        try
            data = load(fpath, '-mat');
        catch ME
            fprintf('  [ERROR] Could not load file: %s\n', ME.message);
            continue;
        end

        MTT_cur = double(data.MTT(:));
        MPP_cur = double(data.MPP(:));
        MSN_cur = double(data.MSN(:));
        MSP_cur = double(data.MSP);
        f_cur   = double(data.f(:));

        % Ensure MSP is [n_clicks x n_freqs] (not transposed)
        if size(MSP_cur, 1) == numel(f_cur) && size(MSP_cur, 2) == numel(MTT_cur)
            MSP_cur = MSP_cur';
        end

        if isempty(f_ref)
            f_ref = f_cur;
        end

        MTT_all = [MTT_all; MTT_cur];  %#ok<AGROW>
        MPP_all = [MPP_all; MPP_cur];  %#ok<AGROW>
        MSN_all = [MSN_all; MSN_cur];  %#ok<AGROW>
        MSP_all = [MSP_all; MSP_cur];  %#ok<AGROW>
    end

    if isempty(MTT_all)
        fprintf('  [WARN] No data loaded for %s. Skipping.\n\n', dbName);
        continue;
    end

    % Sort all detections by time
    [MTT_all, sortIdx] = sort(MTT_all);
    MPP_all = MPP_all(sortIdx);
    MSN_all = MSN_all(sortIdx);
    MSP_all = MSP_all(sortIdx, :);

    fprintf('  Total detections loaded: %d\n', numel(MTT_all));

    % --- Filter to detections within validated episode windows ---
    keepMask  = false(numel(MTT_all), 1);
    ep_assign = zeros(numel(MTT_all), 1);

    for i = 1:nEpisodes
        inEp = (MTT_all >= ep_starts(i)) & (MTT_all <= ep_ends(i));
        keepMask(inEp)  = true;
        ep_assign(inEp) = i;
    end

    idx_keep = find(keepMask);
    fprintf('  Detections within validated episodes: %d\n', numel(idx_keep));

    if isempty(idx_keep)
        fprintf('  [WARN] No detections matched any episode window.\n\n');
        continue;
    end

    % --- Extract filtered data ---
    MTT_keep = MTT_all(idx_keep);
    MPP_keep = MPP_all(idx_keep);
    MSN_keep = MSN_all(idx_keep);
    MSP_keep = MSP_all(idx_keep, :);
    ep_idx   = ep_assign(idx_keep);

    % --- Compute peak frequency for each click ---
    nKeep     = numel(idx_keep);
    peak_freq = zeros(nKeep, 1);

    for n = 1:nKeep
        [~, peakBin] = max(MSP_keep(n, :));
        peak_freq(n) = f_ref(peakBin);
    end

    % --- Convert MTT datenums to datetime strings for output ---
    timestamp_str = cellstr(datestr(MTT_keep, 'yyyy-mm-dd HH:MM:SS.FFF'));

    % --- Assign episode ID strings ---
    ep_id_str = ep_ids(ep_idx);

    % --- Build and write output CSV ---
    outTable = table(timestamp_str, MTT_keep, MPP_keep, peak_freq, ep_id_str, ...
        'VariableNames', {'timestamp_UTC', 'MTT_datenum', 'MPP_dB', 'peak_freq_Hz', 'episode_id'});

    outFile = fullfile(outputDir, sprintf('%s_clicks.csv', dbName));
    writetable(outTable, outFile);
    fprintf('  Saved: %s\n', outFile);

    % --- Tally clicks per episode for summary ---
    for i = 1:nEpisodes
        summaryClickCounts(i, k) = sum(ep_idx == i);
        summaryLowMPP(i, k)      = sum(ep_idx == i & MPP_keep < 30);
    end

    fprintf('  Clicks per episode:\n');
    for i = 1:nEpisodes
        if summaryClickCounts(i, k) > 0
            fprintf('    %s: %d\n', ep_ids{i}, summaryClickCounts(i, k));
        end
    end
    fprintf('\n');

end

%% -------------------------------------------------------------------------
% WRITE SUMMARY CSV: clicks per episode x DB scenario
% -------------------------------------------------------------------------
fprintf('============================================================\n');
fprintf('Writing summary table...\n');

summaryTable = table(ep_ids, 'VariableNames', {'episode_id'});

for k = 1:numel(dbList)
    colName = matlab.lang.makeValidName(dbNames{k});
    summaryTable.(colName) = summaryClickCounts(:, k);
end

summaryTable.episode_start = cellstr(datestr(ep_starts, 'yyyy-mm-dd HH:MM:SS'));
summaryTable.episode_end   = cellstr(datestr(ep_ends,   'yyyy-mm-dd HH:MM:SS'));

summaryFile = fullfile(outputDir, 'SWClicks_Summary_ClicksPerEpisode.csv');
writetable(summaryTable, summaryFile);
fprintf('Saved summary: %s\n\n', summaryFile);


%% -------------------------------------------------------------------------
% WRITE SUMMARY CSV: clicks with MPP < 30 per episode x DB scenario
% -------------------------------------------------------------------------
fprintf('Writing low-MPP summary table...\n');

lowMPPTable = table(ep_ids, 'VariableNames', {'episode_id'});

for k = 1:numel(dbList)
    colName = matlab.lang.makeValidName(dbNames{k});
    lowMPPTable.(colName) = summaryLowMPP(:, k);
end

lowMPPTable.episode_start = cellstr(datestr(ep_starts, 'yyyy-mm-dd HH:MM:SS'));
lowMPPTable.episode_end   = cellstr(datestr(ep_ends,   'yyyy-mm-dd HH:MM:SS'));

lowMPPFile = fullfile(outputDir, 'SWClicks_Summary_LowMPP_below30.csv');
writetable(lowMPPTable, lowMPPFile);
fprintf('Saved low-MPP summary: %s\n\n', lowMPPFile);

fprintf('Done! All outputs written to:\n  %s\n', outputDir);