%% ---- USER SETTINGS ----
%% ---- 1. Parse 5-min bin CSV ----
csvFile = "/Users/allisonpayne/Local Documents/Github/Manuscripts/eavesdropping/data/SWCombBouts_Validated.csv"; % path to input file
tpwsDir = "/Users/allisonpayne/Local Documents/Github/Manuscripts/eavesdropping/data/matlab/SW_types_all.mat";
saveOut = true;

% Read file (assumes first column contains datetime-like entries)
T = readtable(csvFile,'PreserveVariableNames',true);
timeVar = T.(1);  % first column

% Ensure it's datetime
if ~isdatetime(timeVar)
    timeVar = datetime(timeVar, 'InputFormat', 'MM/dd/yyyy HH:mm', 'TimeZone', 'UTC');
end

% Sort just in case
timeVar = sort(timeVar);

% Create Start and End times (each bin = 5 min duration)
startTimes = timeVar;
endTimes = timeVar + minutes(5) - seconds(1);

%% ---- Merge consecutive bins into single bouts ----
% If the next start time is exactly 5 min after the previous, merge
boutStart = datetime.empty;
boutEnd   = datetime.empty;

currStart = startTimes(1);
currEnd   = endTimes(1);

for i = 2:numel(startTimes)
    if timeVar(i) - timeVar(i-1) <= minutes(5)
        % Consecutive bin: extend current bout
        currEnd = endTimes(i);
    else
        % Non-consecutive: save and start new bout
        boutStart(end+1,1) = currStart;
        boutEnd(end+1,1)   = currEnd;
        currStart = startTimes(i);
        currEnd   = endTimes(i);
    end
end

% Append final bout
boutStart(end+1,1) = currStart;
boutEnd(end+1,1)   = currEnd;

nBouts = numel(boutStart);
fprintf('Created %d merged bouts from %d bins.\n', nBouts, numel(startTimes));

% For downstream compatibility
boutStartNum = datenum(boutStart);
boutEndNum   = datenum(boutEnd);


%% ---- 2. Load TPWS files (cached) ----
fprintf('Loading TPWS data from %s ...\n', tpwsDir);
fList = dir(fullfile(tpwsDir, '*.mat'));

tpwsData = cell(numel(fList),1);  % each cell will hold [MTT, MPP]

for fi = 1:numel(fList)
    fname = fullfile(fList(fi).folder, fList(fi).name);
    S = load(fname);

    if isfield(S,'MTT') && isfield(S,'MPP')
        MTT = S.MTT(:);
        MPP = S.MPP(:);

        if numel(MTT) ~= numel(MPP)
            warning('File %s: MTT and MPP lengths differ. Skipping this file.', fList(fi).name);
            tpwsData{fi} = [];
        else
            tpwsData{fi} = [MTT, MPP];  % store as 2-column matrix
        end
    else
        warning('File %s missing MTT or MPP variable', fList(fi).name);
        tpwsData{fi} = [];
    end
end

% Concatenate all files into one big [N x 2] matrix
tpwsData = vertcat(tpwsData{:});

fprintf('Loaded %d total TPWS detections with MPP values\n', size(tpwsData,1));

%% ---- 3. Match bouts to TPWS ----
matches = cell(nBouts,1);
for bi = 1:nBouts
    if isnan(boutStartNum(bi)) || isnan(boutEndNum(bi))
        matches{bi} = [];
        continue;
    end
    
    % Index detections within the bout using column 1 (MTT times)
    idx = tpwsData(:,1) >= boutStartNum(bi) & tpwsData(:,1) <= boutEndNum(bi);
    
    % Keep both columns: [MTT, MPP]
    matches{bi} = tpwsData(idx,:);
end

%% ---- 4. Report ----
for bi = 1:nBouts
    fprintf('Bout %2d: %s -> %s | %d detections\n', ...
        bi, string(boutStart(bi)), string(boutEnd(bi)), numel(matches{bi}));
end

%% You have the vector of MPPs for each bout. Now, let's calculate the median!

medianMPP = nan(nBouts,1);
for bi = 1:nBouts
    if ~isempty(matches{bi})
        medianMPP(bi) = median(matches{bi}(:,2), 'omitnan');
    end
end

%% Great! Now, let's export the general information to a CSV.
% Preallocate summary values
medianMPP     = nan(nBouts,1);
numDetections = zeros(nBouts,1);
boutDurMin    = minutes(boutEnd - boutStart);  % duration in minutes

% Loop through bouts
for bi = 1:nBouts
    if ~isempty(matches{bi})
        medianMPP(bi)     = median(matches{bi}(:,2), 'omitnan');
        numDetections(bi) = size(matches{bi},1);
    end
end

% Build summary table
summaryTbl = table(boutStart, boutEnd, boutDurMin, numDetections, medianMPP, ...
    'VariableNames', {'BoutStart','BoutEnd','BoutDuration_min','NumDetections','MedianMPP'});
%%
% Write to CSV
outFile = fullfile(tpwsDir, 'bout_median_MPP_summary.csv');
writetable(summaryTbl, outFile);

fprintf('Wrote per-bout summary with duration, detections, and median MPP to %s\n', outFile);

%% But now, let's lock in on which bouts we specifically want to evaluate for buzzes. 
% Let's peek at what the top 10% MPP is for bouts.

% Calculate 90th percentile of median MPP (top 10%)
cutoff = prctile(summaryTbl.MedianMPP, 75);

% Find bouts in the top 10% of median MPP
isTop10 = summaryTbl.MedianMPP >= cutoff;

% Extract those bouts
topBouts = summaryTbl(isTop10, :);

% Display results
fprintf('90th percentile of median MPP = %.2f\n', cutoff);
disp(topBouts);
%%
% Write to CSV
outFile = fullfile(tpwsDir, 'top_10_bouts_mpp.csv');
writetable(topBouts, outFile);

%% Now, let's explore how length affects these measurements.

% Calculate 90th percentile of median MPP (top 10%)
cutoff = prctile(summaryTbl.BoutDuration_min, 80);

% Find bouts in the top 10% of median MPP
isTop10 = summaryTbl.BoutDuration_min >= cutoff;

% Extract those bouts
longBouts = summaryTbl(isTop10, :);

% Display results
fprintf('90th percentile of bout duration = %.2f\n', cutoff);
disp(topBouts);

%% Let's explore length constrained to the top 25% loudest bouts.

% Calculate 90th percentile of median MPP (top 10%)
cutoff = prctile(topBouts.BoutDuration_min, 70);

% Find bouts in the top 10% of median MPP
isTop10 = topBouts.BoutDuration_min >= cutoff;

% Extract those bouts
longloudBouts = topBouts(isTop10, :);

% Display results
fprintf('90th percentile of bout duration = %.2f\n', cutoff);
disp(topBouts);


%% ---- 5. Find bouts with median MPP above a set threshold ----
threshold = 9;                     % set threshold here
% logical index: true when medianMPP is a finite number and > threshold
highIdx = ~isnan(boutDurMin) & (boutDurMin > threshold);

nHigh = sum(highIdx);
if nHigh == 0
    fprintf('No bouts found with medianMPP > %g.\n', threshold);
else
    fprintf('Found %d bouts with medianMPP > %g:\n', nHigh, threshold);
    % Print bout number, start, end, duration, medianMPP
    for k = find(highIdx).'   % row vector
        fprintf('  Bout %2d : %s -> %s | Dur (min)=%.2f | MedianMPP=%.3f\n', ...
            k, string(summaryTbl.BoutStart(k)), string(summaryTbl.BoutEnd(k)), ...
            summaryTbl.BoutDuration_min(k), summaryTbl.MedianMPP(k));
    end

    % make a table of only the high-MPP bouts and save it
    highBoutsTbl = summaryTbl(highIdx,:);
    outHighFile = fullfile(tpwsDir, sprintf('bout_median_MPP_above_%d.csv', threshold));
    writetable(highBoutsTbl, outHighFile);
    fprintf('Wrote %d high-MPP bouts to %s\n', nHigh, outHighFile);
end

%% ---- 5. Find bouts with median MPP above a set threshold ----
threshold = 111.5;                     % set threshold here
% logical index: true when medianMPP is a finite number and > threshold
highIdx = ~isnan(medianMPP) & (medianMPP > threshold);

nHigh = sum(highIdx);
if nHigh == 0
    fprintf('No bouts found with medianMPP > %g.\n', threshold);
else
    fprintf('Found %d bouts with medianMPP > %g:\n', nHigh, threshold);
    % Print bout number, start, end, duration, medianMPP
    for k = find(highIdx).'   % row vector
        fprintf('  Bout %2d : %s -> %s | Dur (min)=%.2f | MedianMPP=%.3f\n', ...
            k, string(summaryTbl.BoutStart(k)), string(summaryTbl.BoutEnd(k)), ...
            summaryTbl.BoutDuration_min(k), summaryTbl.MedianMPP(k));
    end

    % make a table of only the high-MPP bouts and save it
    highBoutsTbl = summaryTbl(highIdx,:);
    outHighFile = fullfile(tpwsDir, sprintf('bout_median_MPP_above_%d.csv', threshold));
    writetable(highBoutsTbl, outHighFile);
    fprintf('Wrote %d high-MPP bouts to %s\n', nHigh, outHighFile);
end
%% 
% Find bouts longer than 17 minutes
longBouts = highBoutsTbl(highBoutsTbl.BoutDuration_min > 17, :);

% Display them
disp(longBouts)
%%
writetable(longBouts, 'C:\Users\mg2377\Documents\Oregon\emmaThesisBouts.csv');

%%
threshold = 17;                     % set threshold here
% logical index: true when medianMPP is a finite number and > threshold
highIdx = ~isnan(boutDurMin) & (boutDurMin > threshold);

nHigh = sum(highIdx);
if nHigh == 0
    fprintf('No bouts found with medianMPP > %g.\n', threshold);
else
    fprintf('Found %d bouts with medianMPP > %g:\n', nHigh, threshold);
    % Print bout number, start, end, duration, medianMPP
    for k = find(highIdx).'   % row vector
        fprintf('  Bout %2d : %s -> %s | Dur (min)=%.2f | MedianMPP=%.3f\n', ...
            k, string(summaryTbl.BoutStart(k)), string(summaryTbl.BoutEnd(k)), ...
            summaryTbl.BoutDuration_min(k), summaryTbl.MedianMPP(k));
    end

    % make a table of only the high-MPP bouts and save it
    highBoutsTbl = summaryTbl(highIdx,:);
    outHighFile = fullfile(tpwsDir, sprintf('bout_median_MPP_above_%d.csv', threshold));
    writetable(highBoutsTbl, outHighFile);
    fprintf('Wrote %d high-MPP bouts to %s\n', nHigh, outHighFile);
end
%% ---- 6. Lowest medianMPP among top percentile bouts ----
% You can use this value to inform whether you want to set the "set
% threshold" higher. 
if ~isempty(topBouts)
    minHigh = min(topBouts.MedianMPP, [], 'omitnan');
    fprintf('Lowest MedianMPP among top bouts = %.3f\n', minHigh);
else
    fprintf('No high-MPP bouts found, so no minimum value.\n');
end