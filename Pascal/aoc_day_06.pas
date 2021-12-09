program AoCDay06;

uses SysUtils, StrUtils;

type
    IntegerArray = array of Integer;
    StringArray = array of String;
    Cohorts = array [0..8] of Int64;

function SplitDelimited(text: String): StringArray;
var
    delims: SysUtils.TSysCharSet;
    wc: Integer;
    i: Integer;
    word: String;
begin
    delims := [',', ' '];
    wc := WordCount(text, delims);
    SetLength(result, wc);
    for i := 1 to wc do
    begin
        word := ExtractWord(i, text, delims);
        result[i-1] := word;
    end;
end;

function ReadInput(): IntegerArray;
var
    inputFilename: String;
    inputFile: TextFile;
    line: String;
    splitLine: StringArray;
    i: Integer;
begin
    inputFilename := ParamStr(1);
    WriteLn('Will read data from: ', inputFilename);
    Assign(inputFile, inputFilename);
    Reset(inputFile);

    // All the numbers are CSV on the first line
    // Make sure the ANSI string compiler flag is on, or will trunc at 255!
    ReadLn(inputFile, line);
    Close(inputFile);

    splitLine := SplitDelimited(line);
    SetLength(result, Length(splitLine));

    for i := 0 to Length(splitLine)-1 do
        result[i] := StrToInt(splitLine[i]);
end;

procedure PrintCohorts(var cohorts: Cohorts);
var
    i: Integer;
begin
    for i := 0 to 7 do
    begin
        Write(cohorts[i], ', ');
    end;
    WriteLn(cohorts[8]);
end;

function Solve(population: IntegerArray; numDays: Integer): Int64;
var
    c: Cohorts;
    i, cycle: Integer;
    temp: Cohorts;
begin
    // Zero the array
    for i := 0 to 8 do
        c[i] := 0;

    // Bin the fish
    for i := 0 to Length(population)-1 do
        inc(c[population[i]]);

    //PrintCohorts(c);

    for cycle := 1 to numDays do
    begin
        for i := 0 to 8 do
            temp[i] := 0; // initialize

        for i := 0 to 8 do
        begin
            if i = 0 then
            begin
                temp[6] := c[i];
                temp[8] := c[i];
            end
            else
                temp[i-1] := temp[i-1] +  c[i];
        end;
        for i := 0 to 8 do
            c[i] := temp[i];
        //PrintCohorts(c);
    end;

    result := 0;
    for i := 0 to 8 do
        result := result + c[i];
end;

var
    input: IntegerArray;
    numFish: Int64;
begin
    input := ReadInput();
    numFish := Solve(input, 80);
    WriteLn('The number of fish after 80 days is: ', numFish);
    numFish := Solve(input, 256);
    WriteLn('The number of fish after 256 days is: ', numFish);
end.
