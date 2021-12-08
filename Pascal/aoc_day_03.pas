program AoCDay03;

uses
    SysUtils, StrUtils, Math;

type
    StringArray = array of String;

const
    ELIMINATED = 2;

var
    bitCount : Integer; // 5 for test, 12 for challenge
    lineCount: Integer; // 12 for test, 1000 for challenge
    report: array [1..1000, 1..12] of Integer;

function GetBit(line: String; pos: Integer):Integer;
var
    s: String;
begin
    s := line[pos];
    result := StrToInt(s);
end;

procedure PrintReport();
var
    line: Integer;
    bit: Integer;
begin
    for line := 1 to lineCount do
    begin
        for bit := 1 to bitCount-1 do
            Write(report[line,bit]);
        WriteLn(report[line,bitCount]);
    end;
end;

procedure ReadInput();
var
    inputFilename: String;
    inputFile: TextFile;
    input: StringArray;
    line: String;
    i: Integer;
    j: Integer;
    bit: Integer;
begin
    inputFilename := ParamStr(1);
    writeln('Will read data from: ', inputFilename);
    Assign(inputFile, inputFileName);

    Reset(inputFile);
    lineCount := 0;

    while not Eof(inputFile) do
    begin
        ReadLn(inputFile, line);
        if Length(line) > 0 then
        begin
            inc(lineCount);
            SetLength(input, lineCount);
            input[lineCount - 1] := line;
        end;
    end;
    Close(inputFile);

    bitCount := Length(input[0]);
    //WriteLn('bit count is ', bitCount);
    //WriteLn('line count is ', lineCount);

    for i := 1 to lineCount do
        for j := 1 to bitCount do
        begin
            bit := GetBit(input[i-1], j);
            report[i,j] := bit;
        end;
    WriteLn('Successfully read report.');
    //PrintReport();
end;

function CountValues(value: Integer; pos: Integer): Integer;
var
    line: Integer;
begin
    result := 0;
    for line := 1 to lineCount do
    begin
        if (report[line,pos] = value) then
        begin
            inc(result);
            if (value <> ELIMINATED) and (report[line,1] = ELIMINATED) then
                dec(result);
        end;
    end;
end;

function CalcGERate(valIfOneCommon: Integer; valIfZeroCommon: Integer): Integer;
var
    bit: Integer;
    bits: array [1..12] of Integer;
begin
    result := 0;
    for bit := 1 to bitCount do
    begin
        if CountValues(1, bit) > (lineCount / 2) then
            bits[bit] := valIfOneCommon
        else
            bits[bit] := valIfZeroCommon;
        // bits is big-endian
        result := result + (bits[bit] * (2**(bitCount-bit)))
    end;
end;

procedure EliminateLines(value: Integer; pos: Integer);
var
    i: Integer;
begin
    for i := 1 to lineCount do
    begin
        if report[i,pos] = value then
            report[i,1] := ELIMINATED;
    end;
end;

function GetLastRowStandingValue(): Integer;
var
    bit: Integer;
    line: Integer;
begin
    bit := 1;
    result := 0;
    for line := 1 to lineCount do
    begin
        if report[line,1] <> ELIMINATED then
        begin
            for bit := 1 to bitCount do
            begin
                result := result + (report[line,bit] * (2**(bitCount-bit)))
            end;
            break;
        end;
    end;
end;

function CalcOxygen(): Integer;
var
    bit: Integer;
    oneCount: Integer;
    zeroCount: Integer;
    elimCount: Integer;
begin
    // We will mark lines that are eliminated with a 2 in the first place
    bit := 1;
    elimCount := CountValues(ELIMINATED, 1);
    while (elimCount < (lineCount - 1)) and (bit <= bitCount) do
    begin
        zeroCount := CountValues(0, bit);
        oneCount := CountValues(1, bit);
        if oneCount >= zeroCount then
            EliminateLines(0, bit)
        else
            EliminateLines(1, bit);
        inc(bit);
        elimCount := CountValues(ELIMINATED, 1);
    end;
    // Should be one non-eliminated line
    result := GetLastRowStandingValue()
end;

function CalcCO2(): Integer;
var
    bit: Integer;
    oneCount: Integer;
    zeroCount: Integer;
    elimCount: Integer;
begin
    // We will mark lines that are eliminated with a 2 in the first place
    bit := 1;
    elimCount := CountValues(ELIMINATED, 1);
    while (elimCount < (lineCount - 1)) and (bit <= bitCount) do
    begin
        zeroCount := CountValues(0, bit);
        oneCount := CountValues(1, bit);
        if zeroCount > oneCount then
            EliminateLines(0, bit)
        else
            EliminateLines(1, bit);
        inc(bit);
        elimCount := CountValues(ELIMINATED, 1);
    end;
    // Should be one non-eliminated line
    result := GetLastRowStandingValue();
end;

procedure SolvePartOne();
var
    gamma: Integer;
    epsilon: Integer;
begin
    gamma := CalcGERate(1, 0);
    epsilon := CalcGERate(0, 1);
    WriteLn('Part One power consumption is ', gamma, '*', epsilon, '=', gamma*epsilon);
end;

procedure SolvePartTwo();
var
    oxygen: Integer;
    co2:    Integer;
begin
    oxygen := CalcOxygen();
    ReadInput(); // We mucked up report in CalcOxygen, reread

    co2 := CalcCO2();
    WriteLn('Part Two life support is ', oxygen, '*', co2, '=', oxygen*co2);

end;

begin
    ReadInput();
    SolvePartOne();
    SolvePartTwo();
end.
