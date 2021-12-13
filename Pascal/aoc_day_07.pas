program AoCDay07;

uses SysUtils, StrUtils, AoCUtils;

type
    IntegerArray = array of Integer;

const
    MAX = 2000;

var
    input: AoCStringArray;
    positions: IntegerArray;
    nlcCache: array [1..MAX] of Integer;

function GetMinPos(): Integer;
var
    i: Integer;
begin
    result := MAX;
    for i := 0 to Length(positions)-1 do
        if positions[i] < result then
            result := positions[i];
end;

function GetMaxPos(): Integer;
var
    i: Integer;
begin
    result := -1;
    for i := 0 to Length(positions)-1 do
        if positions[i] > result then
            result := positions[i];
end;

procedure InitNLCache();
var
    i: Integer;
begin
    for i := 1 to MAX do
        nlcCache[i] := 0;
end;

function GetNonLinearFuelCost(distance: Integer):Integer;
var
    i: Integer;
begin
    if distance = 0 then
        result := 0
    else if nlcCache[distance] > 0 then
        result := nlcCache[distance]
    else
    begin
        result := 0;
        for i := 1 to distance do
            result := result + i;
        nlcCache[distance] := result;
    end;
end;

procedure SolvePartOne();
var
    costAtPosition: Integer;
    minCost: Integer;
    minCostPosition: Integer;
    testPosition: Integer;
    i: Integer;
    crabCost: Integer;
begin
    minCost := MaxInt;
    for testPosition := GetMinPos() to GetMaxPos() do
    begin
        costAtPosition := 0;
        for i := 0 to Length(positions)-1 do
        begin
            crabCost := Abs(positions[i] - testPosition); // linear
            costAtPosition := costAtPosition + crabCost;
        end;
        if costAtPosition < minCost then
        begin
            minCost := costAtPosition;
            minCostPosition := testPosition;
        end;
    end;
    WriteLn('Part One min cost is ', minCost, ' at position ', minCostPosition);
end;

procedure SolvePartTwo();
var
    costAtPosition: Integer;
    minCost: Integer;
    minCostPosition: Integer;
    testPosition: Integer;
    i: Integer;
    crabCost: Integer;
begin
    minCost := MaxInt;
    InitNLCache;
    for testPosition := GetMinPos() to GetMaxPos() do
    begin
        costAtPosition := 0;
        for i := 0 to Length(positions)-1 do
        begin
            crabCost := GetNonLinearFuelCost(Abs(positions[i] - testPosition)); // non-linear
            costAtPosition := costAtPosition + crabCost;
        end;
        if costAtPosition < minCost then
        begin
            minCost := costAtPosition;
            minCostPosition := testPosition;
        end;
    end;
    WriteLn('Part Two min cost is ', minCost, ' at position ', minCostPosition);
end;


var
    i: Integer;
begin
    input := ReadInput(ParamStr(1));
    //WriteLn(input[0]);
    input := SplitDelimited(input[0]);
    SetLength(positions, Length(input));
    for i := 0 to Length(input)-1 do
        positions[i] := StrToInt(input[i]);
    SolvePartOne();
    SolvePartTwo();
end.
