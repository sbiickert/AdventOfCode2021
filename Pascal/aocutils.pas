// Utility module for Advent of Code
unit aocutils;

interface

uses SysUtils, StrUtils;


type
	AoCStringArray = array of String;
    AoCGStringArray = array of AoCStringArray;
    AoCCoord2D = record
        x: Integer;
        y: Integer
    end;
    AoCCoord2DArray = array of AoCCoord2D;

function ReadInput(inputFilename: String): AoCStringArray;
function ReadGroupedInput(inputFilename: String): AoCGStringArray;
function Coord2DEqual(a: AoCCoord2D; b: AoCCoord2D): Boolean;

implementation

function ReadInput(inputFilename: String): AoCStringArray;
var
    gString: AoCGStringArray;
begin
    gString := ReadGroupedInput(inputFilename);
    result := gString[0];
end;

function ReadGroupedInput(inputFilename: String): AoCGStringArray;
var
    inputFile: TextFile;
    line: String;
    group: AoCStringArray;
begin
    WriteLn('Will read data from: ', inputFilename);
    Assign(inputFile, inputFilename);
    Reset(inputFile);
    SetLength(result, 0);
    SetLength(group, 0);

    // Make sure the ANSI string compiler flag is on, or will trunc at 255!
    while not Eof(inputFile) do
    begin
        ReadLn(inputFile, line);
        if Length(line) = 0 then
        begin
            SetLength(result, Length(result)+1);
            result[Length(result)-1] := group;
            SetLength(group, 0);
            continue;
        end;
        SetLength(group, Length(group)+1);
        group[Length(group)-1] := line;
    end;

    if Length(group) > 0 then
    begin
        SetLength(result, Length(result)+1);
        result[Length(result)-1] := group;
    end;

    Close(inputFile);
end;

function Coord2DEqual(a: AoCCoord2D; b: AoCCoord2D): Boolean;
begin
    if (a.x = b.x) and (a.y = b.y) then
        result := true
    else
        result := false;
end;

end.
