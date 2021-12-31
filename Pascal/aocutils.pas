// Utility module for Advent of Code
unit aocutils;

interface

uses SysUtils, StrUtils;


type
	AoCStringArray = array of String;


function SplitDelimited(text: String): AoCStringArray;
function ReadInput(inputFilename: String): AoCStringArray;

implementation

function SplitDelimited(text: String): AoCStringArray;
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

function ReadInput(inputFilename: String): AoCStringArray;
var
    inputFile: TextFile;
    line: String;
    count: Integer;
begin
    WriteLn('Will read data from: ', inputFilename);
    Assign(inputFile, inputFilename);
    Reset(inputFile);

    // Make sure the ANSI string compiler flag is on, or will trunc at 255!
    count := 0;
    while not Eof(inputFile) do
    begin
        ReadLn(inputFile, line);
        inc(count);
        SetLength(result, count);
        result[count-1] := line;
    end;
    Close(inputFile);
end;

end.
