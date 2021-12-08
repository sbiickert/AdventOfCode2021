program AoCDay04;

uses
    SysUtils, StrUtils;

type
    Board = array [0..4, 0..4] of String;
    StringArray = array of String;

const
    SIZE = 5;
var
    boards: array of Board;
    called: StringArray;

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

procedure ReadInput();
var
    inputFilename: String;
    inputFile: TextFile;
    line: String;
    space: SysUtils.TSysCharSet;
    i, j, count: Integer;
    row: StringArray;
    b: Board;
begin
    inputFilename := ParamStr(1);
    writeln('Will read data from: ', inputFilename);
    Assign(inputFile, inputFileName);
    Reset(inputFile);

    // First line is called numbers
    ReadLn(inputFile, line);
    called := SplitDelimited(line);

    // Boards
    space := [' '];
    count := 0;
    while not Eof(inputFile) do
    begin
        // Skip empty line
        ReadLn(inputFile, line);
        inc(count);
        SetLength(boards, count);
        for i := 0 to SIZE - 1 do
        begin
            ReadLn(inputFile, line);
            row := SplitDelimited(line);
            for j := 0 to SIZE - 1 do
                b[i,j] := row[j];
        end;
        boards[count-1] := b;
    end;
    Close(inputFile);
end;

function GetNumberInBoard(board: Board; row: Integer; col: Integer): Integer;
begin
    result := 0;
    if (Length(board[row,col]) > 0) then
        result := StrToInt(board[row,col]);
end;

function ScoreBoard(board: Board): Integer;
var
    i,j: Integer;
    num: Integer;
begin
    result := 0;
    for i := 0 to SIZE-1 do
        for j := 0 to SIZE-1 do
            result := result + GetNumberInBoard(board, i, j);
end;

procedure PrintBoard(board: Board);
var
    i: Integer;
begin
    for i := 0 to SIZE-1 do
    begin
        Write(board[i,0]:4);
        Write(board[i,1]:4);
        Write(board[i,2]:4);
        Write(board[i,3]:4);
        WriteLn(board[i,4]:4);
    end;
end;

procedure MarkBoard(var board: Board; value: String);
var
    i,j: Integer;
begin
    for i := 0 to SIZE-1 do
        for j := 0 to SIZE-1 do
            if board[i,j] = value then
            begin
                //WriteLn('Found ', value, ' in the board.');
                board[i,j] := '';
                exit;
            end;
end;

function IsWinner(board: Board): Boolean;
var
    i,j: Integer;
    empty: Boolean;
begin
    result := false;
    // Search rows
    for i := 0 to SIZE-1 do
    begin
        empty := true; // Set false if we find a number
        for j := 0 to SIZE-1 do
            if Length(board[i,j]) > 0 then
                empty := false;
        if empty = true then
        begin
            result := true;
            exit;
        end;
    end;
    // Search cols
    for j := 0 to SIZE-1 do
    begin
        empty := true; // Set false if we find a number
        for i := 0 to SIZE-1 do
            if Length(board[i,j]) > 0 then
                empty := false;
        if empty = true then
        begin
            result := true;
            exit;
        end;
    end;
end;

procedure SolvePartOne();
var
    numIndex, boardIndex: Integer;
    score, num: Integer;
begin
    for numIndex := 0 to Length(called) - 1 do
    begin
        for boardIndex := 0 to Length(boards) - 1 do
        begin
            MarkBoard(boards[boardIndex], called[numIndex]);
            if IsWinner(boards[boardIndex]) then
            begin
                PrintBoard(boards[boardIndex]);
                num := StrToInt(called[numIndex]);
                score := ScoreBoard(boards[boardIndex]);
                WriteLn('Part One: board ', boardIndex+1, ' wins. ', score, ' * ', num, ' = ', score*num);
                exit;
            end;
        end;
    end;
    WriteLn('Part One fail.');
end;

procedure SolvePartTwo();
var
    numIndex, boardIndex: Integer;
    score, num: Integer;
    losingIndex: Integer;
    losingBoard: Board;
    winnerCount: Integer;
begin
    for numIndex := 0 to Length(called) - 1 do
    begin
        winnerCount := 0;
        for boardIndex := 0 to Length(boards) - 1 do
        begin
            MarkBoard(boards[boardIndex], called[numIndex]);
            if IsWinner(boards[boardIndex]) then
                inc(winnerCount);
        end;
        if winnerCount = Length(boards)-1 then
            for boardIndex := 0 to Length(boards) - 1 do
                if IsWinner(boards[boardIndex]) = false then
                    losingIndex := boardIndex;
        if winnerCount = Length(boards) then
        begin
            losingBoard := boards[losingIndex];
            PrintBoard(losingBoard);
            num := StrToInt(called[numIndex]);
            score := ScoreBoard(losingBoard);
            WriteLn('Part Two: board ', losingIndex+1, ' loses. ', score, ' * ', num, ' = ', score*num);
            exit;
        end;
    end;
    WriteLn('Part Two fail.');
end;

begin
    ReadInput();
    SolvePartOne();
    ReadInput();
    SolvePartTwo();
end.
