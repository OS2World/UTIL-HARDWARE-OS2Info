{���������������������������������������������������������}
{�                                                       �}
{�      Virtual Pascal v2.0                              �}
{�      Execute child process and redirect output        �}
{�      ��������������������������������������������������}
{�      Copyright (C) 1996-2000 vpascal.com              �}
{�                                                       �}
{���������������������������������������������������������}

// This unit is provided on an as-is basis and has been made
// available due to popular demand.

// It implements a class, which can be used to execute a
// program that writes output to StdOut or StdErr.  This
// output is catched and is returned to the calling program,
// which can use it in any way it sees fit.

Unit Os2Exec;

{$Delphi+,X+,T-}

interface

uses
  Os2Base, VPUtils, SysUtils, Classes;

const
  MsgBufSize = 512;

type
  MsgCharResult = (mcrTimeOut, mcrReady, mcrEOF);

  tRedirExec = class( tObject )
  private
    // Variables used internally by the class
    fReadHandle  : Longint;
    fWriteHandle : Longint;
    fMsgBufPtr   : LongInt;
    fBytesRead   : LongInt;
    fReadSem     : HEv;
    fMsgReady    : Boolean;
    fMsgBuffer   : array [0..MsgBufSize-1] of Char;
    fMessageChar : Char;

    // Fields published as properties
    fMessageLine : String;
    fTerminated  : Boolean;
    fReturnCode  : Longint;
    fOnTerminate : TNotifyEvent;
    fOnCharReady : TNotifyEvent;
    fOnLineReady : TNotifyEvent;
  protected
    function ReadMessageChar: MsgCharResult;
    function GetMessageReady: Boolean;
    function GetMessage: String;
    property MessageChar: Char read fMessageChar write fMessageChar;
  published
    constructor create;
    destructor Destroy; override;
    function Execute(const S: String; CmdLine: PChar; Env: PChar): Boolean;

    property Terminated: Boolean read fTerminated;
    property MessageReady: Boolean read GetMessageReady;
    property ReturnCode: Longint read fReturnCode;
    property Message: String read GetMessage;

    property OnTerminate: TNotifyEvent read fOnTerminate write fOnTerminate;
    property OnCharReady: TNotifyEvent read fOnCharReady write fOnCharReady;
    property OnLineReady: TNotifyEvent read fOnLineReady write fOnLineReady;
  end;

implementation

const
  StdOut = 1;     // Standard output file handle
  StdErr = 2;     // Standard error file handle

constructor tRedirExec.create;
begin
  inherited create;

  DosCreateEventSem(nil, fReadSem, dc_Sem_Shared, False); // Create event semaphore
  fTerminated := True;
  fMsgReady := False;
end;

function tRedirExec.Execute(const S: String; CmdLine: PChar; Env: PChar): Boolean;
var
  NewOut    : Longint;                   // File handles
  OldOut    : Longint;
  OldErr    : Longint;
  NewErr    : Longint;
  ExecErr   : Longint;
  PostCount : Longint;
  PipeName  : String;                    // Name of pipe used to communicate
  Action    : Longint;                   // Action taken by DosOpen
  ExecRes   : ResultCodes;
  Os2Args   : PChar;
  Args      : array [0..1024*2] of Char;
  PrgName   : array [0..259] of Char;
  FailedObj : array [0..259] of Char;

begin
  Result := False;
  If not fTerminated then               // Process already running - exit
    Exit;

  // Create Named Pipe with a unique name, so several instances of the
  // program can run without interfering by embedding a timer count into
  // the pipe name.
  // The server (read) handle of the pipe is used by the program, while the
  // client (write) handle is redirected to be the STDOUT handle for the
  // program to execute.
  PipeName := Format( '\PIPE\VPX%.4x%.8x'#0, [ GetForegroundProcessID, GetTimeMSec ] );
  if DosCreateNPipe(@PipeName[1], fReadHandle, np_Access_InBound,
    np_NoWait + 1, 0, 4*1024, 0) <> 0 then
    exit;                               // Fail if pipe creation fails

  DosConnectNPipe(fReadHandle);         // Connect to pipe
  DosOpen(@PipeName[1], fWriteHandle, Action, 0, file_Normal, file_Open,
    open_access_WriteOnly+open_share_DenyNone, nil);

  DosResetEventSem(fReadSem, PostCount);// Reset read event semaphore
  DosSetNPipeSem(fReadHandle, HSem(fReadSem), 0);         // Associate with pipe

  OldOut := $FFFFFFFF;                  // Save original StdOut to OldOut
  DosDupHandle(StdOut,OldOut);
  NewOut := StdOut;                     // Redirect StdOut to Write pipe handle
  DosDupHandle(fWriteHandle,NewOut);

  OldErr := $FFFFFFFF;                  // Save original StdErr to OldErr
  DosDupHandle(StdErr,OldErr);
  NewErr := StdErr;                     // Redirect StdErr to Write pipe handle
  DosDupHandle(fWriteHandle,NewErr);

  DosClose(fWriteHandle);               // Close write pipe end to sense EOF on read

  StrPCopy(PrgName,S);                  // Set up DosExecPgm parameters
  Os2Args := Args;
  // Work around OS/2 bug: Argument to ExecPgm must not cross 64K boundary
  if ((Longint(Os2Args) + 1024) and $FFFF) < 1024 then
    Inc(Os2Args, 1024);
  StrCat(StrCat(StrCopy(Os2Args, PrgName), ' '), CmdLine);
  Os2Args[StrLen(Os2Args)+1] := #0;     { #0#0 at the end }
  Os2Args[Length(S)] := #0;             { #0 after program name }
  ExecErr := DosExecPgm(FailedObj, SizeOf(FailedObj), exec_AsyncResult, Os2Args, Env, ExecRes, PrgName);

  // Restore Handles before returning
  DosDupHandle(OldOut,NewOut);          // Restore StdOut to original meaning
  DosClose(OldOut);                     // Close duplicate of StdOut
  DosDupHandle(OldErr,NewErr);          // Restore StdErr to original meaning
  DosClose(OldErr);                     // Close duplicate of StdErr

  if ExecErr <> 0 then                  // If execution failed, exit
    exit;

  fMsgBufPtr  := 0;                     // Reset state variables
  fBytesRead   := 0;
  fTerminated  := False;
  fMessageLine := '';
  Result       := True;
end;

{ Returns next message character if available }

function tRedirExec.ReadMessageChar: MsgCharResult;
var
  PostCount : Longint;
  PipeState : Longint;
  RCWait    : Longint;
  Avail     : AvailData;
begin
  if fMsgBufPtr = fBytesRead then
    begin
      fMsgBufPtr := 0;
      fBytesRead := 0;
      RCWait := DosWaitEventSem(fReadSem, 1);   // Wait 1 msec for posting sem
      DosPeekNPipe(fReadHandle, fMsgBuffer, 1, fBytesRead, Avail, PipeState);
      if fBytesRead = 0 then                   // No data available...
        begin
          if PipeState = np_State_Closing then // If exiting, return EOF
            begin
              ReadMessageChar := mcrEOF;
              Exit;
            end;
          if RCWait <> 0 then                  // If error, return timeout
            begin
              ReadMessageChar := mcrTimeOut;
              Exit;
            end;
        end;
      DosResetEventSem(fReadSem, PostCount);    // Reset semaphore
      DosRead( fReadHandle, fMsgBuffer,        // Read data from pipe
               SizeOf(fMsgBuffer), fBytesRead);
      if fBytesRead = 0 then                   // If no data was read...
        begin
          ReadMessageChar := mcrTimeOut;       // Return timout
          Exit;
        end;
    end;
  ReadMessageChar := mcrReady;                 // Character ready
  fMessageChar := fMsgBuffer[fMsgBufPtr];      // Fill buffer
  Inc(fMsgBufPtr);
end;

{ Checks state of pipe, and returns True if a full line is available }

Function tRedirExec.GetMessageReady : Boolean;
var
  Len     : Longint;
  MsgInx  : Longint;
  RetPID  : Longint;
  ExecRes : ResultCodes;

begin
  If fMsgReady then
    begin
      Result := True;
      exit;
    end;

  Result := False;
  Len := Length(fMessageLine);

  for MsgInx := 0 to MsgBufSize-1 do
    case ReadMessageChar of
      mcrEOF:                                  // EOF reached
        begin
          DosClose(fReadHandle);               // Close handle
          DosWaitChild(dcwa_Process, dcww_Wait, ExecRes, RetPID, 0);
          fReturnCode := ExecRes.codeResult;   // Save return code
          fTerminated := True;
          If Assigned( fOnTerminate ) then     // Execute OnTerminate method
            OnTerminate( Self );
          Exit;
        end;

      mcrReady:                                // Character is received
        begin
          if Assigned( fOnCharReady ) then     // Execute OnCharReady method
            OnCharReady( Self );

          if (fMessageChar = #10) and (fMessageLine <> '') then
            begin
              Result := True;                  // End-of-line
              fMsgReady := True;
              if Assigned( fOnLineReady ) then // Execute OnLineReady method
                OnLineReady( Self );
              Exit;
            end;

          if MessageChar >= ' ' then           // Filter printable chars
            begin
              Inc(Len);                        // Append char to string
              SetLength( fMessageLine, Len );
              fMessageLine[Len] := MessageChar;
            end;
        end;

      mcrTimeOut: Exit;                        // Timeout: Exit
    end;

end;

{ Returns a message, if one is ready.  Otherwise, the empty string is returned }

Function tRedirExec.GetMessage : string;
begin
  if MessageReady then                 // If a message is ready
    begin
      Result := fMessageLine;          // Return message
      fMessageLine := '';
      fMsgReady := False;
    end
  else
    Result := '';                      // else return blank line
end;

destructor tRedirExec.Destroy;
begin
  DosDisConnectNPipe(fReadHandle);     // Disconnect from pipe
  DosCloseEventSem(fReadSem);          // Close event semaphore

  inherited destroy;
end;

end.

