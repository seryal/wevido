program youtube_download;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,              {$ENDIF}
  Classes,
  SysUtils, videodownloader,
  CustApp,
  crt;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    procedure RecvHandler(AFilePath: string; AProgress, ASize: longword);
  protected
    FDownloader: TYouTubeVideo;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TMyApplication }

  procedure TMyApplication.RecvHandler(AFilePath: string; AProgress, ASize: longword);
  var
    x, y: DWord;
  begin
    x := WhereX32;
    y := WhereY32;
    WriteLn(AProgress, '/', ASize, '              ');
    GotoXY32(x, y);

  end;

  procedure TMyApplication.DoRun;
  var
    ErrorMsg: string;
    url: string;
    filename: TFileName;
    info: TVideoInfo;
    i: integer;
    downloadurl: TDownloadUrl;
  begin
    DefaultSystemCodePage := CP_UTF8;
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    url := ParamStr(1);
    if ParamStr(2) <> '' then
      filename := ParamStr(2)
    else
      filename := 'downloaded.mp4';

    writeln(url);
    FDownloader := TYouTubeVideo.Create;
    FDownloader.OnDataReceived := @RecvHandler;
    try
      FDownloader.SetWebURL(url);
      info := FDownloader.GetInfo;
      writeln('Title - ', info.VideoName);
      writeln('Author - ', info.Author);
      writeln('Length - ', info.Length, ' seconds');
      for downloadurl in info.DownloadUrls do
      begin
        TextColor(Yellow);
        Write(' - ', downloadurl.mimeType);
        TextColor(LightGray);
        writeln(' "', downloadurl.Link, '"');

      end;
      writeln('----');
      FDownloader.DownLoad(filename);
    finally
      FreeAndNil(FDownloader);
    end;
    WriteLn('Download complete');

    { add your program here }

    // stop program loop
    // readln;
    Terminate;
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.
